(ns benjamin-schwerdtner.clj-push3-play.hdc.resonator
  (:require
   [clojure.math.combinatorics :refer [cartesian-product]]
   [libpython-clj2.python :refer [py. py..] :as py]
   [libpython-clj2.require :refer [require-python]]

   [benjamin-schwerdtner.clj-push3-play.hdc.fhrr :as hd]
   [benjamin-schwerdtner.clj-push3-play.hdc.data :as hdd]))

(require-python '[torch :as torch])
(require-python '[torch.nn.functional :as F])

;; The problem
;;
;; `factorize`, given a hd find the contributing hdvs:

;;
;; 1. Given a hd `x`.
;; 2. Given a list of codebooks, book-0, book-1, book-2, ...
;; 3. Find a hypervector for each book that together produce x, such that
;;
;;
;;           a-0 ∈ book-0, a-1 ∈ book-1, ...
;;
;; and       x = bind(a-0, a-1, ... )
;;
;;
;;
;; The reference factorizer implementation
(defn exhaustive-search-factorize
  "Returns a seq of hdvs from `books`,

  This does an exhaustive search over all possible hdv combinations.

  "
  [x books]
  ;; search space is the cartesian product of the hdvs in books
  (->> (apply cartesian-product books)
       (some (fn [hdvs]
               (when (hd/similar? x (hd/bind hdvs)) hdvs)))))

(comment
  ;; "Elapsed time: 67.839989 msecs"
  (time (let
         [books [(hd/seed 10)
                 (hd/seed 10)
                 (hd/seed 10)]
          [a b c] (mapv (fn [b]
                          (py/get-item b (rand-int 10)))
                        books)
          x (hd/bind [a b c])

          [af bf cf]
          (into [] (exhaustive-search-factorize x books))]

          (hd/similarity
           (torch/stack [a b c])
           (torch/stack [af bf cf]))))

  ;; tensor([[ 1.0000, -0.0020, -0.0066],
  ;;         [-0.0020,  1.0000,  0.0034],
  ;;         [-0.0066,  0.0034,  1.0000]])
  )
;; Resonator networks for factoring distributed representations of data structures
;; Frady, Kent, Olshausen, Sommer 2020
;; Neural Computation 32, 2311-2331 (2020)

;; https://arxiv.org/abs/2007.03748

;; --------------------------------------
;;
;; The resonator factorizer algorithm, can be interpreted as a kind of recurrent neural net
;; 'resonator network'.
;;

;; - uses computing in superposition,
;; - iterates on estimates, in parallel
;; - solutions emerge as stable fixed-points in the network dynamics
;;

;; Assume you have:
;;
;; Eqn (1)    s = xi ⊙ yj ⊙ zk
;;
;; Where xi, yj, zk are drawn from codebooks
;;
;; X = { x1, ..., xD }, etc.
;;
;; I call `D` the codebook resolution.
;; e.g.
;; D = 100

;;
;; The resonator combines superposition and cleanup
;;

;; -----------------------------
;; A FHRR resonator is found at
;;
;; https://arxiv.org/abs/2208.12880v4

;;
;; reference impl
;; -  (not FHRR) https://github.com/spencerkent/resonator-networks/blob/master/resonator_networks/dynamics/rn_numpy.py#L7
;; -  https://codeocean.com/capsule/1211768/tree/v1
;;

;; -----------------------------

(defn norm
  [x]
  (torch/div (torch/norm x)
             (torch/mul (torch/sqrt (torch/tensor (py.. x (size 0))))
                        2)))

(defn norm-abs [x] (torch/div x (torch/abs x)))

(def resonator-defaults
  {:phasor-projection
   ;; set the mag of all vector elements to 1
   (fn [x] (torch/div x (norm x)))
   :non-linearity
   ;; Quote:
   ;; Our experiments show that a combination of a ReLU and polynomial exponent
   ;; performs best
   (fn poly [x {:keys [cleanup-k]}]
     ;; the k paramater controls the amount of superposition of different possible
     ;; solutions, i.e. the sparsity
     ;;
     ;; below 1: weaken the cleanup (k=0 would return the complete superposition)
     ;; above 1: achieve a stricter cleanup
     ;; like argmax, or winner-take-all for high ks.
     (torch/pow (F/relu x) cleanup-k))

   ;; cleanup-k > 1 didn't do much for me.
   :cleanup-k 1
   :noise-level 0.001
   :update-ratio 0.1
   :max-iterations 13
   :similarity-threshold 0.9

   ;; experimental, doesn't do much.

   ;; :excitability-factor 0.01
   ;; :excitability-decay 0.01
   ;; :attenuation-factor 0.002
   ;; :attenuation-decay 0.01
   })

(defn resonator-step-fhrr
  "Returns new estimates for factorizing `target`, performs a resonator step with `books`.

  shapes:

  - target: (d)
  - books: (n, m, d)
  - estimates: (n, d)
  - output: (n, d)

  d: hdv dimension
  n: books/ factors count
  m: book resolution

  Each new estimate is given by the equivalent of this (for x):

  eqn 1:  x-estimate(t + 1) = g(X^T p(X(s ⊙ y-estimate(t)^-1 ⊙ z-estimate(t)^-1))) + η


  ⊙    : bind
  ^-1    : inverse
  s      : the input / target (to be factorized)
  p(x)   : a a non-linearity
  g(x)   : a phasor-projection, setting the mag of the vectors 1
  η      : a random noise to help prevent local optima

  So for each factor,

  - unbind with the 'other factor' estimates, yielding an estimate of the factor.
  - perform a cleanup with the given book, using the outer matrix product of the book.
  - This step is conceptually similar to a Hopfield Net.
  - Normalize outputs with an activation function `g`, prevent run away effects.

  --

  Beyond the update described, we only change the next state by an update-ratio factor γ:

  if eqn 1 is 'update', then

  x(t+1) = γ * update(x) +  (1 - γ) * x

  So with low `update-ratio`, we mostly keep x, just like a residuals net in deep learning;
  Where each layer only contributes an update but does not replace the output.
  "
  [target books estimates
   {:keys [ ;; p(x)
           non-linearity
           ;; η
           noise-level
           ;; g(x)
           phasor-projection
           ;; γ
           update-ratio]
    :as opts}]
  (let [estimates (torch/unsqueeze estimates 0)
        n (py.. estimates (size -2))
        ;; ----------------------
        ;;
        ;; 1. For each factor, find the
        ;; other factors and bind them
        others (-> (reduce (fn [rolled i]
                             (into rolled
                                   (torch/roll estimates i -2)))
                           []
                           (range 1 n))
                   ;; shape: (n,n-1,d)
                   (torch/stack :dim -2)
                   ;; shape: (n,d)
                   (hd/bind))
        ;; -----------------------------
        ;;
        ;; 2.
        ;; e = s ⊙ others^-1
        ;; shape: (n,d)
        new-estimate (hd/bind target (hd/inverse others))
        ;; -----------------------------
        ;;
        ;; 3.
        ;; pattern = XX^T(e)
        ;; Cleanup / autoassociative part. Scale the output
        ;; pattern by it's similarity (activation).
        ;;
        ;; attn = e*X^T activations | similarity | attention
        ;; Like address decoder neuron activations
        ;; shape: (n)
        ;;
        ;; x-real
        attn
        ;;
        (torch/real (torch/einsum "nd,nmd->nm"
                                  new-estimate
                                  (torch/resolve_conj (torch/conj
                                                       books))))
        ;; 4.
        ;; optionally bias
        ;; att (torch/mul attn bias)
        ;;
        ;; Optional non linearity
        ;; p(x)
        ;;
        attn (non-linearity attn opts)
        attn (py.. attn (type torch/cfloat))
        patt-update (torch/einsum "nm,nmd->nd" attn books)
        ;; ----------------------
        ;; 5. update by γ
        pattern (torch/add (torch/mul update-ratio patt-update)
                           (torch/mul (- 1 update-ratio) estimates))
        ;; ----------------------------
        ;;
        ;; 6.
        ;; out = g(pattern)
        outputs (phasor-projection pattern)
        ;; 7.
        ;; + noise
        ;; #1 random noise
        outputs
        (if (nil? noise-level)
          outputs
          (torch/add outputs (torch/mul (torch/randn_like outputs) noise-level)))

        ;;
        ;; #2, draw noise from the superposition of all codebooks?
        ;; outputs
        ;; (if
        ;;     (nil? noise-level)
        ;;     outputs
        ;;     (torch/add outputs
        ;;                (torch/mul (torch/randn_like outputs)
        ;;                           (torch/mul noise-level
        ;;                                      (hd/normalize (hd/superposition books))))))


        ;; #3, calculate an error and add noise proportional to it?

        ;; error
        ;; (torch/sub 1
        ;;            (hd/similarity
        ;;             target
        ;;             (hd/normalize (hd/bind (torch/squeeze outputs)))))
        ;; outputs
        ;; (torch/add
        ;;  outputs
        ;;  (torch/mul (torch/randn_like outputs)
        ;;             (torch/mul noise-level error)))



        ;; #4, calculate an error delta and add noise proportional to it?
        ;; intuition: if we are moving towards a solution, we don't need noise

        ;; this is maybe the slightly better noise.
        ;; but the noise doesn't help that much in general.


        ;; error
        ;; (torch/sub 1 (hd/similarity target (hd/normalize (hd/bind outputs))))
        ;; prev-error
        ;; (torch/sub 1 (hd/similarity target (hd/normalize (hd/bind estimates))))
        ;; error-delta-rectified
        ;; (torch/relu (torch/sub error prev-error))
        ;; _
        ;; (println
        ;;  "err: " (py.. error (item))
        ;;  " prev: " (py.. prev-error (item))
        ;;  " delta: " (py.. error-delta-rectified (item))
        ;;  " fac " (py.. (torch/mul error-delta-rectified 1000) (item))
        ;;  )

        ;; outputs
        ;; (torch/add
        ;;  outputs
        ;;  (torch/mul
        ;;   (torch/mul
        ;;    (torch/randn_like outputs)
        ;;    (hd/superposition books))

        ;;   (torch/mul
        ;;    error
        ;;    ;; (torch/mul noise-level error)
        ;;    (torch/mul error-delta-rectified 1000))))

        ;; --------------------------------------------
        ]
    (torch/squeeze outputs)))



;; Removed duplicate definition - see the first one above with device handling

(defn resonator-fhrr
  "perform a resonator factorization of `x` with `books`.
  returns an outcome info map with the following keys:

  `:factors`: a seq of hdvs, one for each book. binding the together yields something similar to `x`.
  `:finish-reason`: a keyword indicating why the factorization finished.
                    can be `:factorized` or `:max-iterations-reached`.
  `:success?`: true if the factorization was successful, false otherwise.


  `n`: factors count
  `m`: book resolution
  `d`: hdv dimension

  shapes:

  books: (n, m, d)
  x: (d)

  opts:

  `similarity-threshold`: sim as by [[hd/similarity]] for which estimates are accepted.
                          depends on how dirty your input is.

  `max-iterations`: number of iterations before giving up.

  "
  ([x books] (resonator-fhrr x books resonator-defaults))
  ([x books
    {:keys
     [max-iterations similarity-threshold attenuation-factor attenuation-decay
      excitability-decay
      excitability-factor
      ]
     :as opts}
    ]
   (let [
         initial-estimates (hd/superposition books)
         finish-info
         (fn [{:keys [n-iteration estimates]}]
           (cond (hd/similar? x
                              (hd/normalize (hd/bind estimates))
                              similarity-threshold)
                 (let [factor-idxs (mapv (fn [x book]
                                           (hd/cleanup-idx x
                                                           book))
                                         estimates
                                         books)]
                   {:factor-idxs factor-idxs
                    :factors (into []
                                   (map (fn [idx b]
                                          (py/get-item
                                           b
                                           (py.. idx item)))
                                        factor-idxs
                                        books))
                    :finish-reason :factorized
                    :success? true})
                 (< max-iterations n-iteration)
                 {:finish-reason :max-iterations-reached
                  :success? false}
                 :else nil))]
     (loop [{:keys [n-iteration estimates attenuation excitability]
             :as state}
            {:attenuation (torch/zeros_like initial-estimates)
             :estimates initial-estimates
             :excitability (torch/ones_like initial-estimates)
             :n-iteration 0}]
       ;; ------------------------
       (if-let [info (finish-info state)]
         (merge state info)
         (let [estimates (resonator-step-fhrr
                          x
                          books
                          estimates
                          opts
                          )

               ;; estimates
               ;; (torch/mul estimates excitability)
               ;;  ;; E = E - attenuation

               ;; estimates
               ;; (hd/superposition
               ;;  estimates
               ;;  (hd/negative attenuation))


               ;; ;; attenuation(t+1) = attenuation(t) * (1 -
               ;; attenuation-decay-factor) + estimates(t+1) *
               ;; attenuation-factor

               ;; attenuation (torch/add
               ;;              (torch/mul attenuation
               ;;                         (- 1 attenuation-decay))
               ;;              (torch/mul estimates attenuation-factor))


               ;; ;; exc(t+1) = exc(t) * (1 - exct-decay) +
               ;; exct(t+1) * excitability-grow-factor

               ;; excitability
               ;; (torch/add
               ;;  (torch/mul excitability (- 1 excitability-decay))
               ;;  (torch/mul estimates excitability-factor))
               ]

           #_(println
            "atten: " (norm attenuation)
            "activ: " (norm estimates)
            "excit: " (norm excitability))

           (recur {:attenuation attenuation
                   :estimates estimates
                   :excitability excitability
                   :n-iteration (inc n-iteration)})))))))




(def factorize resonator-fhrr)



(comment












  (torch/sum (hd/magnitude (hd/seed)))
  (torch/sum (hd/magnitude (hd/superposition (hd/seed 2))))

  (def a (hd/seed))

  (hd/similarity a (torch/sub a 1))

  (hd/similarity a (torch/mul (- 1 0.1) a))
  (hd/similarity a (torch/mul (- 1 0.9) a))

  (torch/mul (torch/real (torch/mul a (torch/conj a))) 0.5)




  (hd/similarity a (torch/mul (- 1 0.9) a))
  (torch/mul (- 1 0.9) a)


  (hd/similarity a (torch/mul a (torch/sub 1 a)))



  ;; nothing active:
  (hd/similarity a (torch/mul 0 a))

  ;; everything active, no attenuation:
  (hd/similarity a (torch/mul 1 a))



  (def a1 (hd/normalize (hd/superposition [a (hd/seed) (hd/seed) (hd/seed)])))

  (hd/similarity a (hd/inverse a))
  (hd/similarity a (hd/inverse a1))
  (hd/similarity a (torch/mul a (hd/inverse a1)))


  (time
   (doall
    (for [book-size [100
                     ;; 20 30 50
                     ]]
      (for [update-r [0.1]]
        {:book-zise book-size
         :results
         (doall (for [n (range 1)]
                  (do (def update-ratio update-r)
                      (def books
                        (hd/random [3 book-size
                                    (:fhrr/dimensions hd/*opts*)]))
                      (def origbooks books)
                      (py.. books (size))
                      (def a (py/get-item books [0 -1]))
                      (def b (py/get-item books [1 -1]))
                      (def c (py/get-item books [2 -1]))
                      (def target (hd/bind [a b c]))
                      (select-keys (resonator-fhrr target
                                                   books
                                                   {:max-iterations
                                                    20})
                                   [:n-iteration :success?]))))
         :update-ratio update-r}))))



  (time
   (doall
    (for [book-size [10]]
      (for [update-r [0.1]]
        {:book-zise book-size
         :results (doall (for [n (range 1)]
                           (do (def books
                                 (hd/random [3 book-size
                                             (:fhrr/dimensions
                                              hd/*opts*)]))
                               (def a (py/get-item books [0 -1]))
                               (def b (py/get-item books [1 -1]))
                               (def c (py/get-item books [2 -1]))
                               (def target (hd/bind [a b c]))
                               (select-keys
                                (resonator-fhrr target
                                                books
                                                {:max-iterations 20})
                                [:n-iteration :success?]))))
         :update-ratio update-r}))))















  (time
   (doall
    (for [noise-v [0 0.005 0.1 0.2 0.3]
          ;; (range 0 0.005 0.5)
          ]
      (for [n (range 3)]
        (time (do (def noise-level noise-v)
                  (def update-ratio 0.01)
                  (def book-size 50)
                  (def books
                    (hd/random [3 book-size
                                (:fhrr/dimensions hd/*opts*)]))
                  (def origbooks books)
                  (py.. books (size))
                  (def a (py/get-item books [0 -1]))
                  (def b (py/get-item books [1 -1]))
                  (def c (py/get-item books [2 -1]))
                  (def target (hd/bind [a b c]))
                  (select-keys
                   (resonator-fhrr target books {:max-iterations 20})
                   [:n-iteration :success?])))))))


  #_(
     ({:n-iteration 21 :success? false}
      {:n-iteration 9 :success? true}
      {:n-iteration 7 :success? true})


     ({:n-iteration 4 :success? true}
      {:n-iteration 21 :success? false}
      {:n-iteration 8 :success? true})

     ({:n-iteration 11 :success? true}
      {:n-iteration 8 :success? true}
      {:n-iteration 9 :success? true})

     ({:n-iteration 8 :success? true}
      {:n-iteration 16 :success? true}
      {:n-iteration 18 :success? true})


     ({:n-iteration 21 :success? false}
      {:n-iteration 21 :success? false}
      {:n-iteration 21 :success? false}))


  (time
   (doall
    (for [noise-v [0.15 0.2]]
      (for [n (range 20)]
        (time
         (do (def noise-level noise-v)
             (def update-ratio 0.01)
             (def book-size 50)
             (def books
               (hd/random [3 book-size
                           (:fhrr/dimensions hd/*opts*)]))
             (def origbooks books)
             (py.. books (size))
             (def a (py/get-item books [0 -1]))
             (def b (py/get-item books [1 -1]))
             (def c (py/get-item books [2 -1]))
             (def target (hd/bind [a b c]))
             (select-keys
              (resonator-fhrr target books {:max-iterations 20})
              [:n-iteration :success?])))))))


  (py.. books -device)

  (torch/allclose
   (py.. (hd/superposition books) (to :device :cpu))
   (hd/superposition books))

  (py.. books -device)
  (py.. books -device)


  (torch/allclose
   (hd/superposition (py.. books (to :device :cpu)))
   (py.. (hd/superposition books) (to :device :cpu)))

  (torch/allclose books books)
  (torch/conj books)

  (hd/similarity
   [(hd/superposition (py.. books (to :device :cpu)))
    (py.. (hd/superposition books) (to :device :cpu))])

  (torch/allclose
   (py.. (torch/sum books) (to :device :cpu))
   (torch/sum (py.. books (to :device :cpu))))

  (def e (hd/superposition books))
  (torch/allclose e (hd/superposition books))
  (System/gc)

  (require-python '[numpy :as np])




  (def a (hd/seed))
  (def b (hd/seed))


  (torch/allclose
   (torch/from_numpy (np/divide (py.. a (numpy)) (py.. b (numpy))))
   (hd/unbind a b))

  (torch/allclose
   (torch/divide a b)
   (hd/unbind a b))

  ;; def norm(x):
  ;; return torch.norm(x) / torch.sqrt(torch.tensor((x.size()[0]))) * 2




  (torch/norm a)
  a


  (torch/allclose a (torch/sgn a))
  (torch/allclose a (torch/div a (norm a)))

  (hd/similarity a (torch/div a (norm a)))
  (hd/similarity a (torch/div a (torch/abs a)))




  (hd/seed))















(comment
  (System/gc)



  (time
   (doall
    (for [attenuation-factor [0]
          ;; excitability-factor [0 0.001 0.002 0.5 1 1.5]
          excitability-factor [1.1 1 0]]
      (for [book-size [150]]
        (let [trials 20]
          {:attenuation-factor attenuation-factor
           :book-zise book-size
           :excitability-factor excitability-factor
           :results
           (/ (count
               (filter :success?
                       (doall
                        (for [n (range trials)]
                          (let [books (hd/random [3 book-size
                                                  (:fhrr/dimensions
                                                   hd/*opts*)])
                                a (py/get-item books [0 -1])
                                b (py/get-item books [1 -1])
                                c (py/get-item books [2 -1])
                                x (hd/bind [a b c])]
                            (select-keys (resonator-fhrr
                                          x
                                          books
                                          (merge resonator-defaults
                                                 {:attenuation-factor
                                                  attenuation-factor
                                                  :excitability-factor
                                                  excitability-factor
                                                  :max-iterations 20}))
                                         [:n-iteration :success?]))))))
              trials)})))))


  #_(({:attenuation-factor 0 :book-zise 150 :excitability-factor 1.1 :results 4/5})
     ({:attenuation-factor 0 :book-zise 150 :excitability-factor 1 :results 7/10})
     ({:attenuation-factor 0 :book-zise 150 :excitability-factor 0 :results 9/10}))




  (time
   (doall
    (for [cleanup-k [0.99 1 1.001 1.005 1.01]]
      (for [book-size [150]]
        (let [trials 20]
          {:book-zise book-size
           :cleanup-k cleanup-k
           :results
           (/ (count
               (filter :success?
                       (doall (for [n (range trials)]
                                (let [books (hd/random [3 book-size
                                                        (:fhrr/dimensions
                                                         hd/*opts*)])
                                      a (py/get-item books [0 -1])
                                      b (py/get-item books [1 -1])
                                      c (py/get-item books [2 -1])
                                      x (hd/bind [a b c])]
                                  (select-keys
                                   (resonator-fhrr
                                    x
                                    books
                                    (merge resonator-defaults
                                           {:cleanup-k cleanup-k
                                            :max-iterations 20}))
                                   [:n-iteration :success?]))))))
              trials)})))))


  '(({:book-zise 150 :cleanup-k 0.99 :results 17/20})
    ({:book-zise 150 :cleanup-k 1 :results 13/20})
    ({:book-zise 150 :cleanup-k 1.001 :results 17/20})
    ({:book-zise 150 :cleanup-k 1.005 :results 9/10})
    ({:book-zise 150 :cleanup-k 1.01 :results 7/10}))

  '(({:book-zise 150 :cleanup-k 1 :results 9/10})
    ({:book-zise 150 :cleanup-k 1.001 :results 9/10})
    ({:book-zise 150 :cleanup-k 1.1 :results 17/20})
    ({:book-zise 150 :cleanup-k 1.5 :results 17/20})
    ({:book-zise 150 :cleanup-k 2 :results 1/2}))












  (time
   (doall
    (for [max-iterations [10 20 30 40 50]]
      (for [book-size [150]]
        (let [trials 20]
          {:book-zise book-size
           :max-iterations max-iterations
           :results
           (/ (count
               (filter :success?
                       (doall (for [n (range trials)]
                                (let [books (hd/random [3 book-size
                                                        (:fhrr/dimensions
                                                         hd/*opts*)])
                                      a (py/get-item books [0 -1])
                                      b (py/get-item books [1 -1])
                                      c (py/get-item books [2 -1])
                                      x (hd/bind [a b c])]
                                  (select-keys
                                   (resonator-fhrr
                                    x
                                    books
                                    (merge resonator-defaults
                                           {:max-iterations max-iterations}))
                                   [:n-iteration :success?]))))))
              trials)})))))


  (time
   (doall
    (for [max-iterations [10 20 30]
          book-size [10 20 30 40 50 60 70 80 90 100]]
      (let [trials 20]
        {:book-zise book-size
         :max-iterations max-iterations
         :results
         (/ (count
             (filter :success?
                     (doall (for [n (range trials)]
                              (let [books (hd/random [3 book-size
                                                      (:fhrr/dimensions
                                                       hd/*opts*)])
                                    a (py/get-item books [0 -1])
                                    b (py/get-item books [1 -1])
                                    c (py/get-item books [2 -1])
                                    x (hd/bind [a b c])]
                                (select-keys
                                 (resonator-fhrr
                                  x
                                  books
                                  (merge resonator-defaults
                                         {:max-iterations max-iterations}))
                                 [:n-iteration :success?]))))))
            trials)}))))

  ;; book-size = 50 , num-factors = 3 seems to be within capacity with the this version.

  '({:book-zise 10 :max-iterations 10 :results 1}
    {:book-zise 20 :max-iterations 10 :results 1}
    {:book-zise 30 :max-iterations 10 :results 1}
    {:book-zise 40 :max-iterations 10 :results 1}
    {:book-zise 50 :max-iterations 10 :results 1}
    {:book-zise 60 :max-iterations 10 :results 1}
    {:book-zise 70 :max-iterations 10 :results 1}
    {:book-zise 80 :max-iterations 10 :results 1}
    {:book-zise 90 :max-iterations 10 :results 19/20}
    {:book-zise 100 :max-iterations 10 :results 19/20}
    {:book-zise 10 :max-iterations 20 :results 1}
    {:book-zise 20 :max-iterations 20 :results 1}
    {:book-zise 30 :max-iterations 20 :results 1}
    {:book-zise 40 :max-iterations 20 :results 1}
    {:book-zise 50 :max-iterations 20 :results 1}
    {:book-zise 60 :max-iterations 20 :results 1}
    {:book-zise 70 :max-iterations 20 :results 19/20}
    {:book-zise 80 :max-iterations 20 :results 1}
    {:book-zise 90 :max-iterations 20 :results 19/20}
    {:book-zise 100 :max-iterations 20 :results 1}
    {:book-zise 10 :max-iterations 30 :results 1}
    {:book-zise 20 :max-iterations 30 :results 1}
    {:book-zise 30 :max-iterations 30 :results 1}
    {:book-zise 40 :max-iterations 30 :results 1}
    {:book-zise 50 :max-iterations 30 :results 1}
    {:book-zise 60 :max-iterations 30 :results 1}
    {:book-zise 70 :max-iterations 30 :results 1}
    {:book-zise 80 :max-iterations 30 :results 9/10}
    {:book-zise 90 :max-iterations 30 :results 1}
    {:book-zise 100 :max-iterations 30 :results 1}))
