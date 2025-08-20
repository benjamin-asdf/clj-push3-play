(ns benjamin-schwerdtner.clj-push3-play.hdc.resonator
  (:require
   [clojure.math.combinatorics :refer [cartesian-product]]
   [libpython-clj2.python :refer [py. py..] :as py]
   [libpython-clj2.require :refer [require-python]]

   [benjamin-schwerdtner.clj-push3-play.hdc.fhrr :as hd]
   [benjamin-schwerdtner.clj-push3-play.hdc.data :as hdd]))

(require-python '[torch :as torch])

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

(def update-ratio 0.05)
(def noise-level 0.005)
; (def update-ratio 1) ; Removed - using 0.05 from line above


(def attenuation (atom nil))

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

  x-estimate(t + 1) = g(XX^T(s ⊙ y-estimate(t)^-1 ⊙ z-estimate(t)^-1 ))


  ⊙ is bind
  ^-1 is inverse
  s is the input / target (to be factorized)


  So for each factor,

  - unbind with the 'other factor' estimates, yielding an estimate of the factor.
  - perform a cleanup with the given book, using the outer matrix product of the book.
  - This step is conceptually similar to a Hopfield Net.
  - Normalize outputs with an activation function `g`, prevent run away effects.

  "
  [target books estimates]
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

        attn
        (torch/einsum "nd,nmd->nm" new-estimate
                      (torch/resolve_conj
                       (torch/conj books)))

        patt-update (torch/einsum "nm,nmd->nd" attn books)


        ;; -------------------------


        pattern (torch/add (torch/mul update-ratio patt-update)
                           (torch/mul (- 1 update-ratio) estimates))

        ;; --------------------------------------------------------------------

        ;; ----------------------------
        ;;
        ;; 4.
        ;; out = g(pattern)
        ;; using g = sgn
        outputs (torch/sgn pattern)


        ;; + noise
        outputs
        (torch/add
         outputs
         (torch/mul
          (torch/randn_like outputs)
          (torch/mul
           noise-level
           (hd/normalize (hd/superposition books)))))

        ;; --------------------------------------------



        ]





    ;; (swap!
    ;;  attenuation
    ;;  (fn [a] (torch/add a outputs)))



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
  ([x books] (resonator-fhrr x books {}))
  ([x books
    {:keys [max-iterations similarity-threshold]
     :or {max-iterations 13 similarity-threshold 0.9}}]
   (let [initial-estimates (hd/superposition books)
         finish-info
           (fn [{:keys [n-iteration estimates]}]
             (cond (hd/similar? x
                                (hd/normalize (hd/bind estimates))
                                similarity-threshold)
                     (let [factor-idxs (mapv (fn [x book] (hd/cleanup-idx x book)) estimates books)]
                       {:factor-idxs factor-idxs-cpu
                        :factors
                        (into [] (map (fn [idx b] (py/get-item b (py.. idx item))) factor-idxs books))
                        :finish-reason :factorized
                        :success? true})
                   (< max-iterations n-iteration)
                     {:finish-reason :max-iterations-reached
                      :success? false}
                   :else nil))]
     (loop [{:keys [n-iteration estimates attenuation excitability] :as state}
            {:estimates initial-estimates
             :excitability (torch/ones_like initial-estimates)
             :attenuation (torch/zeros_like initial-estimates)
             :n-iteration 0}]
       ;; ------------------------
       (if-let [info (finish-info state)]
         (merge state info)
         (let
             [estimates (resonator-step-fhrr x books estimates)
              estimates
              (->
               estimates
               (torch/mul excitability)

               ;; E = E - attenuation
               (hd/superposition (hd/negative attenuation)))

              attenuation
              ;; attenuation(t+1) = attenuation(t) * (1 - attenuation-decay-factor) + estimates(t+1) * attenuation-factor
              (torch/add
               (torch/mul attenuation 0.9)
               (torch/mul estimates 0.5))

              ;; exc(t+1) = exc(t) * (1 - exct-decay) + exct(t+1) * excitability-grow-factor
              excitability
              (torch/add
               (torch/mul excitability 0.9)
               (torch/mul estimates 1))

              ;; attenuation (torch/add attenuation estimates)
              ]


             (println "attn: "
                      (mag attenuation)
                      " exc: "
                      (mag excitability)
                      "act: " (mag estimates))


             (recur {:estimates estimates
                     :attenuation attenuation
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

  (defn mag [a]
    (torch/div
     (torch/sum (torch/real (torch/mul a (torch/conj a))))
     (:fhrr/dimensions hd/*opts*)))


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
  (System/gc))
