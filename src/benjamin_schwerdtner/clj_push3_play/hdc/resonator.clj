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

;;
;; reference impl
;; -  (not FHRR) https://github.com/spencerkent/resonator-networks/blob/master/resonator_networks/dynamics/rn_numpy.py#L7
;; -  https://codeocean.com/capsule/1211768/tree/v1
;;

;; WIP
(def update-ratio 0.0005)
(def update-ratio 0.05)
(def noise-level 0.005)

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
        (torch/einsum "nd,nmd->nm" new-estimate (torch/conj books))
        ;; optionally bias, nonlinearity
        ;; pattern (torch/einsum "nm,nmd->nd" attn books)
        patt-update (torch/einsum "nm,nmd->nd" attn books)
        pattern (torch/add (torch/mul update-ratio patt-update)
                           (torch/mul (- 1 update-ratio) estimates))
        ;; ----------------------------
        ;;
        ;; 4. non linearity | act | normalize
        ;; out = g(pattern)
        ;; using g = sgn
        outputs (torch/sgn pattern)
        ;; outputs (torch/abs pattern)
        ;;
        ;; optionally + noise level (effectively causing
        ;; restarts)
        ;; outputs pattern
        ;; outputs

        ;; #1 noise;
        #_(torch/add outputs
                     (torch/mul noise-level
                                (torch/randn_like outputs)))

        ;; #2
        ;; noise makes more sense if not random but drawn from
        ;; the search space?
        #_(torch/add outputs
                     (torch/mul
                      (torch/randn_like outputs)
                      (torch/mul noise-level
                                 (hd/normalize
                                  (hd/superposition
                                   books)))))
        ;; #3:
        ;; Try scale the noise according to an error?

        error (torch/sub
               1
               (hd/similarity target (hd/bind outputs)))
        _ (println "error: " error)

        outputs
        (torch/add
         outputs
         (torch/mul
          (torch/randn_like outputs)
          (torch/mul
           (torch/mul error 2)
           (hd/normalize (hd/superposition books)))))



        ]
    ;; (def pattern pattern)
    (torch/squeeze outputs)))




(defn resonator-fhrr
  "Perform a resonator factorization of `x` with `books`.
  Returns an outcome info map with the following keys:

  `:factors`: a seq of hdvs, one for each book. Binding the together yields something similar to `x`.
  `:finish-reason`: a keyword indicating why the factorization finished.
                    Can be `:factorized` or `:max-iterations-reached`.
  `:success?`: true if the factorization was successful, false otherwise.


  `n`: factors count
  `m`: Book resolution
  `d`: hdv dimension

  shapes:

  books: (n, m, d)
  x: (d)

  opts:

  `similarity-threshold`: Sim as by [[hd/similarity]] for which estimates are accepted.
                          Depends on how dirty your input is.

  `max-iterations`: Number of iterations before giving up.

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
                 (let [factor-idxs
                       (torch/stack
                        (mapv (fn [x book]
                                (hd/cleanup-idx x book))
                              estimates
                              books))]
                   {:factor-idxs factor-idxs
                    :factors (into []
                                   (map (fn [idx b]
                                          (py/get-item b idx))
                                        factor-idxs
                                        books))
                    :finish-reason :factorized
                    :success? true})
                 (< max-iterations n-iteration)
                 {:finish-reason :max-iterations-reached
                  :success? false}
                 :else nil))]
     (loop [{:keys [n-iteration estimates] :as state}
            {:estimates initial-estimates :n-iteration 0}]
       (if-let [info (finish-info state)]
         (merge state info)
         (recur {:estimates (resonator-step-fhrr x books estimates)
                 :n-iteration (inc n-iteration)}))))))

(def factorize resonator-fhrr)
