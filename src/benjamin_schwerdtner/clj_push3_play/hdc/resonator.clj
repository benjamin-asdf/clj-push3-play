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
;; and       x = bind(p(a-0,0), p(a-1,1), ... )
;;
;;
;;
;; where `p(x,n)` is the permutation 'exponentiated' to n.
;; i.e. permuted by n times.
;;

;; Note that
;;
;; f = `bound-seq`,
;;
;; f(a,b,c,...) = bind(p(a,0),p(b,1),...)  is given by:
;;

;; [[benjamin-schwerdtner.hdc.data/bound-seq]]

;;
;; The reference factorizer implementation
(defn exhaustive-search-factorize
  "Returns a seq of hdvs from `books`,

  such that


            a-0 ∈ book-0, a-1 ∈ book-1, ...

  and       x = bind(p(a-0,0), p(a-1,1), ... )


  where p(x,n) is [[hd/permute]]. I.e. this is the
  [[benjamin-schwerdtner.clj-push3-play.hdc.data/bind-seq]]

  We say the hdvs are the factors of x and this function factorizes x.

  Returns nil, if no such list of vectors is found.

  This does an exhaustive search over all possible hdv combinations.

  "
  [x books]
  ;; search space is the cartesian product of the hdvs in books
  (->>
   (apply cartesian-product books)
   (some
    (fn [hdvs] (when (hd/similar? x (hdd/bind-seq hdvs)) hdvs)))))

(comment

  ;; "Elapsed time: 67.839989 msecs"
  (let
   [books [(hd/seed 10)
           (hd/seed 10)
           (hd/seed 10)]
    [a b c] (mapv (fn [b]
                    (py/get-item b (rand-int 10)))
                  books)
    x (hdd/bind-sequence a b c)

    [af bf cf]
    (into [] (exhaustive-search-factorize x books))]

    (hd/similarity
     (torch/stack [a b c])
     (torch/stack [af bf cf])))

  ;; tensor([[1.0000e+00, 4.6433e-04, 7.4571e-03],
  ;;         [4.6433e-04, 1.0000e+00, 7.8958e-03],
  ;;         [7.4571e-03, 7.8958e-03, 1.0000e+00]])
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
;; X = { x1, ..., xD }, Y = {  }
;;
;; e.g. D = 100
;;

;;
;; The resonator combines superposition and cleanup
;;
;;

(def ⊙ hd/bind)

;; references:

;; https://github.com/spencerkent/resonator-networks/blob/master/resonator_networks/dynamics/rn_numpy.py#L7
;;

(defn resonator-step-fhrr
  "


  shapes:

  - target: (d)
  - books: (n, m, d)
  - estimates: (n, d)
  - output: (n, d)



  d: hdv dimension
  n: books/ factors count
  m: book resolution

  "
  [target books estimates]
  (let [n (py.. estimates (size -2))
        estimates (py.. estimates (unsqueeze 0))

        ;;
        ;; x-estimate(t + 1) = g(XX^T(s ⊙ y-estimate(t) ⊙ z-estimate(t) )
        ;;
        ;;                                |_____________________________|
        ;;                                       inv-other^-1               <- 1
        ;;
        ;;
        ;;                           |___________________________________|
        ;;                              new-estimate                        <- 2
        ;;                              x-hat
        ;;
        ;;
        ;;                  |________|                                      <- 3
        ;;
        ;; ======================
        inv-estimates (hd/inverse estimates)
        ;;
        ;; now find the 'other' hdvs for each hdv
        ;;
        ;; [
        ;;   [z y]     <- x
        ;;   [x z]     <- y
        ;;   [x y]     <- z
        ;;          ]
        inv-estimates
        (->
         (reduce
          (fn [rolled i]
            (into rolled (py.. inv-estimates (roll i -2))))
          []
          (range 1 n))
         (torch/stack :dim -2))
        ;;
        ;; bind the 'other' estimates togther
        ;;
        ;; x-inv-others: y^-1 ⊙ z^-1
        ;; shape:
        ;; (n, d)
        ;;                                                     <-  1

        ;; _ (def inv-estimates-1 inv-estimates)
        inv-others (hd/bind inv-estimates)
        _ (def inv-others-1 inv-others)
        ;;
        ;;
        ;; now bind with the input, the closer we are to the solution,
        ;; the closer x-hat to x
        ;; (because unbinding target with the other estimates produces x)
        ;;
        ;;
        ;; [
        ;;   x-hat  =  s ⊙ (y^-1 ⊙ z^-1)
        ;;   y-hat     ...
        ;;   z-hat
        ;;    ]
        _ (def target target)
        _ (def inv-others inv-others)
        new-estimates (hd/bind target inv-others)
        ;; new-estimates (py.. new-estimates (unsqueeze 0))
        ;;                                                      <- 2
        ;;
        ;;
        ;; cleanup new estimates with codebooks
        ;; outer matrix product:
        ;;
        ;; XX^T * x-hat
        ;;
        _ (def new-estimates new-estimates)
        _ (def books books)

        ;; how close each estimate is to each vec in it's codebook ('attention')
        attention
        (torch/einsum "nd,nmd->n" new-estimates (torch/conj books))
        _ (def attention attention)

        ;;
        ;; return a hdv superposition, scaled by the (attention) closeness value
        ;; if x-hat is very close to x, then outputs[0] ~= x
        ;;
        outputs-1
        (torch/einsum "n,nmd->nd" attention books)
        ;; g activation function
        ;;
        ;; x-estimate = g(XX^T * x-hat)
        ;;
        g torch/sgn
        outputs (g outputs-1)
        ;;                                                    <- 3
        ]
    _ (def outputs outputs)
    outputs))


(defn resonator-fhrr
  "

  Perform a resonator factorization of `x` with `books`.

  `n`: factors count
  `m`: Book resolution
  `d`: hdv dimension

  shapes:

  books: (n, m, d)
  x: (d)

  "
  ([x books] (resonator-fhrr x books {}))
  ([x books {:keys [max-iterations] :or {max-iterations 50}}]
   (let [initial-estimates (hd/superposition books)
         finish-info
         (fn [{:keys [n-iteration estimates]}]
           (cond
             (hd/similar? x
                          (py.. (hd/normalize
                                 (hd/bind estimates))
                            (squeeze)))
             {:finish-reason :factorized
              :success? true}
             (< max-iterations n-iteration)
             {:finish-reason :max-iterations-reached
              :success? false}
             :else nil))]
     (loop [{:keys [n-iteration estimates x-estimate] :as state}
              {:estimates initial-estimates :n-iteration 0}]
       (if-let [info (finish-info state)]
         (merge state info)
         (recur {:estimates (resonator-step-fhrr x books estimates)
                 :n-iteration (inc n-iteration)}))))))

(comment

  (py.. inv-estimates-1 (size))
  ;; torch.Size([6, 10000])

  (do
    (def books
      (torch/stack
       [(hd/seed 3)
        (hd/seed 3)
        (hd/seed 3)]))
    (def n (py.. books (size -2)))
    (def a (py/get-item books [0 0]))
    (def b (py/get-item books [1 0]))
    (def c (py/get-item books [2 0]))
    (def x (hd/bind [a b c]))
    ;; (def out (resonator-fhrr x books))
    ;; out
    (def estimates (hd/superposition books))
    (def estimates (py.. estimates (unsqueeze 0)))
    (def inv-estimates
      (let [inv-estimates (hd/inverse estimates)
            inv-estimates (-> (reduce (fn [rolled i]
                                        (into rolled
                                              (py.. inv-estimates
                                                (roll i -2))))
                                      []
                                      (range 1 n))
                              (torch/stack :dim -2))]
        inv-estimates))
    (def inv-others (hd/bind inv-estimates))
    (def new-estimates (hd/bind x inv-others))
    (def attention (torch/einsum "nd,nmd->n" new-estimates (torch/conj books)))
    (def outputs (torch/einsum "n,nmd->nd" attention books))
    (def outputs (torch/sgn outputs)))

  (hd/similarity
   (hd/cleanup
    (py/get-item outputs 0)
    (py/get-item books 0))
   (py/get-item books 0)
   ;; (torch/stack [a b c])
   )



  (hd/similarity
   (hd/cleanup-many outputs books)
   books)



  )





(comment
  ;; torch.Size([1, 2, 10000])

  (let [X (hd/seed 2)
        a (py.. (py/get-item X 0) (unsqueeze 0))
        activations (torch/einsum "ij,kj->ik" a (torch/conj X))
        outputs (torch/einsum "ik,kj->ij" activations X)
        outputs (torch/sgn outputs)
        ]
    activations
    (py.. outputs (size))
    (hd/cleanup a outputs)
    (hd/similarity a outputs)
    (hd/similarity a X)
    (hd/similarity outputs X))







  (let [X (hd/seed 2)
        a (py.. (py/get-item X 0) (unsqueeze 0))
        ;; a (hd/superposition a (hd/seed))
        a (hd/seed)
        activations (torch/einsum "ij,kj->ik" a (torch/conj X))
        outputs (torch/einsum "ik,kj->ij" activations X)
        outputs (torch/sgn outputs)]
    activations
    ;; (hd/similar? a)
    (py.. outputs (size))
    (hd/cleanup a outputs)
    (hd/similarity a outputs))


  (let [X (torch/stack
           [
            (hd/seed 2)
            (hd/seed 2)
            (hd/seed 2)])
        a (py.. (py/get-item X [0 0])
            (unsqueeze 0)
            (unsqueeze 0))
        activations (torch/einsum "bij,kj->bik" a (torch/conj X))
        outputs (torch/einsum "bik,kj->bij" activations X)
        outputs (torch/sgn outputs)]
    activations
    ;; (hd/similar? a)
    (py.. outputs (size))
    (hd/cleanup a outputs)
    (hd/similarity a outputs))





  ;; torch.Size([1, 1, 10000])






  )







(def factorize
  "Default factorizer - uses resonator network for efficiency.
   Falls back to exhaustive search if resonator fails."
  resonator-factorize)

























(comment


  (py.. books (size))
  (py.. (hd/superposition books) (size))


  (hd/superposition books)


  (hd/similarity
   (hd/superposition books)
   (py/get-item books [0 0]))






  (py..
      (resonator-step-fhrr
       (hd/seed 1)
       (py.. books (unsqueeze 0))
       (py.. (hd/superposition books) (unsqueeze 0)))
      (size))



  (py.. (py.. (hd/superposition books) (unsqueeze 0)) (size))


  (let [a (hd/seed)
        b (hd/seed)
        c (hd/seed)
        s (py.. (hd/bind [a b c]) (unsqueeze 0))
        x (py.. (torch/stack [a b c]) (squeeze 1))]
    (py.. x (size))
    (py.. s (size))
    (py.. (hd/bind s x) (size))
    (hd/bind s (hd/inverse x))
    (hd/similarity (hd/bind s (hd/inverse x))
                   [(hd/bind b c) (hd/bind a c) (hd/bind a b)]))





  )







(comment

  (let [X (torch/stack [(hd/seed 2) (hd/seed 2) (hd/seed 2)])
        a (py/get-item X [0 0])
        b (py/get-item X [1 0])
        c (py/get-item X [2 0])
        est (torch/stack [a b c])
        activations
        (torch/einsum
         "bi,bki->bk"
         est
         (torch/conj X))
        outputs
        (torch/einsum
         "bk,bki->bi"
         activations X)
        ;; outputs (torch/sgn outputs)
        ]
    ;; (py.. a (size))
    ;; (py.. X (size))
    ;; activations
    ;; (hd/similar? a)
    ;; (py.. outputs (size))
    ;; (hd/cleanup a outputs)
    ;; (hd/similarity a outputs)
    ;; (py.. est (size))
    activations
    ;; (hd/similarity X outputs)
    (hd/similarity
     (py/get-item outputs 0)
     (py/get-item X 0)))

  (let [X (hd/seed 2)
        a (py.. (py/get-item X 0) (unsqueeze 0))
        activations (torch/einsum "ij,kj->ik" a (torch/conj X))
        outputs (torch/einsum "ik,kj->ikj" activations X)
        outputs (torch/sgn outputs)
        ]
    activations
    (hd/similarity (torch/sgn outputs) X)
    ;; (hd/similar? a)
    (hd/cleanup a outputs)
    (py.. outputs (size)))





  )
