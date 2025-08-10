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



;; TODO: Resonator networks

;; Resonator networks for factoring distributed representations of data structures
;; Frady, Kent, Olshausen, Sommer 2020
;; Neural Computation 32, 2311-2331 (2020)

;; https://arxiv.org/abs/2007.03748










;; --------------------------------------

(def factorize exhaustive-search-factorize)
