(ns benjamin-schwerdtner.problems.hiff
  (:require [random-seed.core :as rand]))

;; -------------------------------

;; =================
;; Hierachical IFF
;; =================

;; R. Watson, G. S. Hornby, J. B. Pollack 2006
;; Modeling building-block interdependency
;; https://link.springer.com/chapter/10.1007/BFb0056853

;; 0 0 = 1
;; 1 0 = 0
;; 0 1 = 1
;; 1 1 = 1
(defn iff [a b]
  (if (= a b) 1 0))

;; idea:
;; - label 'interpret' sub blocks with transform function
;; - combine and go up the hierarchy

;; -------------------------
;;
;; In Dawkins fashion, imagine labeling the teeth as 'carnivore' or 'herbivore'.
;; Label the stomach as 'carnivore' or 'herbivore'. etc.
;;
;; The structure of a modular problem is that for example the harmony between
;; the submodules counts to large degree to the fitness.
;;
;; -------------------------

;; string of symbols `B` = { b1, ..., bn }, over an alphabe S where n=k^p

;; represents a hierachical block structure
;;
;; k = number of sub-blocks in a block
;; p = is the number of levels in the hierarchy
;;
;; at the bottom, transform each block (consisting of k symbols) to a single
;; symbol by the transform function `t`.
;;
;;

;; - creates a new string with length k^(p-1), (first level decoded)
;; - repeat for each level in the hierachy to make a single symbol
;;


;; p: hierachy-levels
;; k: block-size

;;
;; T(B) =
;;
(defn blocks [size coll]
  (partition (/ (count coll) size) coll))

(defn hierachical-transform
  [transform block-size [elm :as input]]
  (if (= (count input) 1)
    ;; atom level
    elm
    (apply
     transform
     (map
      (partial hierachical-transform transform block-size)
      ;; block size also means how many args does `t` take.
      (blocks block-size input)))))

;; F(B) =
(defn hierachical-block-fitness
  "
  f: f(x) -> number, the fitness of a single symbol.
"
  [transform f block-size [elm :as input]]
  (if (= (count input) 1)
    (f elm)
    ;; -------
    (+ (* (count input)
          (f (hierachical-transform transform block-size input)))
       (reduce +
               (map
                (partial hierachical-block-fitness transform f block-size)
                (blocks block-size input))))))


(def hierachical-if-and-only-if
  (partial
   hierachical-block-fitness
   (fn t [a b]
     ;; t outcome is ∈ {0,1,nil}
     ({[0 0] 1 [1 1] 1} [a b]))
   (fn f [a]
     ;; 1 -> 1
     ;; 0 -> 1
     ;; nil -> 0
     ({0 1 1 1} a 0))
   2))


(comment
  (hierachical-if-and-only-if [0 0 1 1])

  (hierachical-if-and-only-if [0 0 0 0 1 1 1 1])

  ;; This is basically the canonical function that is hard for hill climbing algorithms
  ;; The local and global optima are maximally far apart.

  ;; max-fit for 64bit:

  (hierachical-if-and-only-if (repeatedly 64 (constantly 0)))
  448
  (hierachical-if-and-only-if (repeatedly 64 (constantly 1)))
  448)


(defn permute-vec
  "Return a permuted vector by `perm`
  'shuffle-indices'.
  ."
  [perm v]
  (mapv #(nth v %) perm))

(defn make-shuffled-hiff
  [{:keys [hierachy-levels]}]
  ;; k == 2
  (let [block-size 2
        bit-string-length (long (Math/pow block-size hierachy-levels))
        bit-perm (into [] (rand/shuffle (range bit-string-length)))]
    {:shuffle-perm bit-perm
     :shuffled-hiff (fn [input-vec]
                      (hierachical-if-and-only-if
                       (permute-vec bit-perm input-vec)))}))




(comment
  [(hierachical-transform iff 2 [0 1 1 1])
   (hierachical-transform iff 2 [1 1 1 1])]
  [0 1]



  (make-shuffled-hiff {:hierachy-levels 2})

  )
