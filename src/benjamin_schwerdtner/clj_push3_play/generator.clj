(ns benjamin-schwerdtner.clj-push3-play.generator
  (:refer-clojure :exclude [rand rand-int rand-nth shuffle])
  (:require
   [benjamin-schwerdtner.clj-push3-play.util :as util]
   [random-seed.core :refer [rand-nth rand rand-int]]))

;;
;; NOTE:
;; - be deliberate about using random functions only from random-seed.core namespace!
;; - random seed is set by interpreter.clj
;;



;; ?
;; #1 spec it through and use a quick check generator?
;; #2 implement plushy, plush?
;; #3 high dimensional vectors as genomes
;; #4 Implement Algorithmic mutation (like G. Chaitin), start with '()
;;

;; ----------------------

(defn rand-atom
  ([atom-generators _random-seed]
   (rand-atom atom-generators))
  ([atom-generators]
   ((rand-nth atom-generators))))

(defn generate-code
  "Naive.

  Returns a list that is a mix of lists and atoms.

  branch-probability: A function that given a depth returns a probabilty for branching (making a nested list).

  close-probability: dito, but for closing the current list (including being done with the top level form).
  This is there because the output was often a nested list with nothing else.

  max-points: code length counted by [[util/count-points]]. The output has max-points or less code points.
  "
  [{:keys [max-points branch-probability atom-generators depth
           close-probability]
    :or {branch-probability (constantly 0.2)
         close-probability (fn [n] (/ (- 1 (/ 1 n)) 3))
         depth 1}
    :as opts}]
  (loop [prev-program (list)
         program (list)]
    (let [points (util/count-points program)]
      (if (< (rand) (close-probability depth))
        program
        (cond (= max-points points) program
              (< max-points points) prev-program
              :else (recur program
                           (conj program
                                 (if (< (rand)
                                        (branch-probability depth))
                                   (generate-code
                                    (merge opts
                                           {:depth (inc depth)
                                            :max-points (- max-points
                                                           points)}))
                                   (rand-atom atom-generators)))))))))



(defn rand-bool []
  (rand-nth [true false]))

(def default-generators
  [rand-bool
   (fn [] (rand-int 100))
   ;;
   ;; ??
   ;; This is not deterministic, but conceptually arguably it is,
   ;; since every symbol is equivalent to the others.
   ;; But calling = on the output programs won't work.
   ;;
   (fn [] (gensym "push_gensym_"))
   ;; clj-objects
   (fn [] (rand-nth [{} #{} [] nil]))])

(defn identifier-generators [identifiers]
  (let [identifiers (into [] identifiers)]
    [(fn [] (rand-nth identifiers))]))



;; --------------------



(comment
  (generate-code
   {:max-points 10
    :branch-probability (constantly 0)
    :close-probability (constantly 0)
    :atom-generators
    [(fn [] 1)
     (fn [] 2)]})
  '(2 2 1 1 2 1 1 1)

  (generate-code
   {:atom-generators
    [(fn [] 1) (fn [] 2)]
    :branch-probability
    (constantly 0.2)
    :close-probability
    (fn [n]
      (/ (- 1 (/ 1 n)) 3))
    :max-points 10})
  (for [n (range 1 10)]
    ((fn [n] (- 1 (/ 1 n))) n))

  (generate-code
   {:atom-generators [(fn [] 0) (fn [] 1) (fn [] '+)]
    :branch-probability (constantly 0.2)
    :close-probability (fn [n] (/ (- 1 (/ 1 n)) 3))
    :max-points 10}))
