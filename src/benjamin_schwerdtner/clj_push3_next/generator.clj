(ns benjamin-schwerdtner.clj-push3-next.generator
  (:require [benjamin-schwerdtner.clj-push3-next.util :as util]))


;; ?
;; #1 spec it through and use a quick check generator?
;; #2 implement plushy, plush?
;; #3 high dimensional vectors as genomes
;; #4 Implement Algorithmic mutation (like G. Chaitin), start with '()
;;



;; ----------------------

(defn all-identifiers [state]
  (concat
   (:instructions state)
   (vals (:bindings state))))

(defn generate-code
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
        (if (<= max-points (util/count-points program))
          prev-program
          (recur program
                 (conj program
                       (if (< (rand) (branch-probability depth))
                         (generate-code (merge opts
                                               {:depth (inc depth)
                                                :max-points
                                                (- max-points
                                                   points)}))
                         ((first (shuffle atom-generators)))))))))))


(def default-generators
  [(fn [] (rand-nth [true false]))
   (fn [] (rand-int 100))
   (fn [] (gensym "gen_push_sym"))
   ;; clj-objects
   (fn [] (rand-nth [{} #{} [] nil]))])

(defn generators
  [state]
  (concat default-generators
          [(fn [] (rand-nth (into [] (all-identifiers state))))]))







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
