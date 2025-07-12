(ns busy-beaver
  (:require
   [benjamin-schwerdtner.clj-push3-play.generator :as gen]
   [benjamin-schwerdtner.clj-push3-play.stack.pushstate :as stack]
   [benjamin-schwerdtner.clj-push3-play.instructions.interface :as
    instructions]
   [benjamin-schwerdtner.clj-push3-play.interpreter :as push]))

;; -----------------------

(defn execute [program]
  (push/execute (push/setup-state) program {:max-executions 200}))

;; -----------------------

;; The busy beaver fitness landscape:
;;
;; - name the highest number you can
;;


(defn busy-beaver [individual]
  (let [push-state (execute (:push/program individual))]
    (max
     (or
      (-> push-state :stacks :push/integer peek)
      0)
     ;; (or (-> push-state :stacks :push/float peek) 0)
     )))

(comment
  (busy-beaver {:push/program (list 100)})
  100

  )


;; -----------------



(comment

  (defn code
    []
    (into (list)
          (->
           (push/execute
            (-> (push/setup-state)
                (assoc-in
                 [:parameters :max-points-in-random-expressions]
                 250))
            (list (+ 50 (rand-int 200)) 'code_rand))
           :stacks
           :push/code)))

  ;; bunch of rand programs

  (take 3
        (sort-by
         :fitness
         (fn [a b] (compare b a))
         (filter (comp #(< 0 %) :fitness)
                 (for [individual
                       (repeatedly 100 (fn [] {:push/program (code)}))]
                   (assoc individual :fitness (busy-beaver individual)))))))




;; ========================

;; Frugal busy beaver

;; - name the largest integer you can   -> out-num
;; - fitness = out-num / number of execution steps
;;

(defn frugal-busy-beaver [individual]
  (let [push-state (execute (:push/program individual))
        out-num (max (or (-> push-state :stacks :push/integer peek) 0))]
    (/ out-num (:push/num-executions push-state))))

(comment


  (take 3
        (sort-by :fitness
                 (fn [a b] (compare b a))
                 (filter (comp #(< 0 %) :fitness)
                         (for [individual (repeatedly
                                           100
                                           (fn [] {:push/program (code)}))]
                           (assoc individual
                                  :fitness (frugal-busy-beaver individual))))))


  )
