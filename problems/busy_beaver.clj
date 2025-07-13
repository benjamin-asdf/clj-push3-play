(ns busy-beaver
  (:require
   [benjamin-schwerdtner.clj-push3-play.generator :as gen]
   [benjamin-schwerdtner.clj-push3-play.stack.pushstate :as stack]
   [benjamin-schwerdtner.clj-push3-play.instructions.interface :as
    instructions]
   [benjamin-schwerdtner.clj-push3-play.interpreter :as push]
   [benjamin-schwerdtner.clj-push3-play.mutation.mut :as mut]))



;; -----------------------

(defn execute [program]
  (push/execute
   (push/setup-state)
   program
   {:max-executions 50}))

;; -----------------------

;; The busy beaver fitness landscape:
;;
;; - name the highest number you can
;;
;; NOTE:

(defn busy-beaver
  [individual]
  (assoc individual
         :fitness (let [push-state (execute (:push/program individual))]
                    (max (or (-> push-state
                                 :stacks
                                 :push/integer
                                 peek)
                             0)
                         ;; (or (-> push-state :stacks :push/float
                         ;; peek) 0)
                         ))))

(comment
  (:fitness (busy-beaver {:push/program (list 100)}))
  100

  ;; actually with 200 executions this number becomes so large that bigint arithmetic is too slow.
  ;; with 120 executions it is very slow to print the number
  ;;

  (def ftn
    (:fitness (busy-beaver {:push/program '(2 2 exec_y (integer_dup integer_*))})))



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

  ;; ----------------------



  )















(comment
  ;; seam

  (defn single-feature-entity [])

  (declare n)

  ;; E
  ;; this needs to cover all primitives so that they are available in the ecosystem
  (def ecosystem (repeatedly n single-feature-entity))

  (declare composition unstable? stop-condition?)

  (loop [[a b :as E] ecosystem]
    (if (stop-condition? ecosystem)
      ecosystem
      (let [a+b (composition a b)]
        (if
            (unstable? a+b a b)
            ecosystem
            (conj ecosystem a+b)))))

  ;; Pareto Dominance
  (defn unstable? [ecosystem a+b a b]
    (some)))
