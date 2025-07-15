(ns benjamin-schwerdtner.problems.busy-beaver
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
   {:max-executions 250}))

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
;; -----------------------

;; The busy beaver fitness landscape:
;;
;; - name the highest number you can
;;
;; NOTE:
;;
;; - The maximum busy-beaver in this push implementation is 1e12 because of the coercion in
;;   benjamin-schwerdtner.clj-push3-play.stack.pushvalues
;;
;; - frugal busy beaver is more interesting
;;

(defn busy-beaver
  [individual]
  (let [push-state (execute (:push/program individual))]
    (assoc individual
           :fitness (max (or (-> push-state
                                 :stacks
                                 :push/integer
                                 peek)
                             0))
           :push-state push-state)))


(comment
  (:fitness (busy-beaver {:push/program (list 100)}))
  100
  ;; max:
  (:fitness (busy-beaver {:push/program '(2 2 exec_y (integer_dup integer_*))})))


;; -----------------

(comment
  (take 3
        (sort-by
         :fitness
         (fn [a b] (compare b a))
         (filter
          (comp #(< 0 %) :fitness)
          (for [individual
                (repeatedly 100 (fn [] {:push/program (code)}))]
            (busy-beaver individual))))))




;; ========================

;; Frugal busy beaver

;; - name the largest integer you can   -> out-num
;; - fitness = out-num / number of execution steps
;;
;; - (in the context of this push implementation)
;;

(defn frugal-busy-beaver
  [individual]
  (let [push-state (execute (:push/program individual))
        out-num (max (or (-> push-state
                             :stacks
                             :push/integer
                             peek)
                         0))]
    (merge individual
           {:fitness (/ out-num (:push/num-executions push-state))
            :push-state push-state})))

(comment

  (take 3
        (sort-by :fitness
                 (fn [a b] (compare b a))
                 (filter (comp #(< 0 %) :fitness)
                         (for [individual (repeatedly
                                           100
                                           (fn [] {:push/program (code)}))]
                           (frugal-busy-beaver individual)))))

  ;; ----------------------

  (def m-ecosystem (repeatedly 100 (fn [] {:push/program (code)})))
  (def e-ecosystem (repeatedly 100 (fn [] {:push/program (code)})))

  (def dat
    (doall
     (for [m
           [{:push/program (list 'mut_drop_rand_one)}]]
       (doall
        (for [e e-ecosystem]
          (let [e2 {:push/program
                    (mut/mutate (:push/program e)
                                (:push/program m))}]
            (if (<= (:fitness (frugal-busy-beaver e))
                    (:fitness (frugal-busy-beaver e2)))
              [:inc [m e2]]
              [:no-inc [e]])))))))

  ;; in progres..
  )
