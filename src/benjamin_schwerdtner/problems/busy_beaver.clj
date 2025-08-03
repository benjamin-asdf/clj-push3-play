(ns benjamin-schwerdtner.problems.busy-beaver
  (:require
   [benjamin-schwerdtner.clj-push3-play.generator :as gen]
   [benjamin-schwerdtner.clj-push3-play.stack.pushstate :as stack]
   [benjamin-schwerdtner.clj-push3-play.instructions.interface :as
    instructions]
   [benjamin-schwerdtner.clj-push3-play.interpreter :as push]
   [benjamin-schwerdtner.clj-push3-play.mutation.mut :as mut]))


;; -----------------------
;;
;; T. Radó, "On Non-Computatble Functions", Bell systems Technical J. 41 (May 1962), pp. 877-884.





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

;; - out-num <- name the largest integer you can
;; - fitness = out-num / number of execution steps
;; - return fitness
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


;; Chaitin Metabiology:
;; -------------------------


;; Life == randomly evolving software
;; A single software organism O, a methematician!
;; Biological creativity == Mathematical creativity
;; Fitness of O == Busy Beaver problem == size of output
;;    (HERE: frugal busy beaver, output in relationship to execution count)
;;
;; Naming big numbers: N, N + N, N * N, N^N, N^NN N times
;; Evolution == hill-climbing random walk, O, O', O'', ...
;;
;;    (BUT: R. Watson: modularity, not hill climbing, devivide and conquer!)
;;
;; Probability of K-bit algorithmic mutation M = 2-k
;; Non-algorithmic orcacles elimitate bad mutations M
;;
;;    (HERE: no oracle, push program is executed with max execution count,
;;     handles halting problem, makes it practical)
;;
;; Fitness of O increases faster than any computable function!
;; Thus evolutions is creative, not mechanical!

;; --------------------------
;;
;;


;; - single software organism P, a program
;; - randomly mutate P until it's fitness is higher, replace P
;; - Crucial: Allow algorithmic mutaitons.
;; - M is a K-bit program that takes the original organism A as input,
;;   output:
;; - A' = M(A)
;; - probabily 2-K, probability of each transformation that can be described in K
;;   bits is 1/2K
;;
;; - `mutation distance`:
;; - log2 is the probability of going from A to B with a single mutation
;;
;; - this is the same as the 'program size complexity' of the simplest function
;;    M  with B = M(A)
;;   also known as the 'relative information content' of B given A. (G. Chaitin 1975 ACM paper)
;;
