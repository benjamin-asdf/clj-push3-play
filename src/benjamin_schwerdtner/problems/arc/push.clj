(ns benjamin-schwerdtner.problems.arc.push
  (:require
   [benjamin-schwerdtner.clj-push3-play.interpreter :as push]
   [benjamin-schwerdtner.problems.arc.data :as d]
   [benjamin-schwerdtner.clj-push3-play.stack.pushstate :as stack]
   [libpython-clj2.require :refer [require-python]]
   [libpython-clj2.python :refer [py. py..] :as py]
   [benjamin-schwerdtner.clj-push3-play.arc.grid-impl :as grid]))

(defn run-push-on-grid-1
  [input-grid program]
  (->
   (push/execute
    (stack/stack-push
     (push/setup-state)
     :push/grid
     (grid/grid input-grid))
    program)))

(defn run-push-on-grid
  [input-grid program]
  (->
   (run-push-on-grid-1 input-grid program)
   (stack/peek-item :push/grid)
   (grid/clj)))

(defn run-on-task-pair [{:keys [input output] :as pair} program]
  (let [out (run-push-on-grid input program)]
    {:success?
               (= out output)
     :out out
     :pair pair}))

(defn run-task [task program]
  (let [train-out (map #(run-on-task-pair % program) (:train task))
        test-out (map #(run-on-task-pair % program) (:test task))]
    {:train-success (mapv :success? train-out)
     :test-success (mapv :success? test-out)
     :train-out train-out
     :test-out test-out}))


(comment)
