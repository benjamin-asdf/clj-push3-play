(ns benjamin-schwerdtner.problems.arc.manual2
  (:require
   [benjamin-schwerdtner.clj-push3-play.interpreter :as push]
   [benjamin-schwerdtner.problems.arc.data :as d]
   [benjamin-schwerdtner.clj-push3-play.stack.pushstate :as stack]
   [benjamin-schwerdtner.clj-push3-play.arc.grid-impl :as grid]
   [benjamin-schwerdtner.problems.arc.push :as arc]))

;; --------------------------------------------



(comment

  (def task (d/read-task "/home/benj/repos/ARC-AGI-2/data/training/d631b094.json"))

  (arc/run-push-on-grid-1
   (-> task
       :test
       first
       :input)
   '(grid_dup
     grid_max_color_nonzero
     grid_count_non_zero
     (1 grid_zeroes)
     (grid_fill)))


  (arc/run-task task
                '(grid_dup
                  grid_max_color_nonzero
                  grid_count_non_zero
                  (1 grid_zeroes)
                  (grid_fill)))


  {:test-out
   ({:out [[4 4 4 4 4]]
     :pair {:input [[4 4 0] [4 0 4] [0 0 4]] :output [[4 4 4 4 4]]}
     :success? true})
   :test-success [true]
   :train-out
   ({:out [[7]]
     :pair {:input [[0 7 0] [0 0 0] [0 0 0]] :output [[7]]}
     :success? true}
    {:out [[2 2 2]]
     :pair {:input [[0 2 0] [2 0 0] [0 2 0]] :output [[2 2 2]]}
     :success? true}
    {:out [[8 8 8 8]]
     :pair {:input [[0 8 0] [8 8 0] [8 0 0]] :output [[8 8 8 8]]}
     :success? true}
    {:out [[1 1]]
     :pair {:input [[0 0 0] [1 0 0] [0 1 0]] :output [[1 1]]}
     :success? true})
   :train-success [true true true true]}
  )












;; ---------------------------------------




(comment

  (def task (d/read-task "/home/benj/repos/ARC-AGI-2/data/training/c3e719e8.json"))



  (arc/run-push-on-grid-1
   (-> task :test first :input)
   '(grid_dup
     grid_max_color_nonzero


     ))




  )
