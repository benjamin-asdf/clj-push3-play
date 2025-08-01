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


  '{:test-out
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
     grid_dup
     grid_max_color
     grid_filter_color
     ;; grid_dup
     grid_swap
     (3 3 integer_* grid_zeroes_square)
     (grid_fill_with_pattern)))

  (arc/run-task
   task
   '(grid_dup
     grid_dup
     grid_max_color
     grid_filter_color
     (3 3 integer_* grid_zeroes_square)
     (grid_swap grid_fill_with_pattern))))


(comment
  (def task (d/read-task "/home/benj/repos/ARC-AGI-2/data/training/4be741c5.json"))
  (arc/run-push-on-grid-1
   (-> task :test first :input)
   ;; --------------------------
   '()
   )


  ;;
  ;; color correspondance: same colors in input and output
  ;; output: color count == grid element count
  ;;
  ;; objectness: vertical/horizontal, from side to side
  ;;  - object: same color
  ;;  - object: non symetric
  ;;  - object: long/wide 'tall'
  ;;
  ;; output is smaller than input -> pooling?
  ;;
  ;;
  ;; colors in 'layers'.
  ;; the size of a color 'layer' doesn't matter. But they all go from one to the other side.
  ;; Solution to the grid are the layers.
  ;; can be flipped 90degree
  ;;
  ;; - the same alg with a profunctor that flips input-output solves it
  ;;   - how to flip?
  ;;

  ;;
  ;; - color correspondance and color count == output count suggest an 'ordering' concept.
  ;;
  ;; - colors going through the whole grid remind me of 'colums' or 'layers'.
  ;; - (In some ways, the concept of 'layer' is the key to the puzzle)
  ;;
  ;;
  ;; - no symmetry
  ;;

  ;; - implementing a 'contrast scan' would solve the problem
  ;; - 'object topology'
  ;;





  )
