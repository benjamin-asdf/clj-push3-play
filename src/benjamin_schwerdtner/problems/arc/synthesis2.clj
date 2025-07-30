(ns benjamin-schwerdtner.problems.arc.synthesis2
  (:require
   [benjamin-schwerdtner.clj-push3-play.interpreter :as push]
   [benjamin-schwerdtner.problems.arc.data :as d]
   [benjamin-schwerdtner.clj-push3-play.stack.pushstate :as stack]
   [libpython-clj2.require :refer [require-python]]
   [libpython-clj2.python :refer [py. py..] :as py]
   [benjamin-schwerdtner.clj-push3-play.arc.grid-impl :as grid]))

(require-python '[torch :as torch])

(def task
  {:file "/home/benj/repos/ARC-AGI-2/data/training/91413438.json"
   :test [{:input
           [[0 0 8]
            [0 8 0]
            [0 0 0]]
           :output [[0 0 8 0 0 8 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
                    [0 8 0 0 8 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
                    [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
                    [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
                    [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
                    [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
                    [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
                    [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
                    [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
                    [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
                    [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
                    [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
                    [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
                    [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
                    [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
                    [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
                    [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
                    [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
                    [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
                    [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
                    [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]]}]
   :train
   [{:input [[3 0 3] [3 0 3] [0 3 3]]
     :output
     [[3 0 3 3 0 3 3 0 3]
      [3 0 3 3 0 3 3 0 3]
      [0 3 3 0 3 3 0 3 3]
      [3 0 3 3 0 3 3 0 3]
      [3 0 3 3 0 3 3 0 3]
      [0 3 3 0 3 3 0 3 3]
      [0 0 0 0 0 0 0 0 0]
      [0 0 0 0 0 0 0 0 0]
      [0 0 0 0 0 0 0 0 0]]}
    {:input [[0 4 0] [0 4 4] [4 0 0]]
     :output [[0 4 0 0 4 0 0 4 0 0 4 0 0 0 0]
              [0 4 4 0 4 4 0 4 4 0 4 4 0 0 0]
              [4 0 0 4 0 0 4 0 0 4 0 0 0 0 0]
              [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
              [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
              [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
              [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
              [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
              [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
              [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
              [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
              [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
              [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
              [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
              [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]]}
    {:input [[2 0 2] [0 2 0] [0 0 0]]
     :output [[2 0 2 2 0 2 2 0 2 0 0 0 0 0 0 0 0 0]
              [0 2 0 0 2 0 0 2 0 0 0 0 0 0 0 0 0 0]
              [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
              [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
              [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
              [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
              [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
              [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
              [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
              [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
              [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
              [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
              [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
              [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
              [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
              [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
              [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
              [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]]}

    {:input [[6 6 0] [0 6 6] [0 0 6]]
     :output [[6 6 0 6 6 0 6 6 0 6 6 0] [0 6 6 0 6 6 0 6 6 0 6 6]
              [0 0 6 0 0 6 0 0 6 0 0 6] [6 6 0 0 0 0 0 0 0 0 0 0]
              [0 6 6 0 0 0 0 0 0 0 0 0] [0 0 6 0 0 0 0 0 0 0 0 0]
              [0 0 0 0 0 0 0 0 0 0 0 0] [0 0 0 0 0 0 0 0 0 0 0 0]
              [0 0 0 0 0 0 0 0 0 0 0 0] [0 0 0 0 0 0 0 0 0 0 0 0]
              [0 0 0 0 0 0 0 0 0 0 0 0] [0 0 0 0 0 0 0 0 0 0 0 0]]}]})

;;
;; 1. count black squares in input -> a
;;    (or equiv.)
;; 2. set the size to axa, leave input in top left corner
;; 3. count the color in input -> b
;; 4. repeat the input sequare pattern b times from the top left to bottom right
;;    (in effect, divde the grid into 3x3, input size, subgrids, fill from top left
;;    to bottom right the grids with the input patter)
;;

(defn run-push-on-grid
  [input-grid program]
  (->
   (push/execute
    (stack/stack-push
     (push/setup-state)
     :push/grid
     (grid/grid input-grid))
    program)
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


(comment

  (def program
    '(grid_dup
      (0 grid_count_color)
      integer_dup
      ((3 integer_*) grid_zeroes_square)
      (9 integer_- grid_pack)))

  (select-keys (run-task task program) [:test-success :train-success])

  {:test-success [true], :train-success [true true true true]}





  )




;; ======================================================


(comment

  (/ (count [0 0 8 0 0 8 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]) 3)

  (run-push
   (-> task
       :test
       first
       :input)
   '(grid_dup
     (0 grid_count_color)
     (3 integer_*)
     (integer_dup grid_dup grid_resize_top_left)
     (grid_dup grid_max_pool)))


  (run-push
   (-> task :test first :input)
   '(grid_dup
     (0 grid_count_color)
     integer_dup
     ((3 integer_*) grid_zeroes_square)
     (9 integer_- grid_pack)))



  ;; --------------------------------------
  (->
   (grid/grid
    [[0 0 0]
     [0 3 0]
     [0 0 0]])
   (grid/resize-grid-top-left 9 9))

  ;; tensor
  ;; ([[0., 0., 0., 0., 0., 0., 0., 0., 0.],
  ;;   [0., 3., 0., 0., 0., 0., 0., 0., 0.],
  ;;   [0., 0., 0., 0., 0., 0., 0., 0., 0.],
  ;;   [0., 0., 0., 0., 0., 0., 0., 0., 0.],
  ;;   [0., 0., 0., 0., 0., 0., 0., 0., 0.],
  ;;   [0., 0., 0., 0., 0., 0., 0., 0., 0.],
  ;;   [0., 0., 0., 0., 0., 0., 0., 0., 0.],
  ;;   [0., 0., 0., 0., 0., 0., 0., 0., 0.],
  ;;   [0., 0., 0., 0., 0., 0., 0., 0., 0.]])
  )
