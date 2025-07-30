;; Program Synthesis and Genetic Algorithm Ideas
;;
;; 1. Multiple Push Programs Approach:
;;    - Create a library of sub-programs for common ARC operations
;;    - Use program composition to combine smaller programs
;;    - Examples: grid-copy, pattern-repeat, color-count, etc.
;;
;; 2. Symbiogenesis-based Genetic Algorithm:
;;    - Start with a population of random Push programs
;;    - Use crossover operations that preserve program structure
;;    - Implement mutation operators specific to Push:
;;      * Insert/delete instructions
;;      * Replace instructions with similar ones
;;      * Swap code blocks
;;    - Fitness: number of training examples solved correctly
;;
;; 3. Pattern Recognition Module:
;;    - Analyze input/output pairs to detect transformation type
;;    - Use this to guide program generation
;;    - Common patterns: scaling, repetition, rotation, symmetry
;;
;; 4. Stack Effect Analysis:
;;    - Track what each instruction does to the stack
;;    - Use this to generate valid instruction sequences
;;    - Avoid programs that would cause stack errors

 (ns benjamin-schwerdtner.problems.arc.synthesis1
   (:require
    [benjamin-schwerdtner.clj-push3-play.interpreter :as push]
    [benjamin-schwerdtner.problems.arc.data :as d]
    [benjamin-schwerdtner.clj-push3-play.stack.pushstate :as stack]
    [benjamin-schwerdtner.clj-push3-play.arc.grid-impl :as grid]))

(defn count-non-zero-cells [grid]
  (->> grid
       flatten
       (filter #(not= 0 %))
       count))

(defn analyze-pattern [train-examples]
  (for [{:keys [input output]} train-examples]
    (let [non-zero-count (count-non-zero-cells input)
          input-h (count input)
          input-w (count (first input))
          output-h (count output)
          output-w (count (first output))]
      {:input-size [input-h input-w]
       :output-size [output-h output-w]
       :non-zero-count non-zero-count
       :size-ratio [(/ output-h input-h) (/ output-w input-w)]})))

(def arc-task-91413438-push-program
  '(push/input
    grid_dup
    0
    grid_count_color
    9
    integer_sub
    integer_dup
    integer_*
    integer_dup
    2
    integer_swap
    grid_resize_top_left_bg
    grid_dup
    push/input
    3
    3
    0
    0
    grid_window
    integer_dup
    integer_dec
    exec_do*times
    (3
     integer_add
     grid_dup
     push/input
     3
     integer_dup
     2
     integer_yank_dup
     3
     0
     0
     grid_window
     grid_set_cell)))

(def arc-push-program-v2
  '(push/input
    grid_dup
    0
    grid_count_color
    9
    integer_sub
    integer_dup
    3
    integer_*
    integer_dup
    0
    grid_resize_top_left_bg
    push/input
    integer_dup
    integer_dec
    0
    exec_do*range
    (integer_dup
     3
     integer_*
     integer_dup
     3
     integer_add
     0
     exec_do*range
     (grid_dup
      push/input
      5
      integer_yank_dup
      4
      integer_yank_dup
      3
      3
      0
      0
      grid_window
      5
      integer_yank_dup
      4
      integer_yank_dup
      grid_set_cell))))

(def arc-push-program-simple
  '(push/input
    grid_dup
    0
    grid_count_color
    9
    integer_sub
    integer_dup
    3
    integer_*
    integer_dup
    0
    grid_resize_top_left_bg))

(defn test-push-program [program input-grid]
  (let [initial-state (-> (push/setup-state)
                          (stack/push :push/grid input-grid))
        result-state (push/execute initial-state program {:max-executions 1000})]
    (-> result-state :stacks :push/grid peek)))

(defn copy-pattern-to-grid [source-grid target-grid start-x start-y]
  (let [[src-h src-w] (grid/shape source-grid)]
    (reduce (fn [g [y x]]
              (let [color (grid/get-cell source-grid x y)]
                (grid/set-cell g (+ start-x x) (+ start-y y) color)))
            target-grid
            (for [y (range src-h) x (range src-w)] [y x]))))

(defn solve-arc-task [input-grid]
  (let [non-zero-count (grid/count-color input-grid 0)
        non-zero-count (- 9 non-zero-count)
        output-size (* non-zero-count 3)
        output-grid (grid/resize-grid-top-left input-grid output-size output-size 0)]
    (reduce (fn [g idx]
              (let [row (quot idx non-zero-count)
                    col (rem idx non-zero-count)]
                (copy-pattern-to-grid input-grid g (* col 3) (* row 3))))
            output-grid
            (range (* non-zero-count non-zero-count)))))

(defn solve-arc-pattern [input]
  (let [non-zero-count (->> input flatten (filter #(not= 0 %)) count)
        output-size (* non-zero-count 3)
        output (vec (repeat output-size (vec (repeat output-size 0))))]
    (reduce (fn [grid position]
              (let [row (quot position non-zero-count)
                    col (rem position non-zero-count)
                    start-row (* row 3)
                    start-col (* col 3)]
                (reduce (fn [g [y x]]
                          (assoc-in g [(+ start-row y) (+ start-col x)]
                                    (get-in input [y x])))
                        grid
                        (for [y (range 3) x (range 3)] [y x]))))
            output
            (range (* non-zero-count non-zero-count)))))

(defn solve-arc-pattern-v2 [input]
  (let [non-zero-count (->> input flatten (filter #(not= 0 %)) count)
        output-size (* non-zero-count 3)
        output (vec (repeat output-size (vec (repeat output-size 0))))]
    (reduce (fn [grid position]
              (let [row (quot position non-zero-count)
                    col (rem position non-zero-count)]
                (if (< position (* non-zero-count non-zero-count))
                  (let [start-row (* row 3)
                        start-col (* col 3)]
                    (reduce (fn [g [y x]]
                              (assoc-in g [(+ start-row y) (+ start-col x)]
                                        (get-in input [y x])))
                            grid
                            (for [y (range 3) x (range 3)] [y x])))
                  grid)))
            output
            (range output-size))))

(defn solve-arc-pattern-correct [input]
  (let [non-zero-count (->> input flatten (filter #(not= 0 %)) count)
        output-size (* non-zero-count 3)
        output (vec (repeat output-size (vec (repeat output-size 0))))]
    (reduce (fn [grid idx]
              (let [row (quot idx non-zero-count)
                    col (rem idx non-zero-count)
                    start-row (* row 3)
                    start-col (* col 3)]
                (if (and (< start-row output-size) (< start-col output-size))
                  (reduce (fn [g [y x]]
                            (if (and (< (+ start-row y) output-size)
                                     (< (+ start-col x) output-size))
                              (assoc-in g [(+ start-row y) (+ start-col x)]
                                        (get-in input [y x]))
                              g))
                          grid
                          (for [y (range 3) x (range 3)] [y x]))
                  grid)))
            output
            (range (* non-zero-count non-zero-count)))))

(defn solve-arc-final [input]
  (let [zero-count (->> input flatten (filter #(= 0 %)) count)
        output-size (* zero-count 3)
        output (vec (repeat output-size (vec (repeat output-size 0))))]
    (reduce (fn [grid idx]
              (let [row (quot idx zero-count)
                    col (rem idx zero-count)
                    start-row (* row 3)
                    start-col (* col 3)]
                (if (and (< start-row output-size) (< start-col output-size))
                  (reduce (fn [g [y x]]
                            (if (and (< (+ start-row y) output-size)
                                     (< (+ start-col x) output-size))
                              (assoc-in g [(+ start-row y) (+ start-col x)]
                                        (get-in input [y x]))
                              g))
                          grid
                          (for [y (range 3) x (range 3)] [y x]))
                  grid)))
            output
            (range (* zero-count zero-count)))))

(defn solve-arc-task-correct [input]
  (let [zero-count (->> input flatten (filter #(= 0 %)) count)
        non-zero-count (->> input flatten (filter #(not= 0 %)) count)
        output-size (* zero-count 3)
        output (vec (repeat output-size (vec (repeat output-size 0))))]
    (reduce (fn [grid idx]
              (let [row (quot idx zero-count)
                    col (rem idx zero-count)
                    start-row (* row 3)
                    start-col (* col 3)]
                (reduce (fn [g [y x]]
                          (if (and (< (+ start-row y) output-size)
                                   (< (+ start-col x) output-size))
                            (assoc-in g [(+ start-row y) (+ start-col x)]
                                      (get-in input [y x]))
                            g))
                        grid
                        (for [y (range 3) x (range 3)] [y x]))))
            output
            (range non-zero-count))))

(def arc-push-program-final
  '(push/input
    grid_dup
    0
    grid_count_color
    integer_dup
    3
    integer_*
    integer_dup
    0
    grid_resize_top_left_bg
    push/input
    grid_dup
    0
    grid_count_color
    9
    integer_sub
    0
    exec_do*range
    (integer_dup
     4
     integer_yank_dup
     integer_quot
     3
     integer_*
     integer_dup
     5
     integer_yank_dup
     5
     integer_yank_dup
     integer_mod
     3
     integer_*
     grid_dup
     push/input
     4
     integer_yank_dup
     4
     integer_yank_dup
     3
     3
     0
     0
     grid_window
     grid_dup
     6
     integer_yank_dup
     5
     integer_yank_dup
     grid_resize_top_left_bg
     integer_pop
     integer_pop)))
(defn evaluate-push-program-on-task [program task]
  (let [train-results
        (for [example (:train task)]
          (try
            (let [initial-state (-> (push/setup-state)
                                    (stack/push :push/grid (grid/grid (:input example))))
                  result-state (push/execute initial-state program {:max-executions 1000})
                  result-grid (-> result-state :stacks :push/grid peek)]
              (if (and result-grid
                       (= (vec (map vec (grid/rows result-grid)))
                          (:output example)))
                1.0
                0.0))
            (catch Exception e 0.0)))
        score (/ (reduce + train-results) (count train-results))]
    score))

(comment

  (evaluate-push-program-on-task arc-push-program-clean task))

(defn generate-arc-sub-programs []
  {:count-zeros '(grid_dup 0 grid_count_color)
   :count-non-zeros '(grid_dup 0 grid_count_color 9 integer_sub)
   :resize-by-factor '(integer_dup 3 integer_* integer_dup 0 grid_resize_top_left_bg)
   :get-input-cell '(push/input grid_get_cell)
   :copy-3x3-block '(0 exec_do*range 3
                       (0 exec_do*range 3
                          (grid_dup push/input
                                    6 integer_yank_dup 5 integer_yank_dup
                                    grid_get_cell
                                    7 integer_yank_dup 6 integer_yank_dup integer_add
                                    8 integer_yank_dup 8 integer_yank_dup integer_add
                                    grid_set_cell)))})

(defn create-program-synthesis-framework []
  {:mutation-operators
   {:insert-random (fn [prog]
                     (let [pos (rand-int (inc (count prog)))
                           new-instr (rand-nth '[integer_add integer_sub integer_dup
                                                 grid_dup grid_get_cell grid_set_cell
                                                 0 1 2 3 4 5 6 7 8 9])]
                       (concat (take pos prog) [new-instr] (drop pos prog))))
    :delete-random (fn [prog]
                     (if (> (count prog) 1)
                       (let [pos (rand-int (count prog))]
                         (concat (take pos prog) (drop (inc pos) prog)))
                       prog))
    :swap-adjacent (fn [prog]
                     (if (> (count prog) 1)
                       (let [pos (rand-int (dec (count prog)))]
                         (concat (take pos prog)
                                 [(nth prog (inc pos)) (nth prog pos)]
                                 (drop (+ pos 2) prog)))
                       prog))}

   :crossover (fn [prog1 prog2]
                (let [cut1 (rand-int (inc (count prog1)))
                      cut2 (rand-int (inc (count prog2)))]
                  [(concat (take cut1 prog1) (drop cut2 prog2))
                   (concat (take cut2 prog2) (drop cut1 prog1))]))

   :fitness-function evaluate-push-program-on-task})

(comment
  ;; Rich REPL testing for ARC Push program

  ;; First, let's understand what we need:
  ;; 1. Input is a 3x3 grid
  ;; 2. Count zeros (black squares) -> determines output size
  ;; 3. Count non-zeros -> determines how many times to repeat pattern
  ;; 4. Create output grid of size (zeros*3) x (zeros*3)
  ;; 5. Fill with input pattern repeated non-zero times

  (require '[benjamin-schwerdtner.problems.arc.synthesis1 :as syn1] :reload)
  (require '[benjamin-schwerdtner.clj-push3-play.interpreter :as push])
  (require '[benjamin-schwerdtner.clj-push3-play.stack.pushstate :as stack])
  (require '[benjamin-schwerdtner.clj-push3-play.arc.grid-impl :as grid])
  (require '[benjamin-schwerdtner.problems.arc.data :as d])

  (def task (d/read-task "/home/benj/repos/ARC-AGI-2/data/training/91413438.json"))
  (def first-example (first (:train task)))

  ;; Expected: 3 zeros -> 9x9 output, 6 non-zeros -> 6 repetitions
  (println "Input:" (:input first-example))
  (println "Expected output size:" (count (:output first-example)) "x" (count (first (:output first-example))))

  ;; Create initial state with grid on stack
  (def test-grid (grid/grid (:input first-example)))
  (def state-0 (-> (push/setup-state)
                   (stack/push :push/grid test-grid)))

  ;; Step 1: We need the grid twice - once for counting, once for copying
  ;; But we don't have grid_dup! We need to find the right instruction
  ;; Let's check what stack operations are available

  ;; For now, let's manually put two grids on the stack
  (def state-with-2-grids (-> state-0
                              (stack/push :push/grid test-grid)))

  ;; Step 2: Count zeros
  (def state-after-count (push/execute state-with-2-grids
                                       '(0 grid_count_color)
                                       {:max-executions 10}))
  (println "Zero count:" (-> state-after-count :stacks :push/integer peek))
  ;; Should be 3

  ;; Step 3: Calculate output size (3 * 3 = 9)
  (def state-with-size (push/execute state-after-count
                                     '(integer_dup 3 integer_*)
                                     {:max-executions 10}))
  (println "Output size:" (-> state-with-size :stacks :push/integer peek))
  ;; Should be 9

  ;; Step 4: Create output grid
  (def state-with-output (push/execute state-with-size
                                       '(integer_dup 0 grid_resize_top_left_bg)
                                       {:max-executions 10}))
  (println "Grid stack count:" (-> state-with-output :stacks :push/grid count))
  (def output-grid (-> state-with-output :stacks :push/grid peek))
  (println "Output grid shape:" (grid/shape output-grid))
  ;; Should be [9 9]
  )

(push/setup-state)

(d/read-task "/home/benj/repos/ARC-AGI-2/data/training/91413438.json")

(def task
  {:file "/home/benj/repos/ARC-AGI-2/data/training/91413438.json"
   :test [{:input [[0 0 8] [0 8 0] [0 0 0]]
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
     [[3 0 3 3 0 3 3 0 3] [3 0 3 3 0 3 3 0 3] [0 3 3 0 3 3 0 3 3]
      [3 0 3 3 0 3 3 0 3] [3 0 3 3 0 3 3 0 3] [0 3 3 0 3 3 0 3 3]
      [0 0 0 0 0 0 0 0 0] [0 0 0 0 0 0 0 0 0] [0 0 0 0 0 0 0 0 0]]}
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

(count [0 4 0 0 4 0 0 4 0 0 4 0 0 0 0])

;;
;; 1. count black squares in input -> a
;;    (or equiv.)
;; 2. set the size to axa, leave input in top left corner
;; 3. count the color in input -> b
;; 4. repeat the input sequare pattern b times from the top left to bottom right
;;    (in effect, divde the grid into 3x3, input size, subgrids, fill from top left
;;    to bottom right the grids with the input patter)
;;
