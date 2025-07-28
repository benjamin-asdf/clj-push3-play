(ns benjamin-schwerdtner.clj-push3-play.arc.grid-impl
  (:require
   [benjamin-schwerdtner.clj-push3-play.prot :refer
    [PushValue m-typeof-item]]
   [libpython-clj2.require :refer [require-python]]
   [libpython-clj2.python :refer [py. py..] :as py]))

(require-python '[torch :as torch]
                '[torch.nn.functional :as F])

(def colors (into [] (range 10)))

(defn make-color-gensym
  []
  (let [cnt (atom (count colors))]
    (fn []
      (let [i @cnt]
        (swap! cnt inc)
        i))))

(defonce color-gensym (make-color-gensym))

(defn zeroes [dims]
  (torch/tensor dims))

(defn grid [values
            ;; {:keys [device]}
            ]
  (torch/tensor values :dtype torch/float))

(defn fill [grid color]
  (torch/fill grid color))

(defn color-swap
  "Returns a grid where the color `from` is swaped to `to`."
  [grid from to]
  #_(ca-apply-rule
     grid
     (write-rule
      {:kernel [[0 0 0]
                [0 1 0]
                [0 0 0]]
       :mask-color from
       :new-color to
       :old-color from
       :threshold 1}))
  (torch/where (torch/eq grid from) to grid))

(defn get-cell
  "Get the value at position [x y] in the grid"
  [grid x y]
  (py.. grid (item [x y]) item))

(defn set-cell
  "Set the value at position [x y] in the grid to color"
  [grid x y color]
  (let [new-grid (py.. grid clone)]
    (py.. new-grid (setitem [x y] color))
    new-grid))

(defn count-color
  "Count occurrences of a specific color in the grid"
  [grid color]
  (-> (torch/eq grid color)
      (torch/sum)
      (py.. item)))

(defn color-frequencies
  "Return a map of color -> count for all colors in the grid"
  [grid]
  (let [flat-grid (py.. grid flatten)
        unique-colors (py.. (torch/unique flat-grid) tolist)]
    (into {}
          (for [color unique-colors]
            [color (count-color grid color)]))))

(defn max-color
  "Return the color that appears most frequently in the grid"
  [grid]
  (let [freqs (color-frequencies grid)]
    (if (empty? freqs)
      0
      (key (apply max-key val freqs)))))

(defn positions-of-color
  "Return all [x y] positions where the specified color appears"
  [grid color]
  (let [mask (torch/eq grid color)
        indices (py.. (torch/nonzero mask) tolist)]
    (vec (for [[y x] indices] [x y]))))

(defn positions-of-max-color
  "Return all [x y] positions where the most frequent color appears"
  [grid]
  (positions-of-color grid (max-color grid)))

(defn rows [grid]
  (vec grid))

(defn columns [grid]
  (vec (torch/t grid)))

(defn window
  ([grid [x y] size] (window grid [x y] size size))
  ([grid [x y] width height]
   (if (or (>= x (py.. grid (size 0)))
           (>= y (py.. grid (size 1)))
           (< x 0)
           (< y 0)
           (<= width 0)
           (<= height 0)
           (< (py.. grid (size 1)) (+ x width))
           (< (py.. grid (size 0)) (+ y height)))
     grid
     (-> grid
         (torch/narrow 1 x width)
         (torch/narrow 0 y height)
         (py.. clone)))))

(defn shape [grid]
  (vec (py.. grid (size))))

(comment
  (shape (grid [0])))

;; ====================================
;;
;; https://www.kaggle.com/code/daviddamario/evolving-cellular-automata-to-solve-arc-tasks
;;

;; 1. A rule gene encoding:

;;
;; Version 1: Encode colors in 5 bits, encode kernel binary
;;
;;
;;
;; +--------+--------+--------------+-------------+-------------+-----------+
;; |        |        |              |             |             |           |
;; |        | 0-8    | 9-13         | 13-17       | 17-21       | 21-25     |
;; +--------+--------+--------------+-------------+-------------+-----------+
;; |        | kernel | mask color   | old color   | new color   | threshold |
;; +--------+--------+--------------+-------------+-------------+-----------+
;; |        | 9 bit  | 5 bit        | 5 bit       | 5 bit       | 5 bit     |
;; +--------+--------+--------------+-------------+-------------+-----------+
;;
;; meta,idx
;;
;;
;; Version 2:
;;
;; - use float32 (torch default)
;;
;;
;; +--------+--------+--------------+-------------+-------------+-----------+
;; |        |        |              |             |             |           |
;; |        | 0-8    | 9            | 10          | 11          | 12        |
;; +--------+--------+--------------+-------------+-------------+-----------+
;; |        | kernel | mask color   | old color   | new color   | threshold |
;; +--------+--------+--------------+-------------+-------------+-----------+
;;
;; meta,idx
;;
;;
;; kernel: 3x3 kernel for convolution, a neighbour activation encoding.
;; mask color: Color the rule applies to, cell activations only come from this color.
;; old color: Color at target cell that rule applies to.
;; new color: Transformed color at target cell.
;; threshold: Threshold for rule to apply, i.e. neighbour count.
;;

;;
;; In version 2: kernel is weighted
;; In version 1: kernel is binary
;;

(defn rand-gene
  "
  Returns a random CA rule gene tensor of the following form:


  ;;
  ;; +--------+--------+--------------+-------------+-------------+-----------+
  ;; |        |        |              |             |             |           |
  ;; |        | 0-8    | 9            | 10          | 11          | 12        |
  ;; +--------+--------+--------------+-------------+-------------+-----------+
  ;; |        | kernel | mask color   | old color   | new color   | threshold |
  ;; +--------+--------+--------------+-------------+-------------+-----------+
  ;;

  "
  ([]
   (torch/cat
    [;; 0-8
     (torch/rand :size [(* 3 3)])
     ;; 9,10,11
     (torch/randint :size [(count [9 10 11])] :low 0 :high 8 :dtype torch/float)
     ;; 12
     ;; --
     ;; 3x3 = 9 is max input
     ;; if kernel weights are within range [0..1].
     ;; --
     ;; Allow 10 for disabling the rule.
     (torch/randint :size [1] :low 0 :high 10 :dtype torch/float)]))
  ([num-genes] (torch/stack (vec (repeatedly num-genes rand-gene)))))

(defn kernel [rule]
  (->
   (torch/narrow rule -1 0 9)
   (torch/reshape [-1 3 3])))

(defn read-rule
  [rule]
  {:kernel (kernel rule)
   :mask-color (-> (torch/narrow rule -1 9 1)
                   (torch/reshape [-1 1]))
   :new-color (-> (torch/narrow rule -1 11 1)
                  (torch/reshape [-1 1]))
   :old-color (-> (torch/narrow rule -1 10 1)
                  (torch/reshape [-1 1]))
   :threshold (-> (torch/narrow rule -1 12 1)
                  (torch/reshape [-1 1]))})

(defn write-rule [{:keys [kernel mask-color new-color old-color threshold]}]
  (torch/cat
   [(py.. (torch/tensor kernel :dtype torch/float) (view 9))
    (torch/tensor [mask-color old-color new-color threshold] :dtype torch/float)]))

(comment
  (torch/tensor (torch/tensor 0))

  (write-rule
   {:kernel [[1 0 0]
             [0 0 0]
             [0 0 0]]
    :mask-color 0
    :new-color 0
    :old-color (torch/tensor 0)
    :threshold 2}))

(defn ca-apply-rule
  "Returns a grid with CA rule (gene) applied.

  This is one time step of the CA.

 (ca-apply-rule
   (grid
    [[5 0 3]
     [5 0 3]
     [0 0 0]])
   (torch/tensor
    [0 0 0
     1 0 0
     0 0 0

     5 0 2

     1.0]))

  ;; tensor([[5., 2., 3.],
  ;;         [5., 2., 3.],
  ;;         [0., 0., 0.]])

  see [[rand-gene]]
  "
  [grid rule-gene]
  (let [{:keys [kernel mask-color new-color old-color threshold]}
        (read-rule rule-gene)
        grid-masked (py.. (torch/eq grid mask-color)
                          (type torch/float32))
        activations (py.. (F/conv2d (py.. grid-masked
                                          (unsqueeze 0)
                                          (unsqueeze 0))
                                    (py.. kernel (unsqueeze 0))
                                    :padding
                                    1)
                          (squeeze 0)
                          (squeeze 0))]
    ;; [grid grid-masked activations threshold (torch/ge activations threshold) old-color (torch/eq grid old-color)]
    (torch/where (torch/mul (torch/ge activations threshold)
                            (torch/eq grid old-color))
                 new-color
                 grid)))

(defn equal? [g1 g2]
  (torch/equal g1 g2))

(defn reduce-ca
  "Returns a grid with `rule-gene` CA applied multiple times.

  If the CA leads to a stable pattern, terminates and returns that pattern.
  (Class 1 CA).

  In all other cases, including oscillating patterns, this subroutine terminates after `n` steps.
  Default 1000.

  https://en.wikipedia.org/wiki/Cellular_automaton

  "
  ([grid rule-gene] (reduce-ca grid rule-gene (long 1e3)))
  ([grid rule-gene n]
   (reduce
    (fn [grid _]
      (let [out (ca-apply-rule grid rule-gene)]
        (if (torch/equal out grid)
          (ensure-reduced out)
          out)))
    grid
    (range n))))

(defn flood-fill
  "

  Returns a grid where the cell at position plus its neighbours of the same color,
  recursively, are replaced with `color`.

  Imagine that position is a valey in a flood fill algorithm that stops at
  'other color' cells.


 (flood-fill
   (grid
    [[0 0 0 0]
     [1 1 1 1]
     [0 0 0 0]
     [1 1 1 0]])
   [2 2]
   5)

  tensor([[0., 0., 0., 0.],
          [1., 1., 1., 1.],
          [5., 5., 5., 5.],
          [1., 1., 1., 5.]])


  "
  [grid [x y] color]
  (if-not (and (>= x 0)
               (< x (py.. grid (size 0)))
               (>= y 0)
               (< y (py.. grid (size 1))))
    grid
    (let [current-color (py.. (py/get-item grid [x y]) item)]
      (if (= current-color color)
        grid
        (let
         [marker-color ((color-gensym))
             ;; put a marker 'seed' at the position
          grid
          (torch/index_put grid
                           [(torch/tensor [x]) (torch/tensor [y])]
                           (torch/tensor marker-color :dtype torch/float))]
          (->
           grid
             ;; fill marker color growing from seed
           (reduce-ca
            (write-rule
             {:kernel
              [[0 1 0]
               [1 0 1]
               [0 1 0]]
              :mask-color marker-color
              :new-color marker-color
              :old-color current-color

              :threshold 1}))
             ;; subst marker to target color
           (color-swap marker-color color)))))))

(comment

  (ca-apply-rule
   (grid
    [[1 1 1]
     [0 0 0]
     [1 1 1]])
   (write-rule
    {:kernel
     [[0 1 0]
      [1 0 1]
      [0 1 0]]
     :mask-color 0
     :new-color 2
     :old-color 0
     :threshold 1}))

  (reduce-ca
   (grid
    [[0 0 0 0]
     [1 1 1 0]
     [0 10 0 0]
     [1 1 1 0]])
   (write-rule
    {:kernel
     [[0 10 0]
      [10 0 10]
      [0 10 0]]
     :mask-color 10
     :new-color 10
     :old-color 0
     :threshold 10}))

  (reduce-ca
   (grid
    [[0 0 0 0]
     [1 1 1 1]
     [0 10 0 0]
     [1 1 1 0]])
   (write-rule
    {:kernel
     [[0 10 0]
      [10 0 10]
      [0 10 0]]
     :mask-color 10
     :new-color 10
     :old-color 0
     :threshold 10}))

  (flood-fill
   (grid
    [[0 0 0 0]
     [1 1 1 1]
     [0 0 0 0]
     [1 1 1 0]])
   [2 2]
   5)

;; tensor([[0., 0., 0., 0.],
  ;;         [1., 1., 1., 1.],
  ;;         [5., 5., 5., 5.],
  ;;         [1., 1., 1., 5.]])

  ((color-gensym))

;; -----------------------
  ;; hm, this is how we might want to say it:
  (def conways-gol
    [;; reproduction rule
     (write-rule {:kernel [[0 1 0]
                           [1 0 1]
                           [0 1 0]]
                  :mask-color 1
                  :new-color 1
                  :old-color 0
                  :threshold 3})
     ;; overpopulation rule
     (write-rule {:kernel [[0 1 0]
                           [1 0 1]
                           [0 1 0]]
                  :mask-color 1
                  :new-color 0
                  :old-color 1
                  :threshold 3})
     ;; underpopulation rule
     (write-rule {:kernel [[0 1 0]
                           [1 0 1]
                           [0 1 0]]
                  :mask-color 0
                  :new-color 0
                  :old-color 1
                  ;; max neighbours 4, if I have only a single or less
                  ;; neighbours, I die.
                  :threshold (- 4 1)})]))

(comment

  (window
   (grid
    [[1 0 1 0]
     [0 1 2 0]
     [1 0 1 0]])
   [1 1]
   3
   2)

  (window
   (grid
    [[1 0 1 0]
     [0 1 2 0]
     [1 0 1 0]])
   [1 1]
   2)

  (window
   (grid
    [[1 0 1 0]
     [0 1 2 0]
     [1 0 1 0]])
   [0 0]
   3))

;; ==================== Rotation Primitives ====================

(defn rotate-90
  "Rotate grid 90 degrees clockwise"
  [grid]
  (-> grid
      (torch/transpose 0 1)
      (torch/flip [1])))

(defn rotate-180
  "Rotate grid 180 degrees"
  [grid]
  (-> grid
      (torch/flip [0])
      (torch/flip [1])))

(defn rotate-270
  "Rotate grid 270 degrees clockwise (90 degrees counter-clockwise)"
  [grid]
  (-> grid
      (torch/transpose 0 1)
      (torch/flip [0])))

(defn rotate
  "Rotate grid by n degrees (must be multiple of 90)"
  [grid degrees]
  (case (mod degrees 360)
    0 grid
    90 (rotate-90 grid)
    180 (rotate-180 grid)
    270 (rotate-270 grid)
    (throw (ex-info "Rotation must be multiple of 90 degrees" {:degrees degrees}))))

;; ==================== Symmetry Primitives ====================

(defn flip-horizontal
  "Flip grid horizontally (left-right)"
  [grid]
  (torch/flip grid [1]))

(defn flip-vertical
  "Flip grid vertically (up-down)"
  [grid]
  (torch/flip grid [0]))

(defn flip-diagonal
  "Flip grid along main diagonal (transpose)"
  [grid]
  (torch/transpose grid 0 1))

(defn flip-antidiagonal
  "Flip grid along anti-diagonal"
  [grid]
  (-> grid
      (rotate-90)
      (flip-horizontal)))

;; ==================== Symmetry Detection ====================

(defn symmetric-horizontal?
  "Check if grid has horizontal symmetry"
  [grid]
  (torch/equal grid (flip-horizontal grid)))

(defn symmetric-vertical?
  "Check if grid has vertical symmetry"
  [grid]
  (torch/equal grid (flip-vertical grid)))

(defn symmetric-diagonal?
  "Check if grid has diagonal symmetry (must be square)"
  [grid]
  (let [[h w] (py.. grid shape)]
    (and (= h w)
         (torch/equal grid (flip-diagonal grid)))))

(defn rotational-symmetry-order
  "Find the order of rotational symmetry (1, 2, or 4)"
  [grid]
  (cond
    (torch/equal grid (rotate-90 grid)) 4
    (torch/equal grid (rotate-180 grid)) 2
    :else 1))

;; ==================== Symmetry Generation ====================

(defn make-symmetric-horizontal
  "Create a horizontally symmetric grid from the left half"
  [half-grid]
  (let [flipped (flip-horizontal half-grid)]
    (torch/cat [half-grid flipped] 1)))

(defn make-symmetric-vertical
  "Create a vertically symmetric grid from the top half"
  [half-grid]
  (let [flipped (flip-vertical half-grid)]
    (torch/cat [half-grid flipped] 0)))

(defn make-symmetric-4fold
  "Create a 4-fold symmetric grid from a quarter"
  [quarter-grid]
  (let [top-right (flip-horizontal quarter-grid)
        top-half (torch/cat [quarter-grid top-right] 1)
        bottom-half (flip-vertical top-half)]
    (torch/cat [top-half bottom-half] 0)))

;; ======================

(defn all-rotations
  "Generate all 4 rotations of a grid"
  [grid]
  [(identity grid)
   (rotate-90 grid)
   (rotate-180 grid)
   (rotate-270 grid)])

(defn all-symmetries
  "Generate all 8 symmetries (D4 group) of a grid"
  [grid]
  (concat (all-rotations grid)
          (all-rotations (flip-horizontal grid))))

;; ==================

;; perception primitives:
;; (some retina inspired)

;; 1. center-surround ON/OFF
;; 2. edge detection
;; 3. 'inner' / 'outer' detector
;; 4.
;;
