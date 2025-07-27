(ns benjamin-schwerdtner.clj-push3-play.arc.grid
  (:require
   [benjamin-schwerdtner.clj-push3-play.prot :refer
    [PushValue m-typeof-item]]
   [libpython-clj2.require :refer [require-python]]
   [libpython-clj2.python :refer [py. py..] :as py]))

(require-python '[torch :as torch]
                '[torch.nn.functional :as F])

;; --------------------

(definterface ArcGrid)

(extend-protocol PushValue
  ArcGrid
  (m-typeof-item [_this] :push/arc-grid))

;; (m-typeof-item (reify ArcGrid))

;; --------------

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

(defn changed? [g1 g2]
  (< 0.01 (py.. (torch/sum (torch/abs (torch/sub g1 g2))) item)))

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
        (if-not (changed? out grid)
          (ensure-reduced out)
          out)))
    grid
    (range n))))

(defn color-swap
  "Returns a grid where the color `from` is swaped to `to`."
  [grid from to]
  #_(ca-apply-rule
     grid
     (write-rule
      {:kernel [
                [0 0 0]
                [0 1 0]
                [0 0 0]]
       :mask-color from
       :new-color to
       :old-color from
       :threshold 1}))
  (torch/where (torch/eq grid from) to grid))


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
    [ ;; reproduction rule
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
