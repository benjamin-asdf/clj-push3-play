(ns benjamin-schwerdtner.clj-push3-play.arc.instructions
  (:require
   [benjamin-schwerdtner.clj-push3-play.instructions.impl
    :refer [register-instruction] :as impl]
   [benjamin-schwerdtner.clj-push3-play.generator :as gen]
   [benjamin-schwerdtner.clj-push3-play.arc.grid-impl :as grid]
   [benjamin-schwerdtner.clj-push3-play.stack.pushstate :as stack]))

(register-instruction
 {:sym-name 'grid_zeroes
  :in [:push/integer :push/integer]
  :out :push/grid
  :f (fn [_ width height] (grid/zeroes [width height]))})

(register-instruction
 {:sym-name 'grid_zeroes_square
  :in [:push/integer]
  :out :push/grid
  :f (fn [_ size] (grid/zeroes [size size]))})

(register-instruction
 {:sym-name 'grid_new
  :in [:push/integer :push/integer :push/integer]
  :out :push/grid
  :f (fn [_ width height color]
       (grid/fill (grid/zeroes [height width]) color))
  :doc "Create a new grid with the specified width, height, and fill color."})

(register-instruction
 {:sym-name 'grid_fill
  :in [:push/grid :push/integer]
  :out :push/grid
  :f (fn [_ g color] (grid/fill g color))})

(register-instruction
 {:sym-name 'grid_color_swap
  :in [:push/grid :push/integer :push/integer]
  :out :push/grid
  :f (fn [_ g from to] (grid/color-swap g from to))})

(register-instruction
 {:sym-name 'grid_rows
  :in [:push/grid]
  :out :push/code
  :f (fn [_ g] (grid/rows g))})

(register-instruction
 {:sym-name 'grid_columns
  :in [:push/grid]
  :out :push/code
  :f (fn [_ g] (grid/columns g))})

(register-instruction
 {:sym-name 'grid_window
  :in [:push/grid :push/integer :push/integer :push/integer :push/integer]
  :out :push/grid
  :f (fn [_ g x y w h] (grid/window g x y w h))})

(register-instruction
 {:sym-name 'grid_equal
  :in [:push/grid :push/grid]
  :out :push/boolean
  :f (fn [_ g1 g2] (grid/equal? g1 g2))})

(register-instruction
 {:sym-name 'grid_flood_fill
  :in [:push/grid :push/integer :push/integer :push/integer]
  :out :push/grid
  :f (fn [_ g x y color] (grid/flood-fill g [x y] color))})

(register-instruction
 {:sym-name 'grid_rotate_90
  :in [:push/grid]
  :out :push/grid
  :f (fn [_ g] (grid/rotate-90 g))})

(register-instruction
 {:sym-name 'grid_rotate_180
  :in [:push/grid]
  :out :push/grid
  :f (fn [_ g] (grid/rotate-180 g))})

(register-instruction
 {:sym-name 'grid_rotate_270
  :in [:push/grid]
  :out :push/grid
  :f (fn [_ g] (grid/rotate-270 g))})

(register-instruction
 {:sym-name 'grid_rotate
  :in [:push/grid :push/integer]
  :out :push/grid
  :f (fn [_ g degrees] (grid/rotate g degrees))})

(register-instruction
 {:sym-name 'grid_flip_horizontal
  :in [:push/grid]
  :out :push/grid
  :f (fn [_ g] (grid/flip-horizontal g))})

(register-instruction
 {:sym-name 'grid_flip_vertical
  :in [:push/grid]
  :out :push/grid
  :f (fn [_ g] (grid/flip-vertical g))})

(register-instruction
 {:sym-name 'grid_flip_diagonal
  :in [:push/grid]
  :out :push/grid
  :f (fn [_ g] (grid/flip-diagonal g))})

(register-instruction
 {:sym-name 'grid_flip_antidiagonal
  :in [:push/grid]
  :out :push/grid
  :f (fn [_ g] (grid/flip-antidiagonal g))})

(register-instruction
 {:sym-name 'grid_symmetric_horizontal?
  :in [:push/grid]
  :out :push/boolean
  :f (fn [_ g] (grid/symmetric-horizontal? g))})

(register-instruction
 {:sym-name 'grid_symmetric_vertical?
  :in [:push/grid]
  :out :push/boolean
  :f (fn [_ g] (grid/symmetric-vertical? g))})

(register-instruction
 {:sym-name 'grid_symmetric_diagonal?
  :in [:push/grid]
  :out :push/boolean
  :f (fn [_ g] (grid/symmetric-diagonal? g))})

(register-instruction
 {:sym-name 'grid_rotational_symmetry_order
  :in [:push/grid]
  :out :push/integer
  :f (fn [_ g] (grid/rotational-symmetry-order g))})

(register-instruction
 {:sym-name 'grid_make_symmetric_horizontal
  :in [:push/grid]
  :out :push/grid
  :f (fn [_ half-g] (grid/make-symmetric-horizontal half-g))})

(register-instruction
 {:sym-name 'grid_make_symmetric_vertical
  :in [:push/grid]
  :out :push/grid
  :f (fn [_ half-g] (grid/make-symmetric-vertical half-g))})

(register-instruction
 {:sym-name 'grid_make_symmetric_4fold
  :in [:push/grid]
  :out :push/grid
  :f (fn [_ quarter-g] (grid/make-symmetric-4fold quarter-g))})

(register-instruction
 {:sym-name 'grid_all_rotations
  :in [:push/grid]
  :out :push/code
  :f (fn [_ g] (grid/all-rotations g))})

(register-instruction
 {:sym-name 'grid_all_symmetries
  :in [:push/grid]
  :out :push/code
  :f (fn [_ g] (grid/all-symmetries g))})

(register-instruction
 {:sym-name 'grid_resize_center
  :in [:push/grid :push/integer :push/integer]
  :out :push/grid
  :f (fn [_ g new-height new-width]
       (grid/resize-grid-center g new-height new-width))})

(register-instruction
 {:sym-name 'grid_resize_center_bg
  :in [:push/grid :push/integer :push/integer :push/integer]
  :out :push/grid
  :f (fn [_ g new-height new-width bg-color]
       (grid/resize-grid-center g new-height new-width bg-color))})

(register-instruction
 {:sym-name 'grid_resize_top_left
  :in [:push/grid :push/integer :push/integer]
  :out :push/grid
  :f (fn [_ g new-height new-width]
       (grid/resize-grid-top-left g new-height new-width))})

(register-instruction
 {:sym-name 'grid_resize_top_left_bg
  :in [:push/grid :push/integer :push/integer :push/integer]
  :out :push/grid
  :f (fn [_ g new-height new-width bg-color]
       (grid/resize-grid-top-left g new-height new-width bg-color))})

(register-instruction
  {:in
   [[:push/vector :push/integer]
    [:push/vector :push/integer]
    [:push/vector :push/integer]

    :push/integer :push/integer
    :push/integer :push/float]
   :f (fn [_
           kernel-c
           kernel-b
           kernel-a

           mask-color
           new-color
           old-color
           threshold]
        (when (every? (comp #(= % 3) count)
                      [kernel-a kernel-b kernel-c])
          (grid/write-rule {:kernel [kernel-a kernel-b kernel-c]
                            :mask-color mask-color
                            :new-color new-color
                            :old-color old-color
                            :threshold threshold})))
   :out :push/ca-rule
   :sym-name 'grid_make_ca})

(register-instruction
 {:sym-name 'grid_ca_apply_rule
  :in [:push/grid :push/ca-rule]
  :out :push/grid
  :f (fn [_ g rule]
       (def g g )
       (def rule rule)
       (grid/ca-apply-rule g rule))})

(register-instruction
 {:sym-name 'grid_reduce_ca
  :in [:push/grid :push/code :push/integer]
  :out :push/grid
  :f (fn [_ g rules steps] (grid/reduce-ca g rules steps))})

(register-instruction
 {:sym-name 'ca_rule_rand_gene
  :in []
  :out :push/ca_rule
  :f (fn [_] (grid/rand-gene))})

(register-instruction
 {:sym-name 'grid_shape
  :in [:push/grid]
  :out :push/code
  ;; returns a sequence, code stack is ok.
  :f (fn [_ g] (grid/shape g))})

(register-instruction
 {:sym-name 'grid_get_cell
  :in [:push/grid :push/integer :push/integer]
  :out :push/integer
  :f (fn [_ g x y]
       (grid/get-cell g x y))})

(register-instruction
 {:sym-name 'grid_set_cell
  :in [:push/grid :push/integer :push/integer :push/integer]
  :out :push/grid
  :f (fn [_ g x y color]
       (grid/set-cell g x y color))})

(register-instruction
 {:sym-name 'grid_from_code
  :in [:push/code]
  :out :push/grid
  :f (fn [_ values] (grid/grid values))})

(register-instruction
 {:sym-name 'grid_count_color
  :in [:push/grid :push/integer]
  :out :push/integer
  :f (fn [_ g color] (grid/count-color g color))})

;; (register-instruction
;;  {:sym-name 'grid_repeat_diagonal
;;   :in [:push/grid :push/integer :push/integer]
;;   :out :push/grid
;;   :f (fn [_ g window-size num-reps]
;;        (grid/repeat-pattern-diagonal g window-size num-reps))
;;   :doc "Repeat a pattern from top-left corner diagonally. Takes grid, window size, and number of repetitions."})

;; (register-instruction
;;  {:sym-name 'grid_repeat_horizontal
;;   :in [:push/grid :push/integer]
;;   :out :push/grid
;;   :f (fn [_ g num-reps]
;;        (grid/repeat-pattern-horizontal g num-reps))
;;   :doc "Repeat the 3x3 pattern horizontally n times."})

(register-instruction
 {:sym-name 'grid_count_non_zero
  :in [:push/grid]
  :out :push/integer
  :f (fn [_ g] (grid/grid-count-non-zero g))
  :doc "Count the number of non-zero cells in the grid."})

(register-instruction
 {:sym-name 'grid_color_frequencies
  :in [:push/grid]
  :out [:push/vector :push/integer]
  :f (fn [_ g]
       (grid/color-frequencies g))})

(register-instruction
 {:sym-name 'grid_max_color
  :in [:push/grid]
  :out :push/integer
  :f (fn [_ g] (grid/max-color g))})

(register-instruction
 {:sym-name 'grid_max_color_nonzero
  :in [:push/grid]
  :out :push/integer
  :f (fn [_ g] (grid/max-color-non-zero g))})

(register-instruction
 {:sym-name 'grid_positions_of_color
  :in [:push/grid :push/integer]
  :out :push/code
  :f (fn [_ g color] (grid/positions-of-color g color))})

(register-instruction
 {:sym-name 'grid_positions_of_max_color
  :in [:push/grid]
  :out :push/code
  :f (fn [_ g] (grid/positions-of-max-color g))})

(register-instruction
 {:sym-name 'grid_max_pool
  :in [:push/grid]
  :out :push/grid
  :f (fn [_ g]
       (grid/max-pool g))})

;; ----------------------------

(register-instruction
 {:sym-name 'grid_pack
  :in [:push/grid :push/grid :push/integer]
  :out :push/grid
  :f (fn [_ g p n]
       (grid/pack g p n))})

(register-instruction
 {:sym-name 'color_gensym
  :in []
  :out :state
  :f (fn [state]
       ;; color gensym counter is for 1 program execution,
       ;; this way parts of the program can refer to the same color
       (let [i (::color-gensym-counter state (count grid/colors))]
         (-> state
             (assoc ::color-gensym-counter i)
             (stack/push :push/integer i))))})

;; --------------------

(doseq [[sym-name impl]
        [['grid_stackdepth impl/stack-depth-impl]
         ['grid_pop impl/popper]
         ['grid_dup impl/dupper]
         ['grid_flush impl/flusher]
         ['grid_shove impl/shover]
         ['grid_yank impl/yanker]
         ['grid_yankdup impl/yankdupper]
         ['grid_rot impl/rotater]
         ['grid_swap impl/swapper]
         ['grid_define impl/definer]]]
  (register-instruction
   (impl {:sym-name sym-name
          :push-type :push/grid})))
