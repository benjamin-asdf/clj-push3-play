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
 {:sym-name 'grid_ca_apply_rule
  :in [:push/grid :push/ca-rule]
  :out :push/grid
  :f (fn [_ g rule] (grid/ca-apply-rule g rule))})

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

(register-instruction
 {:sym-name 'grid_color_frequencies
  :in [:push/grid]
  :out :push/code
  :f (fn [_ g]
       (map vec (grid/color-frequencies g)))})

(register-instruction
 {:sym-name 'grid_max_color
  :in [:push/grid]
  :out :push/integer
  :f (fn [_ g] (grid/max-color g))})

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
