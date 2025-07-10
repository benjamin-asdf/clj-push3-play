(ns benjamin-schwerdtner.clj-push3-next.instructions.integer
  (:require
   [benjamin-schwerdtner.clj-push3-next.instructions.impl :refer
    [register-instruction] :as impl]
   [benjamin-schwerdtner.clj-push3-next.pushstate :as stack]))

;; Arithmetic instructions

(register-instruction
 {:sym-name 'integer_%
  :in [:push/integer :push/integer]
  :out :push/integer
  :f (fn [_ divisor dividend]
       (if (zero? divisor)
         dividend
         (mod dividend divisor)))})

(register-instruction
 {:sym-name 'integer_*
  :in [:push/integer :push/integer]
  :out :push/integer
  :f (fn [_ a b] (* a b))})

(register-instruction
 {:sym-name 'integer_+
  :in [:push/integer :push/integer]
  :out :push/integer
  :f (fn [_ a b] (+ a b))})

(register-instruction
 {:sym-name 'integer_-
  :in [:push/integer :push/integer]
  :out :push/integer
  :f (fn [_ b a] (- a b))})

(register-instruction
 {:sym-name (symbol "integer_/")
  :in [:push/integer :push/integer]
  :out :push/integer
  :f (fn [_ divisor dividend]
       (if (zero? divisor)
         dividend
         (quot dividend divisor)))})

;; Comparison instructions

(register-instruction
 {:sym-name 'integer_<
  :in [:push/integer :push/integer]
  :out :push/boolean
  :f (fn [_ b a] (< a b))})

(register-instruction
 {:sym-name 'integer_=
  :in [:push/integer :push/integer]
  :out :push/boolean
  :f (fn [_ a b] (= a b))})

(register-instruction
 {:sym-name 'integer_>
  :in [:push/integer :push/integer]
  :out :push/boolean
  :f (fn [_ b a] (> a b))})

;; Min/Max instructions

(register-instruction
 {:sym-name 'integer_max
  :in [:push/integer :push/integer]
  :out :push/integer
  :f (fn [_ a b] (max a b))})

(register-instruction
 {:sym-name 'integer_min
  :in [:push/integer :push/integer]
  :out :push/integer
  :f (fn [_ a b] (min a b))})

;; Type conversion instructions

(register-instruction
 {:sym-name 'integer_fromboolean
  :in [:push/boolean]
  :out :push/integer
  :f (fn [_ bool] (if bool 1 0))})

(register-instruction
 {:sym-name 'integer_fromfloat
  :in [:push/float]
  :out :push/integer
  :f (fn [_ x] (long x))})

;; Random instruction

(register-instruction
 {:sym-name 'integer_rand
  :in []
  :out :push/integer
  :f (fn [state]
       (let [min-val (get-in state [:parameters :min-random-integer] Integer/MIN_VALUE)
             max-val (get-in state [:parameters :max-random-integer] Integer/MAX_VALUE)]
         (+ min-val (rand-int (inc (- max-val min-val))))))})

;; Stack manipulation instructions using impl generators

(doseq [[sym-name impl]
        [['integer_stackdepth impl/stack-depth-impl]
         ['integer_pop impl/popper]
         ['integer_dup impl/dupper]
         ['integer_flush impl/flusher]
         ['integer_shove impl/shover]
         ['integer_yank impl/yanker]
         ['integer_yankdup impl/yankdupper]
         ['integer_rot impl/rotater]
         ['integer_swap impl/swapper]
         ['integer_define impl/definer]]]
  (register-instruction
   (impl {:sym-name sym-name
          :push-type :push/integer})))
