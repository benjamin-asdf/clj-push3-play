(ns benjamin-schwerdtner.clj-push3-next.instructions.numbers
  (:require
   [benjamin-schwerdtner.clj-push3-next.instructions.impl :refer
    [register-instruction] :as impl]
   [benjamin-schwerdtner.clj-push3-next.pushstate :as stack]))

;; Arithmetic instructions

(defn modder [{:keys [sym-name push-type]}]
  {:doc "Pushes the second stack item modulo the top stack item"
   :f (fn [state]
        ;; f the top item is zero this acts as a NOOP
        (if (or (empty? (-> state
                            :stacks
                            push-type))
                (zero? (stack/peek-item state push-type)))
          state
          (let [a (stack/stack-nth state push-type 0)
                b (stack/stack-nth state push-type 1)]
            (-> state
                (stack/pop-n push-type 2)
                (stack/push push-type (mod b a))))))
   :in []
   :out :state
   :sym-name sym-name})

(defn multiplier [{:keys [sym-name push-type]}]
  {:doc "Pushes the second stack item multiplied by the top stack item"
   :f (fn [_state a b]
        (* a b))
   :in [push-type push-type]
   :out push-type
   :sym-name sym-name})

(defn adder [{:keys [sym-name push-type]}]
  {:doc "Pushes the sum of the top two stack items"
   :f (fn [_state a b]
        (+ a b))
   :in [push-type push-type]
   :out push-type
   :sym-name sym-name})

(defn subtractor [{:keys [sym-name push-type]}]
  {:doc "Pushes the difference of the top two stack items"
   :f (fn [_state a b]
        (- a b))
   :in [push-type push-type]
   :out push-type
   :sym-name sym-name})

(defn divider
  [{:keys [sym-name push-type]}]
  {:doc "Pushes the second stack item divided by the top stack item"
   :f (fn [state]
        ;; f the top item is zero this acts as a NOOP
        (if (or (empty? (-> state
                            :stacks
                            push-type))
                (zero? (stack/peek-item state push-type)))
          state
          (let [a (stack/stack-nth state push-type 0)
                b (stack/stack-nth state push-type 1)]
            (-> state
                (stack/pop-n push-type 2)
                (stack/push push-type (/ b a))))))
   :in []
   :out :state
   :sym-name sym-name})

;; -----------
;; Comparison instructions

(defn lt-er [{:keys [sym-name push-type]}]
  {:doc "Pushes true if the second stack item is less than the top stack item"
   :f (fn [_state b a]
        (< a b))
   :in [push-type push-type]
   :out :push/boolean
   :sym-name sym-name})

(defn eq-er
  [{:keys [sym-name push-type]}]
  {:doc
   "Pushes true if the second stack item is equal to the top stack item"
   :f (fn [_state a b] (= a b))
   :in [push-type push-type]
   :out :push/boolean
   :sym-name sym-name})

(defn gt-er
  [{:keys [sym-name push-type]}]
  {:doc
   "Pushes true if the second stack item is greater than the top stack item"
   :f (fn [_state b a] (> a b))
   :in [push-type push-type]
   :out :push/boolean
   :sym-name sym-name})

;; Min/Max instructions

(defn minner [{:keys [sym-name push-type]}]
  {:doc "Pushes the minimum of the top two stack items"
   :f (fn [_state a b] (min a b))
   :in [push-type push-type]
   :out push-type
   :sym-name sym-name})

(defn maxxer
  [{:keys [sym-name push-type]}]
  {:doc "Pushes the maximum of the top two stack items"
   :f (fn [_state a b] (max a b))
   :in [push-type push-type]
   :out push-type
   :sym-name sym-name})

;; RENAMED:
;; integer_% -> integer_mod
;; integer_/ -> integer_div

(doseq [[sym-name impl]
        [['integer_mod modder]
         ['integer_* multiplier]
         ['integer_+ adder]
         ['integer_- subtractor]
         ['integer_div divider]
         ;; --
         ['integer_< lt-er]
         ['integer_> gt-er]
         ['integer_= eq-er]
         ;; --
         ['integer_min minner]
         ['integer_max maxxer]]]

  (register-instruction
   (impl {:push-type :push/integer
          :sym-name sym-name})))

;; -----------------------------------

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

;; -----------------------------------

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

;; ==================================

;; Float instructions

(doseq [[sym-name impl]
        [['float_mod modder]
         ['float_* multiplier]
         ['float_+ adder]
         ['float_- subtractor]
         ['float_div divider]
         ;; --
         ['float_< lt-er]
         ['float_> gt-er]
         ['float_= eq-er]
         ;; --
         ['float_min minner]
         ['float_max maxxer]]]
  (register-instruction
   (impl {:push-type :push/float
          :sym-name sym-name})))

;; -----------------------------------

;; Float type conversion instructions

(register-instruction
 {:sym-name 'float_fromboolean
  :in [:push/boolean]
  :out :push/float
  :f (fn [_ bool] (if bool 1.0 0.0))})

(register-instruction
 {:sym-name 'float_frominteger
  :in [:push/integer]
  :out :push/float
  :f (fn [_ x] (double x))})

;; Float random instruction

(register-instruction
 {:sym-name 'float_rand
  :in []
  :out :push/float
  :f (fn [state]
       (let [min-val (get-in state [:parameters :min-random-float] -1.0)
             max-val (get-in state [:parameters :max-random-float] 1.0)]
         (+ min-val (* (rand) (- max-val min-val)))))})

;; -----------------------------------

;; Float trigonometric instructions

(register-instruction
 {:sym-name 'float_cos
  :in [:push/float]
  :out :push/float
  :f (fn [_ x] (Math/cos x))})

(register-instruction
 {:sym-name 'float_sin
  :in [:push/float]
  :out :push/float
  :f (fn [_ x] (Math/sin x))})

(register-instruction
 {:sym-name 'float_tan
  :in [:push/float]
  :out :push/float
  :f (fn [_ x] (Math/tan x))})

;; -----------------------------------

;; Float stack manipulation instructions using impl generators

(doseq [[sym-name impl]
        [['float_stackdepth impl/stack-depth-impl]
         ['float_pop impl/popper]
         ['float_dup impl/dupper]
         ['float_flush impl/flusher]
         ['float_shove impl/shover]
         ['float_yank impl/yanker]
         ['float_yankdup impl/yankdupper]
         ['float_rot impl/rotater]
         ['float_swap impl/swapper]
         ['float_define impl/definer]]]
  (register-instruction
   (impl {:sym-name sym-name
          :push-type :push/float})))
