(ns benjamin-schwerdtner.clj-push3-play.instructions.boolean
  (:require
   [benjamin-schwerdtner.clj-push3-play.instructions.impl :refer
    [register-instruction] :as impl]
   ;; [benjamin-schwerdtner.clj-push3-play.stack.pushstate :as stack]
   ))

;; Boolean logic instructions

(register-instruction
 {:sym-name 'boolean_and
  :in [:push/boolean :push/boolean]
  :out :push/boolean
  :f (fn [_ a b] (and a b))})

(register-instruction
 {:sym-name 'boolean_or
  :in [:push/boolean :push/boolean]
  :out :push/boolean
  :f (fn [_ a b] (or a b))})

(register-instruction
 {:sym-name 'boolean_not
  :in [:push/boolean]
  :out :push/boolean
  :f (fn [_ a] (not a))})

(register-instruction
 {:sym-name 'boolean_=
  :in [:push/boolean :push/boolean]
  :out :push/boolean
  :f (fn [_ a b] (= a b))})

;; Type conversion instructions

(register-instruction
 {:sym-name 'boolean_fromfloat
  :in [:push/float]
  :out :push/boolean
  :f (fn [_ x] (not (zero? x)))})

(register-instruction
 {:sym-name 'boolean_frominteger
  :in [:push/integer]
  :out :push/boolean
  :f (fn [_ x] (not (zero? x)))})

(register-instruction
 {:sym-name 'boolean_rand
  :in []
  :out :push/boolean
  :f (fn [_] (rand-nth [false true]))})

;; ---------------------------------

(doseq [[sym-name impl]
        [['boolean_stackdepth impl/stack-depth-impl]
         ['boolean_pop impl/popper]
         ['boolean_dup impl/dupper]
         ['boolean_flush impl/flusher]
         ['boolean_shove impl/shover]
         ['boolean_yank impl/yanker]
         ['boolean_yankdup impl/yankdupper]
         ['boolean_rot impl/rotater]
         ['boolean_swap impl/swapper]
         ['boolean_define impl/definer]]]
  (register-instruction
   (impl {:sym-name sym-name
          :push-type :push/boolean})))
