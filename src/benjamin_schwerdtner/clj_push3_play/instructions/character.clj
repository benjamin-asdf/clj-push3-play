(ns benjamin-schwerdtner.clj-push3-play.instructions.character
  (:require
   [benjamin-schwerdtner.clj-push3-play.instructions.impl :refer
    [register-instruction] :as impl]
   [benjamin-schwerdtner.clj-push3-play.generator :as gen]
   [benjamin-schwerdtner.clj-push3-play.stack.pushstate :as stack]
   [random-seed.core :as rand])
  (:refer-clojure :exclude [rand rand-int]))

;; Character predicates

(register-instruction
 {:sym-name 'char_is_letter
  :in [:push/char]
  :out :push/boolean
  :f (fn [_ c] (Character/isLetter c))})

(register-instruction
 {:sym-name 'char_is_digit
  :in [:push/char]
  :out :push/boolean
  :f (fn [_ c] (Character/isDigit c))})

(register-instruction
 {:sym-name 'char_is_whitespace
  :in [:push/char]
  :out :push/boolean
  :f (fn [_ c] (Character/isWhitespace c))})

(register-instruction
 {:sym-name 'char_is_uppercase
  :in [:push/char]
  :out :push/boolean
  :f (fn [_ c] (Character/isUpperCase c))})

(register-instruction
 {:sym-name 'char_is_lowercase
  :in [:push/char]
  :out :push/boolean
  :f (fn [_ c] (Character/isLowerCase c))})

;; Character conversion

(register-instruction
 {:sym-name 'char_from_integer
  :in [:push/integer]
  :out :push/char
  :f (fn [_ i] (char (mod i 128)))})

(register-instruction
 {:sym-name 'char_from_float
  :in [:push/float]
  :out :push/char
  :f (fn [_ f] (char (mod (long f) 128)))})

(register-instruction
 {:sym-name 'char_to_integer
  :in [:push/char]
  :out :push/integer
  :f (fn [_ c] (int c))})

(register-instruction
 {:sym-name 'char_to_uppercase
  :in [:push/char]
  :out :push/char
  :f (fn [_ c] (Character/toUpperCase c))})

(register-instruction
 {:sym-name 'char_to_lowercase
  :in [:push/char]
  :out :push/char
  :f (fn [_ c] (Character/toLowerCase c))})

;; String operations

(register-instruction
 {:sym-name 'char_all_from_string
  :in [:push/string]
  :out :state
  :f (fn [state s]
       (reduce (fn [st ch] (stack/push st :push/char ch))
               state
               (reverse s)))})

(register-instruction
 {:sym-name 'char_from_string
  :in [:push/string :push/integer]
  :out :push/char
  :f (fn [_ s i]
       (if (and (not (empty? s))
                (<= 0 i (dec (count s))))
         (nth s i)
         \space))})

;; Comparison

(register-instruction
 {:sym-name 'char_=
  :in [:push/char :push/char]
  :out :push/boolean
  :f (fn [_ a b] (= a b))})

(register-instruction
 {:sym-name 'char_<
  :in [:push/char :push/char]
  :out :push/boolean
  :f (fn [_ a b] (< (int a) (int b)))})

(register-instruction
 {:sym-name 'char_>
  :in [:push/char :push/char]
  :out :push/boolean
  :f (fn [_ a b] (> (int a) (int b)))})

;; Random generation

(register-instruction
 {:sym-name 'char_rand
  :in []
  :out :push/char
  :f (fn [_] (char (rand/rand-int 128)))})

(register-instruction
 {:sym-name 'char_rand_letter
  :in []
  :out :push/char
  :f (fn [_]
       (let [letters (vec "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")]
         (rand/rand-nth letters)))})

(register-instruction
 {:sym-name 'char_rand_digit
  :in []
  :out :push/char
  :f (fn [_] (char (+ 48 (rand/rand-int 10))))})

;; Stack manipulation (standard for all types)

(impl/dupper {:sym-name 'char_dup :push-type :push/char})
(impl/popper {:sym-name 'char_pop :push-type :push/char})
(impl/swapper {:sym-name 'char_swap :push-type :push/char})
(impl/rotater {:sym-name 'char_rot :push-type :push/char})
(impl/flusher {:sym-name 'char_flush :push-type :push/char})
(impl/yanker {:sym-name 'char_yank :push-type :push/char})
(impl/yankdupper {:sym-name 'char_yankdup :push-type :push/char})
(impl/shover {:sym-name 'char_shove :push-type :push/char})
(impl/stack-depth-impl {:sym-name 'char_stackdepth :push-type :push/char})
