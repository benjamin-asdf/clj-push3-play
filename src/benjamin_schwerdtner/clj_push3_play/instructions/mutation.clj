(ns benjamin-schwerdtner.clj-push3-play.instructions.mutation
  (:require
   [benjamin-schwerdtner.clj-push3-play.instructions.impl :refer
    [register-instruction] :as impl]
   [benjamin-schwerdtner.clj-push3-play.stack.pushstate :as stack]
   [benjamin-schwerdtner.clj-push3-play.util :as util :refer [ensure-list]]
   [benjamin-schwerdtner.clj-push3-play.generator :as gen]))

;; Supposed to be the primitive instructions of Push mutation operators.

;; ---------------------------------------------------------------------------------------
;;
;; Mutation design:
;;
;;
;;
;; mutate(mutator-program, input-program, random-seed) -> variations
;;
;;
;; - `input-program` is a PUSH program.
;; - `mutator-program` is a PUSH program.
;; - `random-seed` is a number used for deterministic randomness.
;;
;;
;; mutate:
;;
;; 0. setup Push interpreter with `mutator-program` on the EXEC stack.
;; 1. Push `input-program` as list on the CODE stack.
;; 2. Set `random-seed` as special state for program execution.
;; 3. Execute program (with limits, etc.)
;; 4. Filter the CODE stack for elements of type list, called programs.
;; 5. Each such program counts as 1 potential VARIATION of `input-program`.
;;
;; - proceed by using `n-variations` variations in your EA.
;;
;; Optionally: they are sorted?
;;
;;
;; Future:
;; - Setup some kind of epigenetic register.
;; - translate Push programs in high dimensional vectors and back,
;;   allow 'neuronal' mutation on subsymbolic representations, + have a door into deep learning etc.
;;
;;
;;
;;
;;
;; ---------------------------------------------------------------------------------------


;;
;; Some design thought processes:
;;
;;
;; first, rest, drop, take
;; duplication of CODE instructions?
;;
;; solution #1: Simply use the code stack for mutator programs?
;; then the CODE instructions are already there

;; solution #2: Duplicate code instructions, whatever.
;; --------------

;; idea #1:
;; - input program goes on code stack
;;   as list?
;;   spread out?
;; - output is the content of code stack, or the top item?
;;


;; idea #2:

;; - intruduce a program input register,
;; - (an instruction MUT_INPUT, that puts the input program on the code stack) ?


;; idea #3:

;; - separate PROGRAM stack
;; - then programs that evolved to manipulate CODE don't work?


;; idea #4:

;; - do the most silly thing imaginable
;; - transform the program into bit encoded string, put it on the integer stack


;; idea #5:

;; - use PUSHP instructions (in spirit)
;; - NEIGHBOR, ELDER, OTHER, OTHER-TAG : They depend on the outer loop structure though.

;; idea #6:
;;
;; - input to the mutator program is the complete mutation program population, with weights representing something like copy numbers
;; -


;; --------------
;; #1 and #2 can be combinend

(register-instruction
 {:sym-name 'mut_duplicate
  :in [:push/code]
  :out :push/code
  ;; actually the same as
  ;; code_dup code_list
  :f
  (fn [_ a]
    (if (list? a)
      (apply list (concat a a))
      (list a a)))})


;; maybe the program just needs to discover
;; length and drop

#_(register-instruction
   {:sym-name 'mut_half
    :in [:push/code]
    :out :push/code
    :f
    (fn [_ code]
      (let [code (ensure-list code)]
        (apply list (take (long (/ (count code) 2)) code))))})


#_(comment
  (let [code (list 1 2 3 4)]
    (let [code (ensure-list code)]
      (apply list (take (long (/ (count code) 2)) code)))))



;; https://en.wikipedia.org/wiki/Mutation_(evolutionary_algorithm)#Mutation_of_permutations
;; (in spirit, not letter)
;;

(defn rotate
  [xs n]
  (if (or (zero? n) (empty? xs))
    xs
    ;; implement periodic boundary condition
    (let [n (mod n (count xs))]
      (take (count xs) (drop n (cycle xs))))))

(comment
  (take 50 (cycle (list 1 2 3)))
  (rotate [1 2 3] 1)
  (rotate [1 2 3] 3)
  (rotate [1 2 3] 2)
  (rotate [1 2 3] 0))

(register-instruction
 {:sym-name 'mut_rotate
  :in [:push/code :push/integer]
  :out :push/code
  :f
  (fn [_ code n]
    (apply list (rotate (ensure-list code) n)))})

(defn inverse
  [xs start len]
  (if (empty? xs)
    xs
    (let [xsc (cycle xs)
          cnt (count xs)
          start (mod start cnt)
          len (min cnt len)]
      (take cnt
            (concat (take start xsc)
                    (reverse (take len (drop start xsc)))
                    (drop (+ start len) xsc))))))

(comment
  (inverse [:a :b :c :d :e] 1 2)
  (inverse [:a :b :c :d :e] 1 5)
  ;; kinda has a logic, from the boundary condition
  '(:a :a :e :d :c)

  (=
   (reverse [:a :b :c :d :e])
   (inverse [:a :b :c :d :e] 5 5))
  true

  (inverse [:a :b :c :d :e] 0 2)
  '(:b :a :c :d :e))

(register-instruction
 {:sym-name 'mut_inverse
  :in [:push/code :push/integer :push/integer]
  :out :push/code
  :f
  (fn [_ code start len]
    (apply list (inverse (ensure-list code) start len)))})




;; -------------------------
