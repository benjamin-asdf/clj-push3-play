(ns benjamin-schwerdtner.clj-push3-play.mutation.mut
  (:require
   [benjamin-schwerdtner.clj-push3-play.interpreter :as push]
   [benjamin-schwerdtner.clj-push3-play.stack.pushstate :as stack]))

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

(defn mutate
  [input-program mutator-program]
  ;; seed todo

  ;; return the code stack, mut could chose to only output useful dat on the top item,
  ;; depends on the user.
  (into []
        ;; reverse the stack
        ;; throw away atoms
        (reverse (filter list?
                         (->
                          (let [outcome (push/execute
                                         (-> (push/setup-state)
                                             (stack/push :push/code
                                                         input-program))
                                         mutator-program
                                         {:max-executions (long 1e6)})]
                            (-> outcome :stacks :push/code))))))


  ;; ------- read-write future: epigenetic-data -------
  )
