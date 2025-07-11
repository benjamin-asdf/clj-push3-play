(ns benjamin-schwerdtner.clj-push3-play.interpreter-concepts)

;; -
;; https://faculty.hampshire.edu/lspector/push3-description.html
;; -



;;
;; Core Concepts:
;;
;;
;; 1. Evaluation is stack based
;; 2. There is a stack for each *data type*
;; 3. There is a CODE datatype for meta programming inside push itself
;;
;; ## Instructions
;;
;; - Take the arguments from the stack and leave the rest
;; - leave the results on the stack of the type
;;
;; - Push is *stack safe*: if the required arguments are not available,
;;   the instruction becomes a NOOP.
;;   (This is a good choice for a software synthesis target language).
;;

;; ## Syntax

;;   program ::= instruction | literal | (program**)

;; - an instruction is a Push program
;; - a literal is a Push program
;; - a pranthesized sequence of zero or more Push programs is a Push program

;;
;; ## Stacks
;;
;; - typed stacks,
;; - exec stack, boolean_stack, integer_stack etc.
;;


;; ## Err on the side of doing nothing

;; - any syntactical PUSH program is expected to run without errors,
;; - internal subroutines cover all expected cases like division by zero,
;; - NOOP in that case
;;
;; ## Limits
;;
;; - EVALPUSH-LIMIT: the maximum allowed number of 'executions' in a single top-level call to the interpreter.
;; - MAX-POINTS-IN-PROGRAM: The maximum size of an item on the CODE stack.
;;   a point is an instruction, a literal, or a pair of paranteheses
;;

;; prior to Push3:

;; - using the host recursion support

#_(defn execute [state program]
    (cond
      (single-instruction? program)
      (execute-single state program)

      (literal? program)
      (push-to-stack state program)

      ;; P is a list
      :else
      (reduce (fn [state p] (execute state p)) program)))


(defprotocol PushValue
  (typeof-item [this]))

(extend-protocol PushValue
  java.lang.String
  (typeof-item [this] :push-type/string)
  java.lang.Boolean
  (typeof-item [this] :push-type/boolean)
  java.lang.Integer
  (typeof-item [this] :push-type/integer))



;; Push3

;; - using the Push EXEC stack for recursion, inside Push

(defn push-to-exec-stack [state p]
  (update state :exec-stack conj p))

(defn single-instruction? [p])

(defn execute-single [state p])

(defn literal? [p])

(defn typeof-item [item])

(def stacks
  {:push-type/boolean :boolean-stack
   :push-type/code :code-stack
   :push-type/float :float-stack
   ;; long?
   :push-type/integer :integer-stack
   :push-type/name :name-stack
   :push-type/string :string-stack})

(defn push-to-stack [state item]
  (update state (stacks (typeof-item item)) conj item))

(defn execute
  [state program]
  (let [state (push-to-exec-stack state program)]
    (loop [state state]
      (if (empty? (:exec-stack state))
        state
        (recur (let [item (peek (:exec-stack state))
                     state (update state :exec-stack pop)]
                 (cond (single-instruction? item)
                         (execute-single state item)
                       (literal? item) (push-to-stack state item)
                       ;; item is a list
                       :else (reduce (fn [state p]
                                       (push-to-exec-stack state p))
                               state
                               ;; reverse, so the first item in
                               ;; the list is the first exec
                               ;; instruction
                               (reverse item)))))))))



;; ## NAME
;; - a symbolic datatype that can be bound to values with DEFINE instructions
;; - any identifier that do not represent known Push instructions or literals
;;   of other type (e.g. TRUE and FALSE) are recognized as NAMES.
;; - if the name was not encountered before, it is pushed to the name stack
;; - DEFINE: like INTEGER.DEFINE or CODE.DEFINE, takes the top name and designated stack
;;   and binds name to value
;; - thenceforth, encountering the name will push the bound value on the EXEC stack
;; - it will usually end up on the respective stack,
;;   but if it is code, it results in this code execution
;;

;; ### NAME.QUOTE instruction
;;
;; - move names that have definitions onto the NAME stack
;; - presumably for the sake of re-definition.
;; - name.quote sets a flag that causes the next encountred name to be
;;   pushed onto the name stack, whether or not ith has previously been bound.

;; ### CODE.QUOTE

;; - similar to NAME.QUOTE
;; - with the exec stack concept, this can be implemented by
;;   simply pushing the code onto the CODE stack
;; - Push includes list manipulation and execution instructions like CODE.DO, CODE.DO*TIMES
