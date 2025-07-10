(ns benjamin-schwerdtner.clj-push3-next.instructions.exec
  (:require
   [benjamin-schwerdtner.clj-push3-next.instructions.impl :refer
    [register-instruction] :as impl]
   [benjamin-schwerdtner.clj-push3-next.pushstate :as stack]))

(register-instruction
 {:sym-name 'exec_=
  :in [:push/exec :push/exec]
  :out :push/boolean
  :f (fn [_ a b] (= a b))})


;; =====================

;; Very similar to code.do*range, but operates on the EXEC stack.

(defn do-range
  ""
  [state code-to-do current-index destination-index]
  (let [increment (cond (< current-index destination-index) 1
                        (> current-index destination-index) -1
                        :else 0)
        continuation (if (zero? increment)
                       state
                       (stack/push
                        state
                        :push/exec
                        (list (+ current-index increment)
                              destination-index
                              ;; this is flipped from code?
                              'exec_do*range
                              code-to-do
                              )))]
    (->
     continuation
     ;; push the current index
     (stack/push :push/integer current-index)
     ;; push the to-do
     (stack/push :push/exec code-to-do))))

;; do count

(defn do-count
  ""
  [state integer code]
  (if (<= integer 0)
    state
    (-> state
        (stack/push
         :push/exec
         (list 0 (dec integer) code 'exec_do*range code)))))


(defn do-times
  "Same as [[do-count]],

  but add a integer-pop to the front of the to-do code,
  so that the index argument is not tracked.
  "
  [state integer code]
  (if (<= integer 0)
    state
    (let [code
          (conj (coerce-list code) 'integer_pop)]
      (->
       state
       (stack/push
        :push/exec
        (list 0 (dec integer) code 'exec_do*range))))))



(register-instruction
 {:sym-name 'exec_do*range
  :in [:push/exec :push/integer :push/integer]
  :out :state
  :f do-range
  :doc "CODE.DO*RANGE: An iteration instruction that executes the top item on the CODE stack a number of times that depends on the top two integers,
  while also pushing the loop counter onto the INTEGER stack for possible access during the execution of the body of the loop.

  The top integer is the \"destination index\" and the second integer is the \"current index.\""})

(register-instruction
 {:sym-name 'exec_do*count
  :in [:push/integer :push/exec]
  :out :state
  :f do-count
  :doc "Execute the code at the top of exec n times.
Where n is the top item of the Integer stack.

This expands to a







"})

(register-instruction
 {:f do-times
  :in [:push/integer :push/exec]
  :out :state
  :sym-name 'exec_do*times})


;; EXEC.IF: If the top item of the BOOLEAN stack is TRUE then this removes the second item on the EXEC stack, leaving the first item to be executed. If it is false then it removes the first item, leaving the second to be executed. This is similar to CODE.IF except that it operates on the EXEC stack. This acts as a NOOP unless there are at least two items on the EXEC stack and one item on the BOOLEAN stack.


(register-instruction
 {:sym-name 'exec_if
  :in [:push/boolean :push/exec :push/exec]
  :out :push/exec
  :doc "If the top item of the BOOLEAN stack is TRUE this recursively executes the second item of the CODE stack;
otherwise it recursively executes the first item of the CODE stack."
  :f (fn [_ condition alternative consequent]
       (if condition
         consequent
         alternative))})


;; EXEC.K: The Push implementation of the "K combinator". Removes the second item on the EXEC stack.

(register-instruction
 {:sym-name 'exec_k
  :in [:push/exec :push/exec]
  :out :push/exec
  :f (fn [_ a b] a)
  :doc "EXEC.K: The Push implementation of the \"K combinator\". Removes the second item on the EXEC stack."})


;; EXEC.S: The Push implementation of the "S combinator". Pops 3 items from the EXEC stack, which we will call A, B, and C (with A being the first one popped). Then pushes a list containing B and C back onto the EXEC stack, followed by another instance of C, followed by another instance of A.








;; EXEC.SHOVE: Inserts the top EXEC item "deep" in the stack, at the position indexed by the top INTEGER. This may be thought of as a "DO LATER" instruction.
;; EXEC.STACKDEPTH: Pushes the stack depth onto the INTEGER stack.
;; EXEC.SWAP: Swaps the top two items on the EXEC stack.
;; EXEC.Y: The Push implementation of the "Y combinator". Inserts beneath the top item of the EXEC stack a new item of the form "( EXEC.Y <TopItem> )".
;; EXEC.YANK: Removes an indexed item from "deep" in the stack and pushes it on top of the stack. The index is taken from the INTEGER stack. This may be thought of as a "DO SOONER" instruction.
;; EXEC.YANKDUP: Pushes a copy of an indexed item "deep" in the stack onto the top of the stack, without removing the deep item. The index is taken from the INTEGER stack.

















;; EXEC.DEFINE: Defines the name on top of the NAME stack as an instruction that will push the top item of the EXEC stack back onto the EXEC stack.

;; EXEC.DUP: Duplicates the top item on the EXEC stack. Does not pop its argument (which, if it did, would negate the effect of the duplication!). This may be thought of as a "DO TWICE" instruction.
;; EXEC.FLUSH: Empties the EXEC stack. This may be thought of as a "HALT" instruction.

;; EXEC.POP: Pops the EXEC stack. This may be thought of as a "DONT" instruction.


;; EXEC.ROT: Rotates the top three items on the EXEC stack, pulling the third item out and pushing it on top. This is equivalent to "2 EXEC.YANK".



;; Stack manipulation instructions
(doseq [[sym-name impl]
        [['exec_stackdepth impl/stack-depth-impl]
         ['exec_pop impl/popper]
         ['exec_dup impl/dupper]
         ['exec_flush impl/flusher]
         ['exec_shove impl/shover]
         ['exec_yank impl/yanker]
         ['exec_yankdup impl/yankdupper]
         ['exec_rot impl/rotater]
         ['exec_swap impl/swapper]
         ['exec_define impl/definer]]]
  (register-instruction
   (impl {:sym-name sym-name
          :push-type :push/exec})))
