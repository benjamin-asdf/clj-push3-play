(ns benjamin-schwerdtner.clj-push3-next.instructions.code-impl
  (:require [benjamin-schwerdtner.clj-push3-next.pushstate :as stack]))

(defn coerce-list [o]
  (if-not (list? o) (list o) o))

;; Reference implementation from Clojush
;; clojush/code.clj


(defn do-range
  "CODE.DO*RANGE: An iteration instruction that executes the top item on the CODE stack a number of times that depends on the top two integers,
  while also pushing the loop counter onto the INTEGER stack for possible access during the execution of the body of the loop.


  The top integer is the \"destination index\" and the second integer is the \"current index.\"
  First the code and the integer arguments are saved locally and popped. Then the integers are compared.

  If the integers are equal then the current index is pushed onto the INTEGER stack and the code (which is the \"body\" of the loop) is pushed onto the EXEC stack for subsequent execution.

  If the integers are not equal then the current index will still be pushed onto the INTEGER stack but two items will be pushed onto the EXEC stack -- first a recursive call to CODE.DO*RANGE (with the same code and destination index, but with a current index that has been either incremented or decremented by 1 to be closer to the destination index) and then the body code. Note that the range is inclusive of both endpoints; a call with integer arguments 3 and 5 will cause its body to be executed 3 times, with the loop counter having the values 3, 4, and 5. Note also that one can specify a loop that \"counts down\" by providing a destination index that is less than the specified current index.


  Executes the code to do at leas once.

  "
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
                              'code_quote
                              code-to-do
                              'code_do*range)))]

    (->
     continuation
     ;; push the current index
     (stack/push :push/integer current-index)
     ;; push the to-do
     (stack/push :push/exec code-to-do))))

;; do count

(defn do-count
  "CODE.DO*COUNT: An iteration instruction that performs a loop (the body of which is taken from the CODE stack)

  the number of times indicated by the INTEGER argument,

  Pushing an index (which runs from zero to one less than the number of iterations) onto the INTEGER stack prior to each execution of the loop body.

  This should be implemented as a macro that expands into a call to CODE.DO*RANGE.
  CODE.DO*COUNT takes a single INTEGER argument (the number of times that the loop will be executed) and a single CODE argument (the body of the loop).


  If the provided INTEGER argument is negative or zero then this becomes a NOOP. Otherwise it expands into:

  ( 0 <1 - IntegerArg> CODE.QUOTE <CodeArg> CODE.DO*RANGE )

"
  [state integer code]
  (if (<= integer 0)
    state
    (-> state
        (stack/push
          :push/exec
          (list 0 (dec integer) 'code_quote code 'code_do*range)))))


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
        (list 0 (dec integer) 'code_quote code 'code_do*range))))))









#_(defn if-impl
    [_ condition alternative consequent]
    (if condition consequent alternative))
