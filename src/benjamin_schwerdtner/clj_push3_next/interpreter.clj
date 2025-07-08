(ns benjamin-schwerdtner.clj-push3-next.interpreter
  (:require
    [benjamin-schwerdtner.clj-push3-next.pushstate :as stack]
    [benjamin-schwerdtner.clj-push3-next.prot :as prot]
    [benjamin-schwerdtner.clj-push3-next.instructions.interface :as
     instructions]))

;; -
;; https://faculty.hampshire.edu/lspector/push3-description.html
;; -

;; =============================
;; Push3
;; =============================

(defn push-to-exec-stack [state p]
  (stack/push state :push/exec p))

(defn typeof-item
  "Returns the type of the item, as a keyword."
  [item]
  (prot/m-typeof-item item))

(defn push-to-stack [state item]
  (stack/push state (typeof-item item) item))

(defn name? [item]
  (= :push/name (typeof-item item)))

(defn literal? [p]
  (not (#{:push/name :push/code} (typeof-item p))))

(defn handle-name
  "Handle a push identifier.
  "
  [state identifier]
  (->
   (if
       ;; When the name-quoted flag is set, a identifier is pushed onto the NAME stack
       (::name-quoted? state)
       (stack/push state :push/name identifier)

       (if-let [v (get state :bindings identifier)]
         ;; the bound value is pushed to the exec stack
         (stack/push state :push/exec v)
         ;; if the identifier was not encountered before, push it on the NAME stack
         (stack/push state :push/name identifier)))

   (assoc ::name-quoted? false)))


(instructions/register-instruction
 {:sym-name 'name_quote
  :in []
  :out :state
  :doc "The next name is pushed on the NAME stack, regardless of whether it is bound or not.
Presumably for redefinition."
  :f (fn [state] (assoc state ::name-quoted? true))})

(instructions/register-instruction
 {:sym-name 'code_quote
  :in []
  :out :state
  :doc "CODE.QUOTE causes the next encountered piece of code, whatever it is, to be pushed onto the CODE stack rather than being executed.

NOTE: this moves the next item in the EXEC stack to the CODE stack.
"
  :f (fn [state]
       ;; the existence of the EXEC stack permits a simpler implementation: simply move the top item of the EXEC stack to the CODE stack
       (let [item (stack/peek-item state :push/exec)]
         (-> state
             (stack/pop-item :push/exec)
             (stack/push :push/code item))))})


(defn execute
  [state program]
  ;; using the Push EXEC stack for recursion, inside Push
  (let [state (stack/push state :push/exec program)]
    (loop [state state]
      (if (empty? (-> state :stacks :push/exec))
        state
        (recur (let [item (stack/peek-item state :push/exec)
                     state (stack/pop-item state :push/exec)
                     instruction (instructions/instruction item)]
                 (cond
                   instruction
                   (instructions/execute-instruction state instruction)

                   (name? item)
                   (handle-name state item)

                   (literal? item) (push-to-stack state item)
                   ;; item is a list
                   :else (reduce (fn [state p]
                                   (stack/push state :push/exec p))
                                 state
                                 ;; reverse, so the first item in
                                 ;; the list is the first exec
                                 ;; instruction
                                 (reverse item)))))))))










(comment


  )
