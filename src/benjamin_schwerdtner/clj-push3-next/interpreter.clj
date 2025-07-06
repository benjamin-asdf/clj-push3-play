(ns benjamin-schwerdtner.clj-push3-next.interpreter
  (:require
   [benjamin-schwerdtner.clj-push3-next.pushstate :as stack]
   [benjamin-schwerdtner.clj-push3-next.prot :as prot]
   [benjamin-schwerdtner.clj-push3-next.instructions.interface
    :as instructions
    ]))

;; -
;; https://faculty.hampshire.edu/lspector/push3-description.html
;; -

;; =============================
;; Push3
;; =============================

(defn push-to-exec-stack [state p]
  (stack/push state :exec p))

(defn typeof-item
  "Returns the type of the item, as a keyword."
  [item]
  (prot/m-typeof-item item))

(defn push-to-stack [state item]
  (stack/push state (typeof-item item) item))

(defn name? [item]
  (= :push-type/name (typeof-item item)))

(defn literal? [p]
  (not (#{:push-type/name :push-type/code} (typeof-item p))))

(defn handle-name
  ""
  [state name]
  (if-let [v (get state :bindings name)]
    ;; the bound value is pushed to the exec stack
    (stack/push state :exec v)
    ;; if the name was not encountered before, push it on the name stack
    (stack/push state :push-type/name name)))

(defn execute
  [state program]
  ;; using the Push EXEC stack for recursion, inside Push
  (let [state (stack/push state :exec program)]
    (loop [state state]
      (if (empty? (-> state :stacks :exec))
        state
        (recur (let [item (stack/peek-item state :exec)
                     state (stack/pop-item state :exec)
                     instruction (instructions/instruction item)]
                 (cond
                   instruction
                   (instructions/execute-instruction state instruction)

                   (name? item)
                   (handle-name state item)

                   (literal? item) (push-to-stack state item)
                   ;; item is a list
                   :else (reduce (fn [state p]
                                   (stack/push state :exec p))
                                 state
                                 ;; reverse, so the first item in
                                 ;; the list is the first exec
                                 ;; instruction
                                 (reverse item)))))))))
