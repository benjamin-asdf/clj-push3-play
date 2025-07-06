(ns benjamin-schwerdtner.clj-push3-next.instructions.boolean)


;; 1.
;;
;; Komplett ausgeschreiben

(defn boolean-and
  [state]
  (let [a (-> state :stacks :boolean peek)
        b (-> state :stacks :boolean pop peek)]
    (cond (nil? a) state
          (nil? b) state
          :else (update-in state
                           [:stacks :boolean]
                           (comp #(conj % (and a b)) pop pop)))))

;; 2:
;;
;; In/out stacks metadata

(defn
  ^{:push/stack-in [:boolean :boolean]
    :push/stack-out :boolean
    :push/instruction 'boolean_and}
  boolean-and
  [state a b]
  (and a b))

;; .. this is so under the hood?

(defn find-push-instructions
  [ns]
  (keep (fn [[_ v]]
          (when-let [instr-name (-> v
                                    meta
                                    :push/instruction)]
            [instr-name v]))
        (ns-publics ns)))

(def instruction-table-from-metas
  (into
    {}
    (mapcat find-push-instructions
      (map find-ns
           ;; user needs to list all namespaces
           ['benjamin-schwerdtner.clj-push3-next.instructions.boolean]))))


;; 3.
;;
;; instruction mit multimethod


(defmulti push-instruction identity)

;; user:

(defmethod push-instruction :boolean_and
  [_]
  #:push.instruction{:f (fn boolean_and [_ a b] (and a b))
                     :in [:boolean :boolean]
                     :out :boolean})



;; pro: The extension method is obvious

;; con: user can execute code in the instruction lookup, is not intended

;; ------------------------
;; exectute instruction:

(defn execute-instruction
  [state {:push.instruction/keys [in out f]}]
  (let [in-vals-info (reduce (fn [[s vs] t]
                               (let [stack (-> s :stacks t)]
                                 (if (empty? stack)
                                   (ensure-reduced ::empty)
                                   [(update-in s [:stacks t] pop)
                                    (conj vs (peek stack))])))
                             [state []]
                             in)]
    (if (= in-vals-info ::empty)
      ;; NOOP
      ;;
      state
      (let [[state in-vals] in-vals-info
            outcome (apply f state in-vals)]
        (update-in state [:stacks out] conj outcome)))))


(let [state {:stacks {:boolean [true false]}}]
  (execute-instruction state (push-instruction :boolean_and)))

{:stacks {:boolean [false]}}


;; 4.

(def instruction-table (atom {}))

(defn register-instruction
  [{:keys [sym-name in out f]}]
  (swap! instruction-table assoc
    sym-name
    {:push.instruction/in in
     :push.instruction/out out
     :push.instruction/f f}))

(defn instruction [sym]
  (get @instruction-table sym :no-instruction))


;; ---------------------
;; user:

(register-instruction
 {:sym-name 'boolean_and
  :in [:boolean :boolean]
  :out :boolean
  :f (fn [_ a b] (and a b))})

(when-let [inst (instruction 'boolean_and)]
  (let [state {:stacks {:boolean [true true]}}]
    (execute-instruction state inst)))


;; combines pro of multimethod and is obvious?
