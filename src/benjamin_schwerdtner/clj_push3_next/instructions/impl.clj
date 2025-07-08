(ns benjamin-schwerdtner.clj-push3-next.instructions.impl
  (:require
   [benjamin-schwerdtner.clj-push3-next.pushstate :as stack]))

(defn execute-instruction
  "Returns a new `state` with instruction `instr` executed.

  instr follows an instruction spec:

  in: A vector of input stack types, can be empty.

  out: An output stack type, or the keyword `:state` to signify
       state level transformation.

  f: A transform function, where the first arg is the state,
     successive args are `:in` arguments.

  "
  [state {:push.instruction/keys [in out f] :as instr}]
  (let [in-vals-info (reduce (fn [[s vs] t]
                               (let [stack (-> s
                                               :stacks
                                               t)]
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
        (cond
          (= out :state)
          outcome
          :else
          (update-in state [:stacks out] conj outcome))))))

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


;; ------------------------

(defn define-impl
  "Defines the identifier `push-name` as an instruction that
  puts the value onto the exec stack."
  [state push-name value]
  (assoc-in state [:bound-names push-name] value))

(comment
  ;; ---------------------
  ;; user:

  (register-instruction
   {:sym-name 'boolean_and
    :in [:push/boolean :push/boolean]
    :out :push/boolean
    :f (fn [_ a b] (and a b))})

  (when-let [inst (instruction 'boolean_and)]
    (let [state {:stacks {:push/boolean [true true]}}]
      (execute-instruction state inst))))


;; == operators ==
;; ----------------------------

(defn shover [{:keys [sym-name push-type]}]
  {:sym-name sym-name
   :in [:push/integer]
   :out :state
   :f (fn [state index]
        (stack/shove-item state push-type index))})

(defn yankdupper [{:keys [sym-name push-type]}]
  {:sym-name sym-name
   :in [:push/integer]
   :out :state
   :f (fn [state index]
        (stack/yankdup-item state push-type index))})

(defn yanker [{:keys [sym-name push-type]}]
  {:sym-name sym-name
   :in [:push/integer]
   :out :state
   :f (fn [state index]
        (stack/yank-item state push-type index))})

(defn flusher [{:keys [sym-name push-type]}]
  {:sym-name sym-name
   :in []
   :out :state
   :f (fn [state]
        (stack/flush-stack state push-type))})

(defn rotater [{:keys [sym-name push-type]}]
  {:sym-name sym-name
   :in []
   :out :state
   :f (fn [state]
        (stack/rotate-top-three state push-type))})

(defn swapper
  [{:keys [sym-name push-type]}]
  {:f (fn [state] (stack/swap-top-two state push-type))
   :in []
   :out :state
   :sym-name sym-name})

(defn dupper
  [{:keys [sym-name push-type]}]
  {:f (fn [state] (stack/duplicate-top state push-type))
   :in []
   :out :state
   :sym-name sym-name})

(defn popper
  [{:keys [sym-name push-type]}]
  {:f identity
   :in [push-type]
   :out :state
   :sym-name sym-name})

(defn stack-depth-impl
  [{:keys [sym-name push-type]}]
  {:f (fn [state]
        (stack/stack-depth state push-type))
   :in []
   :out :push/integer
   :sym-name sym-name})

(defn definer
  [{:keys [sym-name push-type]}]
  {:f define-impl
   :in [:push/name push-type]
   :out :state
   :sym-name sym-name})
