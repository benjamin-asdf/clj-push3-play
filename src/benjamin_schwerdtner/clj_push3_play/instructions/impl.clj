(ns benjamin-schwerdtner.clj-push3-play.instructions.impl
  (:require
   [benjamin-schwerdtner.clj-push3-play.stack.pushstate :as stack]))

(defn execute-instruction
  "Returns a new `state` with instruction `instr` executed.

  instr follows an instruction spec:

  in: A vector of input stack types, can be empty.

  out: An output stack type, or the keyword `:state` to signify
       state level transformation.

  f: A transform function, where the first arg is the state,
     successive args are `:in` arguments.

  "
  [state {:as instr :push.instruction/keys [in out f]}]
  (let [{:push.instruction/keys [in out f]} instr
        in-vals-info (reduce (fn [[s vs] t]
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
            [outcome err?] (try
                             [(apply f state in-vals) false]
                             (catch Exception e
                               (if (:exceptions-are-warnings? state)
                                 (binding [*out* *err*] (println e))
                                 (throw e))
                               [nil true]))]
        (cond
          ;; Exceptions are NOOPS, still pop the args and make
          ;; program go forward.
          err? state
          (= out :state) outcome
          :else (stack/push state out outcome))))))

(defonce instruction-table (atom {}))

(defn register-instruction
  [{:keys [sym-name in out f]}]
  (swap! instruction-table assoc
    sym-name
    {:push.instruction/in in
     :push.instruction/out out
     :push.instruction/f f
     :push.instruction/sym-name sym-name}))

(defn instruction [sym]
  (get @instruction-table sym :no-instruction))

(defn all-instructions [] (keys @instruction-table))

(defn all-identifiers [state]
  (concat
   (:instructions state)
   (vals (:bindings state))))


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

;; ------------------------

(defn define-impl
  "Defines the identifier `push-name` as an instruction that
  puts the value onto the exec stack."
  [state push-name value]
  (assoc-in state [:bindings push-name] value))

;; -------------------------------




;; == operators ==
;; ----------------------------

(defn shover [{:keys [sym-name push-type]}]
  {:sym-name sym-name
   :in [push-type :push/integer]
   :out :state
   :f (fn [state item index]
        (stack/shove-item state push-type item index))})

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
  {:f (fn [s _] s)
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
