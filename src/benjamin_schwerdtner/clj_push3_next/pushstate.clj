;; See clojush/pushstate.clj

(ns benjamin-schwerdtner.clj-push3-next.pushstate)

(defn push [state push-type value]
  (update-in state [:stacks push-type] conj value))

(defn peek-item [state push-type]
  (let [stack (get-in state [:stacks push-type])]
    (if (empty? stack)
      :no-stack-item
      (peek stack))))

(defn stack-assoc
  "Puts value at position on type stack in state. This is a utility, not for use
   as an instruction in Push programs."
  [state push-type n value]
  (update-in state [:stacks push-type] assoc n value))

(defn stack-nth [state push-type n]
  (get-in state [:stacks push-type n]))

(defn stack-delete-deep
  "Deletes item `n` in stack `push-type`."
  [state push-type n]
  (update-in state
             [:stacks push-type]
             (fn [stack]
               (vec (concat (subvec stack 0 n)
                            (subvec stack (inc n)))))))

(comment
  (stack-delete-deep {:stacks {:f [1 2 3]}} :f 0)
  (stack-delete-deep {:stacks {:f [1 2 3]}} :f 2)
  (stack-delete-deep {:stacks {:f [1 2 3]}} :f 1))

(defn pop-item [state push-type]
  (update-in state [:stacks push-type] pop))

(defn stack-depth [state push-type]
  (count (get-in state [:stacks push-type] [])))

(defn empty-stack? [state push-type]
  (empty? (get-in state [:stacks push-type])))

(defn pop-n [state push-type n]
  (if (>= (stack-depth state push-type) n)
    (loop [i 0
           s state
           values []]
      (if (< i n)
        (let [value (peek-item s push-type)]
          (recur (inc i)
                 (pop-item s push-type)
                 (conj values value)))
        [s values]))
    nil))

(defn duplicate-top [state push-type]
  (let [top (peek-item state push-type)]
    (if (= top :no-stack-item)
      state
      (push state push-type top))))

(defn swap-top-two [state push-type]
  (if-let [[s [a b]] (pop-n state push-type 2)]
    (-> s
        (push push-type a)
        (push push-type b))
    state))

(defn rotate-top-three [state push-type]
  (if-let [[s [a b c]] (pop-n state push-type 3)]
    (-> s
        (push push-type b)
        (push push-type c)
        (push push-type a))
    state))

(defn flush-stack [state push-type]
  (assoc-in state [:stacks push-type] []))

(defn yank-item
  "Yanks an item from deep in the specified stack,
  at index `index`.
   "
  [state push-type index]
  ;; Behaviour around yank-item and integer stack itself might
  ;; be slightly different from the spec.
  (if (empty-stack? state push-type)
    state
    (let [state (pop-item state :push/integer)
          stack (get-in state [:stacks push-type])
          ;; design decision to auto put into idx range
          index (mod (Math/abs (int index)) (count stack))
          item (stack-nth state push-type index)]
      (-> state
          (stack-delete-deep push-type index)
          (push push-type item)))))

(comment
  (stack-nth {:stacks {:f [0]}} :f 0)
  (stack-nth {:stacks {:f [0]}} :f (mod 1 1)))

(defn yankdup-item
  "Pushes a copy of an indexed item \"deep\" in the stack onto the top of the stack,
  without removing the deep item. The index is taken from the INTEGER stack."
  [state push-type index]
  (if (empty-stack? state push-type)
    state
    (let [state (pop-item state :push/integer)
          stack (get-in state [:stacks push-type])
          index (mod (abs index) (count stack))
          deep-item (stack-nth state push-type index)]
      (-> state
          (push push-type deep-item)))))

(defn stack-shove
  [state push-type item n]
  (update-in
    state
    [:stacks push-type]
    (fn [stack]
      (vec (concat (subvec stack 0 n) [item] (subvec stack n))))))


(defn shove-item
  "Inserts the top of `push-type` \"deep\" in the stack"
  [state push-type item index]
  (let [stack (-> state :stacks push-type)
        index (mod (abs index) (count stack))]
    (-> state
        (stack-shove push-type item index))))

(comment
  (stack-shove {:stacks {:f [1 2 3]}} :f :a 0)
  (stack-shove {:stacks {:f [1 2 3]}} :f :a 2))
