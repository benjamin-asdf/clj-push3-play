;; See clojush/pushstate.clj

(ns benjamin-schwerdtner.clj-push3-next.pushstate
  ;; (:refer-clojure)
  )

(defn push [state push-type value]
  (update-in state [:stacks push-type] conj value))

(defn peek-item [state push-type]
  (or (-> state :stacks push-type peek) :no-stack-item))

(defn stack-assoc
  "Puts value at position on type stack in state. This is a utility, not for use
   as an instruction in Push programs."
  [state push-type n value]
  (update-in state [:stacks push-type] assoc n value))

(defn stack-nth [state push-type n]
  (get-in state [:stacks push-type n]))

(defn pop-item [state push-type]
  (update-in state [:stacks push-type] pop))
