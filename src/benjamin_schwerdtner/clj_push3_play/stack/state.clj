(ns benjamin-schwerdtner.clj-push3-play.stack.state)

(def push-types
  [:push/boolean
   :push/char
   :push/clj-object
   :push/code
   :push/exec
   :push/float
   :push/integer
   :push/name
   :push/string])

(defn setup-stacks []
  (into {} (map (juxt identity (constantly [])) push-types)))
