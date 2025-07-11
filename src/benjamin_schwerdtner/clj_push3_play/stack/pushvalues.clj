(ns benjamin-schwerdtner.clj-push3-play.stack.pushvalues)

(defmulti sanitize-value (fn [_ push-type v] push-type))

(defmethod sanitize-value :default [_ _ v] v)

(defmethod sanitize-value :push/integer [_ _ v] v)

(defmethod sanitize-value :push/float [_ _ v]
  (if (nil? v)
    (throw (Exception. "float is nil"))
    v))

(defmethod sanitize-value :push/code [state _ v]
  ;; check max code points
  v)
