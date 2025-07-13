(ns benjamin-schwerdtner.clj-push3-play.stack.pushvalues)

(defmulti sanitize-value (fn [_ push-type v] push-type))

(defmethod sanitize-value :default [_ _ v] v)

(defmethod sanitize-value :push/integer [_ _ v]
  (def v v)
  (max (long -1e10) (min (long 1e10) v)))

(defmethod sanitize-value :push/float [_ _ v]
  (let [v (max -1e12 (min 1e12 v))]
    (if (< (abs v) 1e-12)
      (float 1e-12)
      v)))

(defmethod sanitize-value :push/code [state _ v]
  ;; check max code points
  v)
