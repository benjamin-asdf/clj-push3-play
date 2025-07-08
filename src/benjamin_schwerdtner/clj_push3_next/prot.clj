(ns benjamin-schwerdtner.clj-push3-next.prot)

(defprotocol PushValue
  (m-typeof-item [this]))

(extend-protocol PushValue
  java.lang.String
  (m-typeof-item [this] :push/string)
  java.lang.Boolean
  (m-typeof-item [this] :push/boolean)
  java.lang.Integer
  (m-typeof-item [this] :push/integer)
  java.lang.Long
  (m-typeof-item [this] :push/long)
  java.lang.Float
  (m-typeof-item [this] :push/float)
  java.lang.Double
  (m-typeof-item [this] :push/double)
  java.util.List
  (m-typeof-item [this] :push/code)
  clojure.lang.Symbol
  (m-typeof-item [this] :push/name)
  java.lang.Character
  (m-typeof-item [this] :push/char)
  Object
  (m-typeof-item [this] :push/clj-object)
  nil
  (m-typeof-item [this] :push/clj-object))
