(ns benjamin-schwerdtner.clj-push3-next.prot)

(defprotocol PushValue
  (m-typeof-item [this]))

(extend-protocol PushValue
  java.lang.String
  (m-typeof-item [this] :push-type/string)
  java.lang.Boolean
  (m-typeof-item [this] :push-type/boolean)
  java.lang.Integer
  (m-typeof-item [this] :push-type/integer)
  java.lang.Long
  (m-typeof-item [this] :push-type/long)
  java.lang.Float
  (m-typeof-item [this] :push-type/float)
  java.lang.Double
  (m-typeof-item [this] :push-type/double)
  java.util.List
  (m-typeof-item [this] :push-type/code)
  clojure.lang.Symbol
  (m-typeof-item [this] :push-type/name)
  java.lang.Character
  (m-typeof-item [this] :push-type/char)
  Object
  (m-typeof-item [this] :push-type/clj-object)
  nil
  (m-typeof-item [this] :push-type/clj-object))