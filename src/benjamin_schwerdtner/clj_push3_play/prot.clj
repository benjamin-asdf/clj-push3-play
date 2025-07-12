(ns benjamin-schwerdtner.clj-push3-play.prot)

(defprotocol PushValue
  (m-typeof-item [this]))

(extend-protocol PushValue
  java.lang.String
  (m-typeof-item [_this] :push/string)
  java.lang.Boolean
  (m-typeof-item [_this] :push/boolean)
  ;; -------------------
  java.lang.Integer
  (m-typeof-item [_this] :push/integer)
  java.lang.Long
  (m-typeof-item [_this] :push/integer)
  clojure.lang.BigInt
  (m-typeof-item [_this] :push/integer)
  ;; ------------------
  java.lang.Float
  (m-typeof-item [_this] :push/float)
  java.lang.Double
  (m-typeof-item [_this] :push/float)
  clojure.lang.Ratio
  (m-typeof-item [_this] :push/float)
  ;; -------------------
  java.util.List
  (m-typeof-item [_this] :push/code)
  clojure.lang.Symbol
  (m-typeof-item [_this] :push/name)
  java.lang.Character
  (m-typeof-item [_this] :push/char)
  clojure.lang.Keyword
  (m-typeof-item [_this] :push/clj-object)
  Object
  (m-typeof-item [_this] :push/clj-object)
  nil
  (m-typeof-item [_this] :push/clj-object))
