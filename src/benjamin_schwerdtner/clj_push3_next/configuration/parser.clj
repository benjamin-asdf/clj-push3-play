(ns benjamin-schwerdtner.clj-push3-next.configuration.parser
  (:require [clojure.string :as str]))

;; --------------------------------
;;
;; It is not implemented in Clojush either.
;;
;; I feel licenced to skip this for now.
;;
;; --------------------------------


;; A Push3 configuration file is a plain text file. Any line beginning with "#" is a comment and is ignored. Actual configuration lines come in three forms:

;; <parameter name> <value>
;; type <type name>
;; instruction <instruction name>

(defn read-configuration-1
  [lines]
  (sequence
   (comp (remove #(re-find #"^\s+#" %))
         (map #(str/split % #"\s+"))
         cat
         (remove str/blank?)
         (partition-all 2)
         ;; (map vec)
         (map (fn [[k v]] [(keyword (str/lower-case k)) v])))
   lines))

(def read-type
  {"FLOAT" :push/float
   "INTEGER" :push/integer
   "BOOLEAN" :push/boolean
   "STRING" :push/string
   "CHAR" :push/char
   "CODE" :push/code})

(defn read-configuration
  [str]
  (->> (str/split-lines str)
       read-configuration-1
       (map read-value)))

(comment

  (->> (read-configuration-1
        [" # foo" " MAX-RANDOM-FLOAT 1.0" " EVALPUSH-LIMIT 1000"
         "  type FLOAT"
         " instruction INTEGER.FROMBOOLEAN" " instruction INTEGER.>"])
       (reduce (fn [acc [k v]]
                 (case k
                   :type (update acc :types conj (read-type v))
                   :instruction (update acc
                                        :instructions
                                        conj
                                        (symbol (str/replace
                                                 (str/lower-case v)
                                                 #"\."
                                                 "_")))
                   (assoc-in acc [:parameters k] (read-string v))))
               {:instructions [] :parameters {} :types []}))

  ;; {:instructions [integer_fromboolean integer_>]
  ;;  :parameters {:evalpush-limit 1000 :max-random-float 1.0}
  ;;  :types [:push/float]}
  )
