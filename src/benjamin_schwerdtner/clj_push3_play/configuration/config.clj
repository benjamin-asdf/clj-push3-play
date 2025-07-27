(ns benjamin-schwerdtner.clj-push3-play.configuration.config
  (:require [clojure.edn :as edn]))

(defn defaults []
  {:parameters
   {:max-random-float 1.0
    :min-random-float -1.0
    :max-random-integer 10
    :min-random-integer -10
    :evalpush-limit 1000
    :new-erc-name-probability 0.001
    :max-points-in-random-expressions 25
    :max-points-in-program 100}

   ;; ignore, use all
   ;; :intructions []
   ;; :types [:push/float]
   })

(def program-config
  (memoize (fn []
             (edn/read-string (slurp "config.edn")))))
