(ns benjamin-schwerdtner.clj-push3-next.configuration.config)

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
