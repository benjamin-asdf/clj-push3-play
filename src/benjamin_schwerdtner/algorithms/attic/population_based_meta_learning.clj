(ns benjamin-schwerdtner.algorithms.population-based-meta-learning)

;; https://arxiv.org/abs/2103.06435

;; 1. paper did not keep capturing my attention atm. potentially revisit.

;;
;; - total-population not defined in the alg code?
;; - splitting { X, R } is similar to ideas/metaevolution.org idea, there { mut, E }
;;


;; Basic idea #1 seems similar to Dawkin's "rower boat" analogy.





{
 ;; G
 :genomes
 [{:parameters [] :population-count 0}]
 ;; F()
 :fitness (fn [genome])
 ;; M()
 ;; genome -> offpring
 :mutation
 (fn [genome])
 ;; D
 ;; the ratio of population that dies every generation
 :decay-ratio 'number
 ;; C
 :offpring-count 'number
 ;; genome -> rank between 0 and 1 based on fitness
 :rank (fn [genome])}

(declare total-population)

(defn pupulation-based-meta-learning
  [{:keys [genomes fitness mutation decay-ratio rank N
           offspring-count]
    :as state}]
  (loop [{:keys [genomes generation] :as state} (assoc state
                                                  :generation 0)]
    (if (<= N generation)
      state
      (let [genomes
            (pmap (fn [g]
                    (let [f (fitness (:parameters g))
                          rank (rank f)
                          population-count (* (:population-count g)
                                              (- 1 decay-ratio))
                          population-count (* population-count rank)
                          population-count (/ population-count
                                              total-population)]
                      (merge g
                             {:fitness f
                              :population-count population-count
                              :rank rank})))
                  genomes)
            genomes
            (concat
             ;; They also say the drop genomes with less that 1/1000 pop in the paper somewhere
             genomes
             (mapcat (fn [g]
                       (repeatedly (int (* (:population-count g)
                                           offspring-count))
                                   (fn [] (mutation g))))
                     genomes))]
        (recur (-> state
                   (assoc :genomes genomes)
                   (update :generation inc)))))))


;; Q:
;; - how does having fit children help with a 'genome' being fit?
;;   Do I do something wrong with the population count?

;; - where is the alg that splits the fitness and the mut genome?
;; {X, R} in the paper
