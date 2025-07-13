(ns benjamin-schwerdtner.algorithms.hill-climbing)

(defn bit-mutate
  [individual mut]
  (into [] (map (fn [e] (if (< (rand) mut) (rand-nth [0 1]) e))) individual))

(defn bit-make-individual [N]
  (into [] (repeatedly N #(rand-int 2))))

(defn random-mutation-hill-climbing
  "
  `mutate`: a function of 2 args, an individual and the mutation chance `mut`.

   It should return a variant individual where each locus of the genome is mutated with `mut` chance
   from the available alleles (at that locus).

  `make-individual`: A function to create an initial individual.

  `fitness`: A function that returns a number, the fitness, given an individual.

  `done?` : A function given state

   {:individual _the current individual genome_ :generation _generation number_  }

  "
  [{:keys [fitness done? mut mutate make-individual]}]
  (loop [{:keys [individual] :as state} {:fitness fitness
                                         :generation 0
                                         :individual
                                           (make-individual)}]
    (if (done? state)
      state
      (recur (let [candidate (mutate individual mut)]
               (cond-> state
                 (<= (fitness individual) (fitness candidate))
                   (assoc :individual candidate)
                 true (update :generation inc)))))))
