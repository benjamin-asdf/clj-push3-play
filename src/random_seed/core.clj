;; https://github.com/trystan/random-seed/blob/master/src/random_seed/core.clj
(ns random-seed.core
  (:refer-clojure :exclude [rand rand-int rand-nth shuffle]))

;; Benjamin:
;; I made this a dynamic var
;; https://ask.clojure.org/index.php/4257/clojure-core-rand-for-seedable-randomness
;; I cannot use ThreadLocalRandom unfortunately, the seed cannot be set.

(defn random [^long seed]
  (doto (new java.util.Random) (.setSeed seed)))

(def ^:dynamic *rng* (random 0))

(defn rand
  "Returns a random floating point number between 0 (inclusive) and
  n (default 1) (exclusive). Works like clojure.core/rand except it
  uses the seed specified in set-random-seed!."
  ([] (.nextFloat *rng*))
  ([n] (* n (rand))))

(defn rand-int
  "Returns a random integer between 0 (inclusive) and n (exclusive).
  Works like clojure.core/rand except it uses the seed specified in
  set-random-seed!."
  [n]
  (int (rand n)))

(defn rand-nth
  "Return a random element of the (sequential) collection. Will have
  the same performance characteristics as nth for the given
  collection. Works like clojure.core/rand except it uses the seed
  specified in set-random-seed!."
  [coll]
  (nth coll (rand-int (count coll))))

(defn shuffle
  "Return a random permutation of coll. Works like clojure.core/shuffle
  except it uses the seed specified in set-random-seed!."
  [^java.util.Collection coll]
  (let [al (java.util.ArrayList. coll)]
    (java.util.Collections/shuffle al *rng*)
    (clojure.lang.RT/vector (.toArray al))))
