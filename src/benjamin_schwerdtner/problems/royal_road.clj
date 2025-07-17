(ns benjamin-schwerdtner.problems.royal-road)

;; Royal Road Functions
;; Mitchel, Forrest Holland (1992)
;;
;;
;; Tries to make a building block structure like problem domain
;;

(defn royal-road
  "

  Royal Road Functions
  Mitchel, Forrest Holland (1992).

  A fitness function where the bit string is divided into blocks,
  a block only contributes fitness when it is all 1's, 'solved'.

  The idea is that a search process exploiting modularity can
  find sub solutions to the problem.

  Count variables: for example 64, then there are 8 blocks.
  "
  [variables]
  (let [
        ;; subdivide into 8 blocks
        blocks (partition-all (Math/sqrt (count variables)) variables)
        ;; each block has a separate fitness, given by f1
        ;; f1 = all-one?
        all-one? (fn [vars] (if (every? #{1} vars) 1 0))]
    (apply + (map all-one? blocks))))
