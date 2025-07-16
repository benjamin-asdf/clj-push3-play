(ns benjamin-schwerdtner.problems.vsm)

;; Variable structual modularity (VSM)

(defn weighted-sum-of-pairwise
  "

  `f`: The pairwise interaction of variables with states p an dq.

  `weights-ij`: The weighting of the interaction between variables i and j.


  "
  [weights-ij f variables]
  (apply +
         (for [i (range (count variables))
               j (range (count variables))]
           (*
            (weights-ij i j)
            (f (variables i) (variables j))))))

(defn variable-structural-modularity
  "

  The sum of the weighted pairwise dependencies between the problem variables.

  Like a weighted if-and-only-if.
  When the variables agree, their weight contributes to the fitness.

  This can be used to model modular dependency, by choosing some variables with
  strong interdepency (modules) and weak interpendency between modules.

  "
  [weights-ij input]
  (weighted-sum-of-pairwise
   weights-ij
   (fn [a b] (if (= a b) 1 0))
   input))


;; example
(defn modular-weights
  "
  Returns a weight matrix with shape [ variable-count, variable-count ].

  With 2 kinds of interactions, weak and strong.

  Along the matrix diagonal, creates 2 dimensional blocks of size `k`,
  filled with the strong interaction value.

  Otherwise, weak interaction value.

  "
  [{:keys [variable-count k weight-weak weight-strong]}]
  (->> (for [i (range variable-count)]
         (for [j (range variable-count)]
           (if (= (Math/floor (/ i k)) (Math/floor (/ j k)))
             weight-strong
             weight-weak)))
       (mapv vec)))


(comment
  (modular-weights
   {:k 5 :variable-count 10 :weight-strong 5 :weight-weak 1})

  ;; [[5 5 5 5 5 1 1 1 1 1]
  ;;  [5 5 5 5 5 1 1 1 1 1]
  ;;  [5 5 5 5 5 1 1 1 1 1]
  ;;  [5 5 5 5 5 1 1 1 1 1]
  ;;  [5 5 5 5 5 1 1 1 1 1]
  ;;  [1 1 1 1 1 5 5 5 5 5]
  ;;  [1 1 1 1 1 5 5 5 5 5]
  ;;  [1 1 1 1 1 5 5 5 5 5]
  ;;  [1 1 1 1 1 5 5 5 5 5]
  ;;  [1 1 1 1 1 5 5 5 5 5]]
  )
