(ns benjamin-schwerdtner.algorithms.seam
  (:require [random-seed.core :as rand]))

;; ========================================
;; symbiogenic evolutionary adaptation model
;; ========================================

;; R. Watson 2006 "Compositional Evolution"


;; - I try to adapt this to GP.


;;
;; if we don't know which dimensions are important,
;; an individual b is better than a, if it is at least equal in all
;; and at least better than a in one dimension.
(defn pareto-dominance
  "
  Returns true, if a is perato dominant over b in contexts.

  That is when `a` has at least `b`s fitness in all contexts.

 `context-sensitive-fitness`: csf(x,theta) => fitness of x in context theta.

  "
  [context-sensitive-fitness contexts a b]
  ;;   a >> b <=> there is no theta for which: csf(y,theta) > csf(x,theta)
  (not
   (some (fn [θ]
           (<
            (context-sensitive-fitness a θ)
            (context-sensitive-fitness b θ)))
         contexts)))


(defn unstable?
  [a+b a b ecosystem {:keys [context-count fitness]}]
  ;; The intuition is that a+b only forms if
  ;; it outcompetes a and b in some niche
  (let [dominat? (partial pareto-dominance
                          (fn [x theta]
                            ;;
                            ;; In original, the context is build
                            ;; from superimposing a random set of
                            ;; ecosystem members.
                            ;;
                            (< (fitness x) (fitness theta)))
                          (take context-count
                                (rand/shuffle ecosystem)))]
    (or (not (dominat? a+b a)) (not (dominat? a+b b)))))


(defn symbiogenic-evolutionary-adaptation
  "
  Performs an early symbiogenesis inspired EA, SEAM.

  `composition`: A composition operator creating a symbiont from 2 individuals.


  `done?`: Stop condition.


  `make-single-feat-entities`: A function returning a sequence of single feature entities.
   SEAM does not create entities/features otherwise, so _all features must be listed_.

  `fitness`: f(x) => fitness of individual x.

  `context-count`: Used in the unstable? subroutine, the count of context to check for dominance
   in the ecosystem.

  That latest formed individual will be on the top slot of the :ecosystem stack at the end of the execution.
  And they are at least roughly sorted by fitness, with new formed individuals on top.


  "
  [{:keys [make-single-feat-entities composition done?] :as opts}]
  (loop [{:keys [ecosystem generation] :as state}
         (merge opts
                {:ecosystem (vec (make-single-feat-entities))
                 :generation 0})]
    (if (done? state)
      state
      (recur (merge state
                    {:ecosystem (let [[a b] (rand/shuffle ecosystem)
                                      a+b (composition a b)]
                                  (if (unstable? a+b a b ecosystem opts)
                                    ecosystem
                                    (conj ecosystem a+b)))
                     :generation (inc generation)})))))
