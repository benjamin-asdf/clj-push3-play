(ns benjamin-schwerdtner.algorithms.rssa-bits
  (:require [random-seed.core :as rand]))

;; HIFF
;; see src/benjamin_schwerdtner/problems/hiff.clj


;; https://link.springer.com/chapter/10.1007/978-3-540-74913-4_119

;; Alg is relatively hard to make from papar alone unfortunately.
;;

;; 1. population of candidates that is supposed to model entities in an ecosystem,
;;    that _do not_ compete for the same niche.
;; - a candidate entity only specifies alleles for a subset of the problem loci

;; 2. No mutation, only symbiogenic join

;; 3. Partially specified entity is evaluated in context.
;;    A context is a set of randomly generated values for each problem varialbeeee that is not speffieied by the entity under evaluation.


;; fitness delta δ (small delta)
;; the fitness diffence between a context `c` alone and an entity `A` in that context.

(defn fitness-δ [context entity {:keys [fitness compose]}]
  ;; compose: The symbiosis operator
  ;;
  (-
   (fitness (compose entity context))
   (fitness context)))


;; Synergy `s` is defined as the difference in fintess deltas of the symbiont A+B and
;; the sum of the individual entities

(defn synergy [entity-a entity-b context {:keys [compose] :as opts}]
  (-
   (fitness-δ (compose entity-a entity-b) context opts)
   ;; The synergy of A+B is low, if the fitness of A or B with the other context is high.
   ;; This models the notion that symbiononts don't need to form, if they do well on their own.
   (fitness-δ entity-a context opts)
   (fitness-δ entity-b context opts)))


;; - reciprocal, both entities 'want' the synergy

(defn initial-synergy-matrix [ecosystem-size]
  (vec
   (for [_i (range ecosystem-size)]
     (vec
      (for [_j (range ecosystem-size)]
        0)))))

(defn reciprocal-synergy-symbiosis
  "

  Symbiogenesis (biology): Make a new species from parent species.

  Symbiogenetic algorithms allow putting together 2 _separately evolved_ genomes.
  The structure of the algorithm changes from hill climbing to divide-and-conquer.
  The intuition is that this should do well with problems that de-compose into subproblems.
  "
  [{:keys [atom-units
           max-generations
           contexts-count
           bit-string-lenght
           ecosystem
           ]}]
  ;; ecosystem initial with all atomic units availabe
  (loop [{:keys [generation ecosystem compose fitness] :as state}
         {:generation 0
          :synergy-matrix (initial-synergy-matrix (count ecosystem))}]
    (if
        (or
         (<= max-generations generation)
         ;; all remaining entities have length N

         )
        state
        (recur
         ;; generate context as random bit-string
         (let [context (repeatedly bit-string-lenght #(rand/rand-int 2))
               ;; evaluate context?
               ]

           ;; for each pair in the ecosysem

           (for [A ecosystem
                 B ecosystem
                 :when (not= A B)]

             ;; what do you do with the synergy?
             #_(synergy A B context {:compose compose :fitness fitness})

             ;;
             ;; rescale synergy averages values to lie in range [0,1]
             ;;

             ;;
             (let [reciprocal-synergy
                   (*
                    (synergy A B context {:compose compose :fitness fitness})
                    (synergy B A context {:compose compose :fitness fitness}))])


             ;; find J highest reciprocal synergy values and make these joins
             ;;
             ))
         (-> state (update :generation inc))))))
