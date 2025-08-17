(ns benjamin-schwerdtner.clj-push3-play.hdc.conceptual-hyperspace
  (:require
   [benjamin-schwerdtner.clj-push3-play.hdc.fhrr :as hd]
   [benjamin-schwerdtner.clj-push3-play.hdc.data :as hdd]
   [benjamin-schwerdtner.clj-push3-play.hdc.spatial-semantic-pointer :as ssp]
   [benjamin-schwerdtner.clj-push3-play.hdc.config :refer [*torch-device*]]

   [benjamin-schwerdtner.clj-push3-play.hdc.resonator :as resonator]
   [benjamin-schwerdtner.clj-push3-play.hdc.fractional-power-encoding :as fpe]

   [libpython-clj2.python :refer [py. py..] :as py]
   [libpython-clj2.require :refer [require-python]]))



;;
;; =================================================================
;;
;; Conceptual Hyperpsace (CH) is a CST implemented using HDC.
;;
;; https://arxiv.org/abs/2411.08684
;;
;; CST: Conceptual Spaces Theory
;; https://en.wikipedia.org/wiki/Conceptual_space
;;
;; =================================================================
;;


;;
;; 'Analogies are partial similarities between different situations
;; that support further inferences' [Gentner 1998].
;;
;; Eqn 1)   A : B :: C : X
;;

;; Analogy desiderata:
;; -------------------------------


;; retrieval
;; -----------
;; Given C find A and B


;; mapping
;; -----------
;; determine the structural correspondance (frame) between A and B
;; find X by applying it to C.
;;
;; A : B  is doing a lot of work... 👈
;;
;; called semantic relationships
;; theories: "category based", "property based", others: event-based, part-whole, causal
;;
;;

;; inference
;; ---------------
;; Use A to advance the concept C

;;

;;
;; CST:
;;
;; - points denote objects
;; - regions is a concept
;;
;; HDC is a natural fit to model CST!
;;
;;
;; crucial insight: that the HDC framework could operationalize the
;; cognitive science theory of Conceptual Spaces [Gärdenfors 2000]
;;

;; ----------------------------------------------------------------
;; ---


;;
;; Domain `D`:
;; A `k`-dimensional space (low dimensional)
;;
;; for example the color domain
;;
;; k = 3
;; k-bases = { hue, saturation, brightness }
;;
;;
;; concept: a point in D.
;;
;; e.g. point-a, point-b, point-c
;; point-a ~= red
;; point-b ~= purple, etc.
;;

;; let's stay with color example.

;; (def base-hdv (memoize (fn [_] (hd/seed))))

;; k = 3: hue, saturation, brightness
(defonce bases-codebook (hd/seed 3))
(defn base-hdv [k]
  (torch/index_select bases-codebook 0 (torch/tensor [k])))

;; concrete:

#_(defn encode-point-concrete
    [[hue saturation brightness]]
    (hd/multibind
     [(ssp/fractional-power-encoding (base-hdv 0) hue)
      (ssp/fractional-power-encoding (base-hdv 1) saturation)
      (ssp/fractional-power-encoding (base-hdv 2) brightness)]))

(defn encode-point
  "
  Returns a hdv representing the concept `P` given by domain point `p`.

  `p`: A real valued vector representing a low dimensional point in the concept domain `D`.

  `bases`: An indexed lookup of basis hypervectors.
   You need `k` basis vectors for the encoding of the k-dimensional domain `D`.

   dim(D) == dim(p) == count(bases) == k

   Example concept encoding for `purple`:

   PURPLE = x^6.2 ⊗ y^-6.2 ⊗ z^5.3


   Like this:


   ;; k == 3, k-bases = { hue, saturation, brightness }

   (defonce bases-codebook (hd/seed 3))
   (encode-point [6.2 -6.2 5.3] (into [] bases-codebook))
   ;; => P∈H

  "
  [p bases]
  ;;
  ;; dim(p) == count(bases) == k
  ;;
  (hd/bind (map (fn [b v] (fpe/fpe b v)) bases p)))


;; =====================================

(comment
  ;; the prototype location (purple)
  (def p [6.2 -6.2 5.3])
  )

;; ----------------------

(defn encode
  ""
  [[pa pb pc :as domain-points]]
  ;; output F
  ;;
  )


;;
;; Category based analogy:
;;

;; Obtaining X:
;;
;; The concepts in this example are already in same category (color).
;;
;;
;; Parallelogram model [Rumelhart and Abrahamson, 1973]
;;
;;

;; Eqn 5: pX = pC - pA + pB


;; for

;; PURPLE : BLUE :: ORANGE : X

;; Eqn 6: x = (orange ⊗ purple^-1)  ⊗ blue
;;

;;
(defn categorical-mapping
  "
  Returns a hypervector that is the query of outcome of categorical analogical mapping:

  Given A, B, C find X

  A : B :: C : X

  a, b and c are hypervectors

  Using Parallelogram model [Rumelhart and Abrahamson, 1973]

  "
  [a b c]
  ;; Algorithm 3 find (using parallelogram method)
  ;; 1: Input: a, b, c
  ;; 2: Output: x
  ;; 3: x ← (c ⊛ a−1 ) ⊛ b
  ;; 4: return x
  ;; (hd/bind (hd/bind c (hd/inverse a)) b)

  (hd/bind (hd/unbind c a) b))


;;
;; with x in hand, decode using a resonator network
;;

(defn decode
  "Returns a concept point in the domain D given a hypervector x.


  opts are like the return of [[bases-codebooks]].

  shapes:

  x: (d)
  values: (r)
  books: (k,r,d)

  k: Domain dimensions = D
  r: book resolution
  d: hyperdim

  "
  [x {:keys [books values]}]
  (when-let [factors-idxs (:factor-idxs (resonator/factorize x books))]
    (torch/index_select values -1 factors-idxs)))


;; resolution means step size

(defn bases-codebooks
  [{:keys [k resolution high low basis-seeds]}]
  (let [epsilon 1e-5
        values (torch/arange low
                             (+ high epsilon)
                             resolution
                             :device
                             *torch-device*)
        basis-seeds (or basis-seeds (hd/seed k))]
    {:books
     (fpe/fpe basis-seeds values)
     :values values
     :basis-seeds basis-seeds}))








(comment
  (do
    (def scaling-constant 10)
    (defn preprocess-color [hue saturation brightness]
      [(* (Math/cos hue) (/ saturation scaling-constant))
       (* (Math/sin hue) (/ saturation scaling-constant))
       (/ brightness scaling-constant)])
    (def p-purple (preprocess-color (* 315 (/ Math/PI 180)) 87 53))
    (def p-blue   (preprocess-color (* 270 (/ Math/PI 180)) 1 5))
    (def p-orange (preprocess-color (* 30  (/ Math/PI 40)) 88 1)
      ;; [-6.222539674441618 6.222539674441619 1/10]
      )
    (def p-yellow (preprocess-color (* 73 (/ Math/PI 180)) 2 97))
    (def basis-seeds (hd/seed 3))
    (def B
      (bases-codebooks
       {:basis-seeds basis-seeds
        :high 10
        :k 3
        :low -10
        :resolution 0.1}))
    (def P (encode-point [1.0 1.0 1.0] basis-seeds))
    (let [[purple blue orange yellow]
          (mapv
           #(encode-point % basis-seeds)
           [p-purple p-blue p-orange p-yellow])
          x
          (categorical-mapping purple blue orange)
          #_(hd/bind
             (hd/bind orange (hd/inverse purple))
             blue)]
      ;; (decode x B)
      (hd/similarity purple blue)
      (hd/similarity x orange)
      (hd/similarity x yellow)
      (hd/similarity orange yellow)
      (hd/similarity orange purple)
      (decode P B)
      (decode orange B))))





;; Solving a Property-based Analogy

;; compare semantic relationship between categories and properties

;; A : B :: C : X

;; property based example:

;; APPLE : RED :: BANANA : X

;; 1. identify property dimensions that correspond to concept or property B
;;    RED, color domain
;; 2. find the location within that dimenions closes to prototype location C.
;;    Would be color domain dimensions for BANANA (i.e. YELLOW)
;;







;; -----------------------------------------



;; Observation:
;; spatial_semantic_pointer.clj is a HC with k=2
;;
;;
;; where
;; ⊗ is bind
;; x^k is fractional power encoding of x by k.
;;
;;
;;
;; function 'readout' is equivalent to decode.
;; Alg also needs to build reso
;;
