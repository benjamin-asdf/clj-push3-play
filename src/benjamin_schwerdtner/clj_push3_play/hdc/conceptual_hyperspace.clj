(ns benjamin-schwerdtner.clj-push3-play.hdc.conceptual-hyperspace
  (:require
   [benjamin-schwerdtner.clj-push3-play.hdc.fhrr :as hd]
   [benjamin-schwerdtner.clj-push3-play.hdc.data :as hdd]
   [benjamin-schwerdtner.clj-push3-play.hdc.spatial-semantic-pointer :as ssp]
   [benjamin-schwerdtner.clj-push3-play.hdc.config :refer [*torch-device*]]

   [libpython-clj2.python :refer [py. py..] :as py]
   [libpython-clj2.require :refer [require-python]]))



;;
;; =================================================================
;;
;; Conceptual Hyperpsace (CH) is a CST implemented using HDC.
;;
;; https://arxiv.org/html/2411.08684v1
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
  (hd/bind
   (map-indexed (fn [idx v] (ssp/fractional-power-encoding (bases idx) v)) p)))

;; =====================================

(comment

  ;; the prototype location (purple)
  (def p [6.2 -6.2 5.3])
  ;; preprosssing:

  ;; scaling-constant beta
  (def scaling-constant 10)

  (defn preprocess-color [hue saturation brightness]
    [(* (Math/cos hue) (/ saturation scaling-constant))
     (* (Math/sin hue) (/ saturation scaling-constant))
     (/ brightness scaling-constant)])

  ;; ---------------------------------------


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
  (hd/bind (hd/unbind c a) c))



;;
;; with x in hand, decode using a resonator network
;;

(defn decode
  "Returns a concept point in the domain D given a hypervector x.

  bases-codebooks: `k` code books with length of the desired resolution.


  "
  [x bases-codebooks])

;; resolution means step size
(defn bases-codebooks
  [{:keys [k resolution high low basis-seeds]}]
  (let [epsilon 1e-5
        values (torch/arange low (+ high epsilon) resolution :device *torch-device*)
        basis-seeds (or basis-seeds (hd/seed k))]
    (ssp/fractional-power-encoding basis-seeds values)))




(comment
  (py..
      (bases-codebooks {:k 3 :resolution 0.5 :high 10 :low -10})
      (size))
  ;; torch.Size([3, 41, 10000])
  ;; -----------

  (def a (hd/seed))

  (hd/similarity
   (ssp/fractional-power-encoding a 0)
   (hd/ones))


  ;; ------------------------
  ;; conrete example:
  ;; ------------------------
  (defonce bases-codebook (hd/seed 3))

  (def p-purple (preprocess-color (* 315 (/ Math/PI 180)) 87 53))
  (def p-blue   (preprocess-color (* 270 (/ Math/PI 180)) 1 5))
  (def p-orange (preprocess-color (* 30  (/ Math/PI 40)) 88 1))
  (def p-yellow (preprocess-color (* 73 (/ Math/PI 180)) 2 97))
  ;; ------------------------------------

  (let [bases (into [] bases-codebook)
        [purple blue orange]
        (mapv
         #(encode-point % bases)
         [p-purple p-blue p-orange])
        x (categorical-mapping purple blue orange)
        ]
    x))



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
