(ns benjamin-schwerdtner.clj-push3-play.hdc.spatial-semantic-pointer
  (:require
   [benjamin-schwerdtner.clj-push3-play.hdc.fhrr :as fhrr]
   [libpython-clj2.require :refer [require-python]]
   [libpython-clj2.python :refer [py. py..] :as py]))

(require-python '[torch :as torch]
                '[torch.nn.functional :as F])

;;
;; A neural representation of continuous space using fractional binding
;;
;;
;; Brent Komer, Terrence C. Stewart,
;; Aaron R. Voelker, and Chris Eliasmith. A neural repre-
;; sentation of continuous space using fractional binding. In
;; Annual Meeting of the Cognitive Science Society, 2019.

;;
;; Problem: Represent continous spaces such as maps, surfaces, grid cells, ...
;;
;;
;; Compared to non continous representations: Maps, graphs, trees, etc. [data.clj]
;;

;; -----------------------------------------

;; Consider b^k:

;; This can used as a list position marker, discrete encoding.
;; Example: Choo & Eliasmith 2010
(defn power
  ([x k] (power x k x))
  ([x k acc]
   (if (zero? k) acc (recur x (dec k) (fhrr/bind acc x)))))

(comment
  (let [sequence (fn [& elements]
                   (fhrr/superposition
                    (torch/cat (vec (map-indexed (fn [idx e] (power e idx)) elements)))))
        a (fhrr/seed)
        b (fhrr/seed)
        c (fhrr/seed)
        d (fhrr/seed)
        xs (sequence a b c d)]
    [(fhrr/dot-similarity a xs)
     (fhrr/dot-similarity (fhrr/bind b b) xs)
     (fhrr/dot-similarity (fhrr/bind (fhrr/bind c c) c) xs)
     (fhrr/dot-similarity (fhrr/bind (fhrr/bind (fhrr/bind d d) d) d) xs)
     (fhrr/dot-similarity b xs)
     (fhrr/dot-similarity c xs)]))

;; ------------------------

;; Idea: allow `k` to be real,
;; so that x^k = power(x,k) encodes continous quantity

(defn fractional-power-encoding
  "Computes the fractional power of a hypervector x raised to power k.
   For FHRR, this is done by multiplying the phase angles by k.

   This implements x^k where k can be any real number.

   Args:
     x: FHRR hypervector (complex tensor)
     k: real number exponent

   Returns:
     FHRR hypervector representing x^k



(let [a (fhrr/seed)
      b (fhrr/seed)]
     [
      ;; pretty cool effect: A smooth similarity :

      ;; 0          |  0-1                    | 1          | 1-2       | 2
      ;; dissimilar | smoothly more similiar  | identical  | dito less | dissimilar
      (into []
            (py..
                (torch/cat
                 (vec
                  (for [k (range 0 2.2 0.1)]
                    (fhrr/dot-similarity a (fractional-power-encoding a k)))))
                flatten
                tolist))])

[[-66.85767364501953 1037.0406494140625 2293.38232421875
  3643.3388671875 5018.9560546875 6347.482421875 7556.1494140625
  8577.060546875 9351.818359375 9835.544921875

  10000.0

  9835.544921875
  9351.8173828125 8577.060546875 7556.1494140625 6347.482421875
  5018.95556640625 3643.337890625 2293.382568359375 1037.0408935546875
  -66.8578109741211 -972.173095703125]]

  "
  [x k]
  ;; Get the phase angles of the complex numbers
  (let [angles (torch/angle x)
        ;; Multiply angles by k
        scaled-angles (torch/mul angles k)
        ;; Convert back to complex form
        result (torch/complex (torch/cos scaled-angles)
                              (torch/sin scaled-angles))]
    result))


;; ------------------------------------------------------
;; with this in hand:

;; SSP | Spatial Semantic Pointer

(def x-marker (memoize (fn [] (fhrr/seed))))
(def y-marker (memoize (fn [] (fhrr/seed))))

(defn spatial-semantic-pointer-2d
  "Returns a spatical semantic pointer (SSP) for
  cartesian point [x y].

  This can be used to encode a position in HD/VSA.

  x and y are real numbers.

  Usage represent locations / regions:



   (let [p1 [0 0]
        p2 [0.2 0.2]
        p3 [5 5]
        a (fhrr/seed)
        b (fhrr/seed)
        ;; M = Obj ⊗ S
        M (fhrr/bind a (spatial-semantic-pointer-2d p1))
        ;; M(objects,points) = superposition ( ... for obj in
        ;; objects bind(obj,point) )
        memory (fhrr/superposition
                ;; a at p1
                (fhrr/bind a (spatial-semantic-pointer-2d p1))
                ;; b at p3
                (fhrr/bind b (spatial-semantic-pointer-2d p3)))]
    ;; what is at point p1?
    ;;   ->    a
    [:what-is-at-point-p1

     (fhrr/dot-similarity (fhrr/unbind M
                                       (spatial-semantic-pointer-2d p1))
                          (torch/cat [a b]))
     ;; 'mostly a' (funny!)
     :what-is-at-point-p2
     (fhrr/dot-similarity (fhrr/unbind M
                                       (spatial-semantic-pointer-2d p2))
                          (torch/cat [a b]))

     ;; nothing
     :what-is-at-point-p3
     (fhrr/dot-similarity (fhrr/unbind M
                                       (spatial-semantic-pointer-2d p3))
                          (torch/cat [a b]))
     ;; using the memory with b:
     ;;   ->  b
     :what-is-at-point-p3-mem
     (fhrr/dot-similarity (fhrr/unbind memory
                                       (spatial-semantic-pointer-2d p3))
                          (torch/cat [a b]))])


  "
  [[x y]]
  (fhrr/bind
   (fractional-power-encoding (x-marker) x)
   (fractional-power-encoding (y-marker) y)))

(defn spatial-semantic-region [& points]
  (fhrr/superposition
   (map spatial-semantic-pointer-2d points)))

;; representing an object occupying a location or region:

(comment
  (let [p1 [0 0]
        p2 [0.2 0.2]
        p3 [5 5]
        a (fhrr/seed)
        b (fhrr/seed)
        ;; M = Obj ⊗ S
        M (fhrr/bind a (spatial-semantic-pointer-2d p1))
        ;; M(objects,points) = superposition ( ... for obj in
        ;; objects bind(obj,point) )
        memory (fhrr/superposition
                ;; a at p1
                (fhrr/bind a (spatial-semantic-pointer-2d p1))
                ;; b at p3
                (fhrr/bind b (spatial-semantic-pointer-2d p3)))]
    ;; what is at point p1?
    [:what-is-at-point-p1
     (fhrr/dot-similarity (fhrr/unbind M
                                       (spatial-semantic-pointer-2d p1))
                          (torch/cat [a b]))
     ;; 'mostly a' (funny!)
     :what-is-at-point-p2
     (fhrr/dot-similarity (fhrr/unbind M
                                       (spatial-semantic-pointer-2d p2))
                          (torch/cat [a b]))
     ;; nothing
     :what-is-at-point-p3
     (fhrr/dot-similarity (fhrr/unbind M
                                       (spatial-semantic-pointer-2d p3))
                          (torch/cat [a b]))
     ;; using the memory with b:
     :what-is-at-point-p3-mem
     (fhrr/dot-similarity (fhrr/unbind memory
                                       (spatial-semantic-pointer-2d p3))
                          (torch/cat [a b]))]))


;; --------------------------------------------------------------

(defn spatial-semantic-memory-2d
  "Returns a spatial semantic memory hd representing
  the objects standing for `obj-hdvs` at `points`.

  "
  [points obj-hdvs]
  (fhrr/superposition
   (map
    (fn [s obj] (fhrr/bind s obj))
    (map spatial-semantic-pointer-2d
         points)
    obj-hdvs)))

;; ---------------------------------------

(comment
  (let [p1 [0 0]
        p2 [0.2 0.2]
        p3 [5 5]
        a (fhrr/seed)
        b (fhrr/seed)
        memory (spatial-semantic-memory-2d [p1 p2 p3]
                                           [a (fhrr/seed) b])]
    [:what-is-at-point-p1
     (fhrr/dot-similarity (fhrr/unbind memory
                                       (spatial-semantic-pointer-2d p1))
                          (torch/cat [a b]))
     ;; 'mostly a' (funny!)
     :what-is-at-point-p2
     (fhrr/dot-similarity (fhrr/unbind memory
                                       (spatial-semantic-pointer-2d p2))
                          (torch/cat [a b])) :what-is-at-point-p3
     (fhrr/dot-similarity (fhrr/unbind memory
                                       (spatial-semantic-pointer-2d p3))
                          (torch/cat [a b]))]))
