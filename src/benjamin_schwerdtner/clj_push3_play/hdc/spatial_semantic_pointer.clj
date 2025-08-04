(ns benjamin-schwerdtner.clj-push3-play.hdc.spatial-semantic-pointer
  (:require
   [benjamin-schwerdtner.clj-push3-play.hdc.fhrr :as fhrr]
   [benjamin-schwerdtner.clj-push3-play.hdc.config :refer [*torch-device*]]
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
;; Problem: Represent continous spaces such as maps, surfaces, grid cells(?), ...
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
                    (map-indexed (fn [idx e] (power e idx)) elements)))
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

   This implements x^k where k can be any real number or tensor of real numbers.

   Args:
     x: FHRR hypervector (complex tensor) with shape [..., dimensions]
     k: real number exponent or tensor of exponents
        - If scalar: applies same power to all elements of x
        - If tensor: must be broadcastable with x
        - Common use cases:
          * Scalar k: same power for entire vector
          * Tensor k with shape [1]: same as scalar
          * Tensor k with compatible shape for broadcasting

   Returns:
     FHRR hypervector representing x^k with same shape as x

   Examples:
     ;; Scalar exponent - same power for all elements
     (fractional-power-encoding x 2.5)

     ;; Tensor with single value - equivalent to scalar
     (fractional-power-encoding x (torch/tensor [2.5]))


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

  ------------------------------


  Also it holds:

  1)  ∀(a,b)∈H: (a ⊗ b) ⊗ fractional-power-encoding(a, -1) = (a ⊗ b) ⊘ a = b

  where
  ⊗ is [[bind]]
  ⊘ is [[unbind]].

  2)  ∀(a)∈H:  fractional-power-encoding(a, -1) = inverse(a).

  Strictly speaking this is approximate, but when the vectors are unitary, like by [[fhrr/seed]],
  it is the true inverse.

  "
  [x k]
  ;; Convert k to tensor if needed, with intelligent shape
  ;; handling
  (let [k-tensor
        (cond (number? k) (torch/tensor [k]
                                        :device *torch-device*
                                        :dtype torch/float32)
              (coll? k) (torch/tensor (vec k)
                                      :device *torch-device*
                                      :dtype torch/float32)
              :else (py.. k (to :dtype torch/float32)))
        ;; Get the phase angles of the complex numbers
        angles (torch/angle x)
        k-tensor (py.. k-tensor (unsqueeze 1))
        scaled-angles (torch/mul angles k-tensor)
        result (torch/complex (torch/cos scaled-angles)
                              (torch/sin scaled-angles))]
    result))

(comment
  (torch/allclose (x-marker) (fractional-power-encoding (x-marker) 0))
  (torch/allclose (x-marker) (fractional-power-encoding (x-marker) 1))
  (fractional-power-encoding
   (torch/stack
    [(x-marker)
     (x-marker)])
   [0 0.1 0.5 1])

  ;; on GPU:
  ;; "Elapsed time: 1.680106 msecs"
  ;; on CPU:
  ;; "Elapsed time: 22.332514 msecs"

  (time
   (fractional-power-encoding
    (x-marker)
    (torch/tensor (vec (range 0 1 0.001)) :device *torch-device*))))

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

(defn spatial-semantic-region-integral
  "Computes the integral representation of a continuous region R.

   Komer 2019 equation (5):

   S(R) = ∫∫_{(x,y)∈R} X^x ⊗ Y^y dx dy

   Where:
   - X and Y are the basis vectors (x-marker and y-marker)
   - ⊗ is the bind operation (element-wise multiplication)
   - The integral is approximated using Monte Carlo sampling

   Args:
     region-bounds: [[x-min x-max] [y-min y-max]] - rectangular bounds of region
     num-samples: number of sample points for Monte Carlo integration (default 1000)

   Returns:
     FHRR hypervector representing the integrated region

   Example:
     ;; Integrate over a unit square [0,1] x [0,1]
     (spatial-semantic-region-integral [[0 1] [0 1]] 1000)

     ;; Integrate over a rectangle [-2,2] x [-1,1]
     (spatial-semantic-region-integral [[-2 2] [-1 1]] 2000)
   "
  ([region-bounds] (spatial-semantic-region-integral region-bounds 100))
  ([[[x-min x-max] [y-min y-max]] num-samples]
   (let [;; Generate random sample points within the region
         x-samples (torch/add (torch/mul (torch/rand [num-samples] :device *torch-device*)
                                         (- x-max x-min))
                              x-min)
         y-samples (torch/add (torch/mul (torch/rand [num-samples] :device *torch-device*)
                                         (- y-max y-min))
                              y-min)

         ;; Compute area of the region for proper scaling
         area (* (- x-max x-min) (- y-max y-min))

         ;; Compute the integrand for each sample point efficiently using batch processing
         ;; X^x for all samples at once
         x-powers (fractional-power-encoding
                   (py. (x-marker) unsqueeze 0) ; Add batch dimension [1, dimensions]
                   (py. x-samples unsqueeze 1)) ; Shape [num-samples, 1] for broadcasting

         ;; Y^y for all samples at once
         y-powers (fractional-power-encoding
                   (py. (y-marker) unsqueeze 0) ; Add batch dimension [1, dimensions]
                   (py. y-samples unsqueeze 1)) ; Shape [num-samples, 1] for broadcasting

         ;; Bind X^x with Y^y for each sample point: element-wise multiplication
         integrand-samples (torch/mul x-powers y-powers)

         ;; Monte Carlo approximation: (area / num-samples) * sum of samples
         sum-samples (torch/sum integrand-samples :dim 0)
         scaling-factor (/ area num-samples)]

     ;; Scale by the area and number of samples
     (torch/mul sum-samples scaling-factor))))

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

  Query it using unbind.
  As with HD 'record', both key and value can be used to query.
  I.e. both objects and locations/ regions.

  "
  [points obj-hdvs]
  (fhrr/superposition
   (map
    (fn [s obj] (fhrr/bind s obj))
    (map spatial-semantic-pointer-2d points)
    obj-hdvs)))

(defn encode-grid
  "Encode a 2D grid into a spatial-semantic-memory-2d.

   Args:
     grid: 2D tensor with elements ranging from e.g. 0-9
     color-codebook: tensor of HDVs corresponding to numbers e.g. 0-9

   Returns:
     Map containing:
       :memory - the spatial semantic memory
       :positions - list of [x,y] coordinates (0-1 normalized)
       :numbers - list of numbers from the grid
       :number-hdvs - corresponding HDVs from color-codebook
       :grid-size - [height, width] of the original grid
   "
  [grid color-codebook]
  (let [ ;; Get grid dimensions
        grid-shape (vec (py.. grid size))
        [height width] grid-shape

        ;; Create normalized coordinates between 0 and 1
        ;; For a grid of size HxW, positions are:
        ;; x: 0/(W-1), 1/(W-1), ..., (W-1)/(W-1)
        ;; y: 0/(H-1), 1/(H-1), ..., (H-1)/(H-1)
        x-coords (if (= width 1)
                   [0.5]                ; Single column case
                   (mapv #(/ % (- width 1)) (range width)))
        y-coords (if (= height 1)
                   [0.5]                ; Single row case
                   (mapv #(/ % (- height 1)) (range height)))

        ;; Generate all grid positions as [x,y] pairs
        positions (for [i (range height)
                        j (range width)]
                    [(nth x-coords j) (nth y-coords i)])

        ;; Flatten grid to get corresponding numbers
        grid-flat (py.. grid (to :dtype torch/long) flatten)

        number-hdvs (py/get-item color-codebook grid-flat)
        memory (spatial-semantic-memory-2d positions number-hdvs)]
    memory))



(comment
  (def codebook (fhrr/seed 10))
  (def mem (encode-grid
            (torch/tensor
             [[1 2 3]
              [1 2 3]]
             :dtype torch/float)
            codebook))

  (py/get-item
   (torch/tensor [10 20 30 40])
   (torch/tensor [1 2 3]))

  (py..
      (fhrr/dot-similarity
       (fhrr/cleanup
        (fhrr/unbind
         mem
         (spatial-semantic-pointer-2d [0 0]))
        codebook)
       codebook)
      (to :dtype torch/int)))

(comment

  (let [a (fhrr/seed)
        b (fhrr/seed)
        c (fhrr/seed)]
    (spatial-semantic-memory-2d
     [[0 0] [1 1] [2 2]]
     [a b c]))

  (let [codebook (fhrr/seed 10)
        lut (atom {})
        nextv (fn [] (py/get-item codebook (count @lut)))]
    (def vsa-obj
      (fn [o]
        (when-not (get @lut o)
          (swap! lut conj [o (nextv)]))
        (get @lut o))))

  (defn encode-grid [grid]

    (for [[i row] (map-indexed vector grid)]
      (for [[j elm] (map-indexed vector row)]
        ;; point
        [i j]
        ;; obj
        (vsa-obj elm))))

  (def grid (torch/tensor [[0 1 2] [0 1 2] [0 1 2]]))

  (encode-grid grid))

(defn query
  "Like the query of a HD record.
  This is just [[unbind]].
  "
  [mem obj-or-region-location]
  (fhrr/unbind mem obj-or-region-location))

(defn query-location [mem [x y]]
  (fhrr/bind
   mem
   (spatial-semantic-pointer-2d [(- x) (- y)])))

;; --------------------

(defn query-region [mem points])

(comment
  (let [a (fhrr/seed)
        b (fhrr/seed)]
    (fhrr/dot-similarity
     (fhrr/bind (fhrr/bind a b) (fractional-power-encoding a -1))
     (fhrr/unbind (fhrr/bind a b) a)))
  ;; tensor([[10000.]], device='cuda:0')
  )

;; ---------------------------------------

(comment
  (let [p1 [0 0]
        p2 [0.2 0.2]
        p3 [5 5]
        a (fhrr/seed)
        b (fhrr/seed)
        memory
        (spatial-semantic-memory-2d [p1 p2 p3]
                                    [a (fhrr/seed) b])]
    ;; memory
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
