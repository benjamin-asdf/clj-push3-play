(ns benjamin-schwerdtner.clj-push3-play.hdc.spatial-semantic-pointer
  (:require
   [benjamin-schwerdtner.clj-push3-play.hdc.fhrr :as fhrr]
   [benjamin-schwerdtner.clj-push3-play.hdc.config :refer [*torch-device*]]

   [benjamin-schwerdtner.clj-push3-play.hdc.fractional-power-encoding
    :refer [fractional-power-encoding]]



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

;; =========================================
;; SSP | Spatial Semantic Pointer
;; =========================================

(def x-marker (memoize (fn [] (fhrr/seed))))
(def y-marker (memoize (fn [] (fhrr/seed))))

;; 2d
(defn spatial-semantic-pointer
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
        M (fhrr/bind a (spatial-semantic-pointer p1))
        ;; M(objects,points) = superposition ( ... for obj in
        ;; objects bind(obj,point) )
        memory (fhrr/superposition
                ;; a at p1
                (fhrr/bind a (spatial-semantic-pointer p1))
                ;; b at p3
                (fhrr/bind b (spatial-semantic-pointer p3)))]
    ;; what is at point p1?
    ;;   ->    a
    [:what-is-at-point-p1

     (fhrr/dot-similarity (fhrr/unbind M
                                       (spatial-semantic-pointer p1))
                          (torch/cat [a b]))
     ;; 'mostly a' (funny!)
     :what-is-at-point-p2
     (fhrr/dot-similarity (fhrr/unbind M
                                       (spatial-semantic-pointer p2))
                          (torch/cat [a b]))

     ;; nothing
     :what-is-at-point-p3
     (fhrr/dot-similarity (fhrr/unbind M
                                       (spatial-semantic-pointer p3))
                          (torch/cat [a b]))
     ;; using the memory with b:
     ;;   ->  b
     :what-is-at-point-p3-mem
     (fhrr/dot-similarity (fhrr/unbind memory
                                       (spatial-semantic-pointer p3))
                          (torch/cat [a b]))])


  "
  [[x y]]
  (fhrr/bind (fractional-power-encoding (x-marker) x)
             (fractional-power-encoding (y-marker) y)))


(defn spatial-semantic-pointer-many
  "See [[spatial-semantic-pointer]] but `points` is a seq of positions.

  In the second form, xs and ys are torch tensors of positions."
  ([points]
   (spatial-semantic-pointer-many (torch/tensor (mapv first points)
                                                :device *torch-device*
                                                :dtype torch/float)
                                  (torch/tensor (mapv second points)
                                                :device *torch-device*
                                                :dtype torch/float)))
  ([xs ys]
   (let [x-powers (fractional-power-encoding
                   (py. (x-marker) unsqueeze 0)
                   (py. xs unsqueeze 1))
         y-powers (fractional-power-encoding
                   (py. (y-marker) unsqueeze 0)
                   (py. ys unsqueeze 1))]
     (fhrr/bind x-powers y-powers))))

(defn spatial-semantic-region-integral
  "Computes the integral representation of a continuous region R.

   Komer 2019:

     S(R) = ∫∫_{(x,y)∈R} X^x ⊗ Y^y dx dy    (5)

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
   (let [ ;; Generate random sample points within the region
         x-samples (torch/add (torch/mul (torch/rand [num-samples] :device *torch-device*)
                                         (- x-max x-min))
                              x-min)
         y-samples (torch/add (torch/mul (torch/rand [num-samples] :device *torch-device*)
                                         (- y-max y-min))
                              y-min)

         ;; Compute area of the region for proper scaling
         area (* (- x-max x-min) (- y-max y-min))

         ;; Bind X^x with Y^y for each sample point: element-wise multiplication
         integrand-samples (spatial-semantic-pointer-many
                            x-samples
                            y-samples)

         ;; Monte Carlo approximation: (area / num-samples) * sum of samples
         sum-samples (torch/sum integrand-samples :dim 0)
         scaling-factor (/ area num-samples)]

     ;; Scale by the area and number of samples
     (torch/mul sum-samples scaling-factor))))

(defn points-region
  "Returns a ssp representing the superposition of `points`."
  [points]
  (fhrr/normalize (fhrr/superposition (spatial-semantic-pointer-many
                                       points))))


(comment
  (let [p1 [0 0]
        p2 [0.2 0.2]
        p3 [5 5]
        a (fhrr/seed)
        b (fhrr/seed)
        ;; M = Obj ⊗ S
        M (fhrr/bind a (spatial-semantic-pointer p1))
        ;; M(objects,points) = superposition ( ... for obj in
        ;; objects bind(obj,point) )
        memory (fhrr/superposition
                ;; a at p1
                (fhrr/bind a (spatial-semantic-pointer p1))
                ;; b at p3
                (fhrr/bind b (spatial-semantic-pointer p3)))]
    ;; what is at point p1?
    [:what-is-at-point-p1
     (fhrr/dot-similarity (fhrr/unbind M
                                       (spatial-semantic-pointer p1))
                          (torch/cat [a b]))
     ;; 'mostly a' (funny!)
     :what-is-at-point-p2
     (fhrr/dot-similarity (fhrr/unbind M
                                       (spatial-semantic-pointer p2))
                          (torch/cat [a b]))
     ;; nothing
     :what-is-at-point-p3
     (fhrr/dot-similarity (fhrr/unbind M
                                       (spatial-semantic-pointer p3))
                          (torch/cat [a b]))
     ;; using the memory with b:
     :what-is-at-point-p3-mem
     (fhrr/dot-similarity (fhrr/unbind memory
                                       (spatial-semantic-pointer p3))
                          (torch/cat [a b]))]))

;; --------------------------------------------------------------

(defn spatial-semantic-memory
  "Returns a spatial semantic memory hd representing
  the objects standing for `obj-hdvs` at `points`.

  Query it using unbind.
  As with HD 'record', both key and value can be used to query.

  Both locations and objects can be queried to yield the loc / obj and vise versa.
  "
  [points obj-hdvs]
  (fhrr/normalize
   (fhrr/superposition
    (map
     (fn [s obj] (fhrr/bind s obj))
     (map spatial-semantic-pointer points)
     obj-hdvs))))





(comment
  (let [a (fhrr/seed)
        b (fhrr/seed)
        c (fhrr/seed)]
    (fhrr/cleanup-idx
     (query
      (spatial-semantic-memory
       [[0 0]
        [0.5 0.5]
        [1 1]
        [2 2]]
       [a a a b c])
      (spatial-semantic-region-integral [[0 1] [0 1]]))
     (torch/cat [a b c])))
  (let [a (fhrr/seed)
        b (fhrr/seed)
        c (fhrr/seed)]
    (fhrr/cleanup-idx
     (query
      (spatial-semantic-memory
       [[0 0]
        [0.5 0.5]
        [1 1]
        [2 2]]
       [a c b b c])
      (spatial-semantic-region-integral [[0.2 0.8] [0.2 0.8]]))
     (torch/cat [a b c]))))

(defn query
  "Like the query of a HD record.
  This is just [[unbind]].
  "
  [mem obj-or-region-location]
  (fhrr/unbind mem obj-or-region-location))

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
        (spatial-semantic-memory [p1 p2 p3]
                                 [a (fhrr/seed) b])]
    ;; memory
    [:what-is-at-point-p1
     (fhrr/dot-similarity (fhrr/unbind memory
                                       (spatial-semantic-pointer p1))
                          (torch/cat [a b]))
     ;; 'mostly a' (funny!)
     :what-is-at-point-p2
     (fhrr/dot-similarity (fhrr/unbind memory
                                       (spatial-semantic-pointer p2))
                          (torch/cat [a b])) :what-is-at-point-p3
     (fhrr/dot-similarity (fhrr/unbind memory
                                       (spatial-semantic-pointer p3))
                          (torch/cat [a b]))]))

;; ------------------------------

;; diserderata

;; Capacity
;; How many object can be encoded into M and a targed object succesfully decoded

;; Query single object
(defn query-object* [mem obj]
  (query mem obj))

(defn location-codebook
  "Returns a codebook of the grid position hdvs like by [[spatial-semantic-pointer]].
  "
  [{:keys [width height resolution]}]
  (let [width (or width 1)
        height (or height 1)
        [resolution-x resolution-y]
          (cond (number? resolution) [resolution resolution]
                (vector? resolution) resolution
                :else [0.1 0.1])
        x-coords (torch/arange 0
                               (+ width 0.01)
                               resolution-x
                               :device
                               *torch-device*)
        y-coords (torch/arange 0
                               (+ height 0.01)
                               resolution-y
                               :device
                               *torch-device*)
        [y-grid x-grid]
          (torch/meshgrid y-coords x-coords :indexing "ij")
        x-flat (py.. x-grid flatten)
        y-flat (py.. y-grid flatten)
        all-ssps (spatial-semantic-pointer-many x-flat y-flat)]
    {:codebook all-ssps :x-positions x-flat :y-positions y-flat}))

;; -----------------------------------------------
;; > This could be done with resonator,
;;   or https://arxiv.org/html/2412.00488v1
;;
;; -----------------------------------------------

(defn readout
  "Returns a [x y] position decoded from a 2d spatial semantic pointer ssp.

   This is a drop-in replacement for the original readout function
   that uses efficient tensor operations instead of map/for loops.

   Args:
     ssp: spatial semantic pointer to decode
     opts: map with keys:
       - :width - width of the space
       - :height - height of the space
       - :resolution - grid resolution (default 0.1)
       - :threshold - similarity threshold (default 0.1)

   Returns:
     [x y] position that best matches the input SSP
  "
  [ssp {:keys [width height resolution threshold] :as opts}]
  (let [{:keys [codebook y-positions x-positions]}
        (location-codebook opts)]
    (when-let [idx (fhrr/cleanup-idx ssp codebook (or threshold 0.1))]
      [(py.. (py/get-item x-positions idx) item)
       (py.. (py/get-item y-positions idx) item)])))


;;
;; NOTE that readout-multi is not part of the diserdarata for
;; ssp - mem
;;

(defn readout-multi
  "Decodes multiple [x y] positions from a 2d spatial semantic pointer `ssp`.

   This is useful when the SSP is a superposition of multiple locations,
   such as when querying for an object that appears at multiple positions.

   Args:
     ssp: spatial semantic pointer (potentially a superposition)
     opts: map with keys:
       - :width - width of the space
       - :height - height of the space
       - :resolution - grid resolution (default 0.1)
       - :threshold - similarity threshold (default 0.1)

   Returns:
     Vector of [x y] positions where similarity exceeds threshold,
     or empty vector if no positions found.
  "
  [ssp {:keys [width height resolution threshold]}]
  (let [threshold (or threshold 0.1)
        resolution (or resolution 0.1)
        {:keys [codebook y-positions x-positions]}
        (location-codebook
         {:height height :resolution resolution :width width})
        similarities (py.. (fhrr/similarity ssp codebook) (squeeze))
        above-threshold (torch/gt similarities threshold)
        idxs (torch/nonzero above-threshold)]
    (torch/cat [(py/get-item x-positions idxs)
                (py/get-item y-positions idxs)]
               :dim
               1)))


;;
;; Query missing object
;; Indicate if an object is not present when queried
;;
;; Query duplicate object
;; Returns the superposition of all locations of obj.
(defn query-object
  "Returns a ssp representing the location of `obj` in `mem`.

  In the first form, query the location hdv raw.

  In the second form,
  reuturns nil, if the object is not present.

  otherwise returns data:

  returns {:location location-ssp :redout-pos [x y]}

  Multi object case:

  Observe that location-ssp is the superposition
  of the locations of the object.

  "
  ([mem obj] (query-object* mem obj))
  ([mem obj {:keys [width height resolution threshold] :as opts}]
   (let [loc (query-object* mem obj)
         pos (readout loc opts)]
     (when pos {:location loc :redout-pos pos}))))

(comment
  (let [a (fhrr/seed)
        b (fhrr/seed)
        c (fhrr/seed)
        mem (spatial-semantic-memory [[0 0] [0.5 0.5] [1 1] [2 2] [3 3]]
                                     [a b a b c])
        {:keys [location readout-pos]}
        (query-object mem a {:height 3 :width 3})]
    (readout-multi location
                   {:height 3 :resolution 0.5 :width 3
                    :threshold 0.36}))
  ;; with lower threshold also returns everything in between.

  ;; tensor([[0., 0.],
  ;;       [1., 1.]])


  (let [a (fhrr/seed)
        b (fhrr/seed)
        c (fhrr/seed)
        mem (spatial-semantic-memory [[0 0] [0.5 0.5] [1 1] [2 2] [3 3]]
                                     [a b a b c])
        {:keys [location readout-pos]}
        (query-object mem a {:height 3 :width 3})]
    (readout-multi location
                   {:height 3 :resolution 0.5 :width 3}))
  ;; tensor
  ;; ([[0.0000, 0.0000],
  ;;   [0.5000, 0.0000],
  ;;   [0.0000, 0.5000],
  ;;   [0.5000, 0.5000],
  ;;   [1.0000, 0.5000],
  ;;   [1.5000, 0.5000],
  ;;   [0.5000, 1.0000],
  ;;   [1.0000, 1.0000],
  ;;   [1.5000, 1.0000],
  ;;   [0.5000, 1.5000],
  ;;   [1.0000, 1.5000],
  ;;   [1.5000, 1.5000]])

  ;; is logical.
  ;; readout-multi is not super useful Ig.
  )



;; Query location
#_(defn query-location [mem point]
    (query mem (spatial-semantic-pointer point)))

(defn query-location
  "Returns an hdv representing the object at location [x y]
  on spatial semantic memory `mem`."
  [mem [x y]]
  (fhrr/bind mem (spatial-semantic-pointer [(- x) (- y)])))

(comment
  (let [a (fhrr/seed)
        b (fhrr/seed)
        c (fhrr/seed)]
    (fhrr/cleanup-idx (query-location
                       (spatial-semantic-memory
                        [[0 0] [0.5 0.5] [1 1] [2 2]]
                        [a b b c])
                       [1 1])
                      (torch/cat [a b c]))))

;; region represenation
(def region spatial-semantic-region-integral)

;; Query Region
(defn query-region [mem region-bounds]
  (query mem (region region-bounds)))

;; Shif single object in Group
;; Change the position of a single object without decoding M

(defn shift
  "Returns a new spatial semantic memory
  where the position of `obj` is shifted by [dx dy].

  The rest of mem is preserved.
  "
  [mem obj [dx dy] {:keys [width height resolution] :as opts}]
  (let [{:keys [codebook]} (location-codebook opts)
        ;; Is not possible without a location cleanup?
        loc (fhrr/cleanup (query mem obj) codebook)
        l*obj (fhrr/bind loc obj)
        delta-ssp (spatial-semantic-pointer [dx dy])
        delta-mem (fhrr/bind l*obj delta-ssp)]
    (-> (fhrr/superposition [mem (fhrr/negative l*obj) delta-mem])
        (fhrr/normalize))))

(comment

  (let [hdvs
        {:a (fhrr/seed) :b (fhrr/seed) :c (fhrr/seed) :d (fhrr/seed)}
        positions {:a [0 0] :b [0 1] :c [0.5 0.5] :d [1 1]}
        mem (spatial-semantic-memory (vals positions) (vals hdvs))
        opts {:height 2 :resolution 0.5 :width 2}
        mem-shifted (shift mem (:b hdvs) [1 1] opts)]
    (into []
          (for [[k v] (seq positions)]
            (let [a (get hdvs k)
                  pos (get positions k)]
              [:k k :pos pos :shifted-pos
               (readout (query-object mem-shifted a) opts)]))))
  [[:k :a :pos [0 0] :shifted-pos [0.0 0.0]]
   [:k :b :pos [0 1] :shifted-pos [1.0 2.0]]
   [:k :c :pos [0.5 0.5] :shifted-pos [0.5 0.5]]
   [:k :d :pos [1 1] :shifted-pos [1.0 1.0]]]

  )

;; Shift whole group
;; Change the position of all objects in M similarly
(defn shift-group
  "Returns a spatial semantic memory representing `mem` shifted by
  [dx dy].
  "
  [mem [dx dy]]
  ;;
  ;; Utilizing
  ;;
  ;; eqn. 3 Komer 2019:
  ;; x-marker^k1 ⊙ x-marker^k2 = x-marker^(k1 + k2)
  ;;
  ;;
  ;;
  (let [delta-ssp (spatial-semantic-pointer [dx dy])]
    (fhrr/bind mem delta-ssp)))

(comment

  (let [a (fhrr/seed)
        b (fhrr/seed)
        c (fhrr/seed)
        d (fhrr/seed)
        mem (spatial-semantic-memory [[0 0] [0 1] [0.5 0.5] [1 1]]
                                     [a b c d])]
    [(readout (query-object* (shift-group mem [1 1]) a)
              {:height 2 :resolution 0.2 :threshold 0.1 :width 2})
     (readout (query-object* (shift-group mem [1 1]) b)
              {:height 2 :resolution 0.2 :threshold 0.1 :width 2})
     (readout (query-object* (shift-group mem [1 1]) c)
              {:height 2 :resolution 0.2 :threshold 0.1 :width 2})
     (readout (query-object* (shift-group mem [1 1]) d)
              {:height 2 :resolution 0.2 :threshold 0.1 :width 2})])
  [[1.0 1.0] [1.0 2.0] [1.600000023841858 1.399999976158142] [2.0 2.0]])


;; Readout (x,y) location from SSP

#_(defn readout
    "Returns a [x y] position decoded from a 2d spatial semantic pointer ssp."
    [ssp [width height] resolution]
    (let [positions
          (into []
                cat
                (for [x (range 0 width resolution)]
                  (for [y (range 0 height resolution)] [x y])))
          codebook
          (torch/stack (mapv spatial-semantic-pointer positions))]
      (nth
       positions
       (fhrr/cleanup-idx ssp codebook))))

;; Removed duplicate readout function - using the one defined earlier

(comment
  (readout (spatial-semantic-pointer [0.5 0.5]) {:width 2 :height 2 :resolution 0.2})
  [0.4000000059604645 0.4000000059604645]
  (time (readout
         (spatial-semantic-pointer [0.5 0.5])
         {:width 2 :height 2 :resolution 0.1}))
  [0.5 0.5]
  ;; It is dangerous to use small resolution. OOM !!
  )

(comment
  (fhrr/similarity
   (points-region [[0 0] [1 0] [3 3]])
   (spatial-semantic-pointer [1 0]))

  (fhrr/similarity
   (points-region [[0 0] [1 0] [3 3]])
   (spatial-semantic-pointer [0.5 0])))
