;; ------------------------------------
;; hm,,,,,,,,,,,,,









(ns benjamin-schwerdtner.clj-push3-play.hdc.ssp-grid
  (:require
   [benjamin-schwerdtner.clj-push3-play.hdc.spatial-semantic-pointer :as ssp]
   [benjamin-schwerdtner.clj-push3-play.hdc.fhrr :as fhrr]

   [libpython-clj2.require :refer [require-python]]
   [libpython-clj2.python :refer [py. py..] :as py]))

(require-python '[torch :as torch]
                '[torch.nn.functional :as F])

(defn encode-grid
  "Encode a 2D grid into a spatial-semantic-memory-2d.

   Args:
     grid: 2D tensor with elements ranging from e.g. 0-9
     color-codebook: tensor of HDVs corresponding to numbers e.g. 0-9
     [scale-x scale-y]: Default to grid size,
      say how close the cells of the grids lie together in the encoding.

  When the scale is [1 1], all points (>= [0 0]) in the encoding will have some similarity.

   "
  ([grid color-codebook]
   (encode-grid grid
                color-codebook
                (mapv dec (vec (py.. grid size)))))
  ([grid color-codebook [scale-x scale-y]]
   (let [[height width] (vec (py.. grid size))
         ;; Get numbers from grid and index from the codebook
         grid-flat (py.. grid (to :dtype torch/long) flatten)
         number-hdvs (py/get-item color-codebook grid-flat)]

     (ssp/location-codebook {:height scale-y
                             :resolution [(/ scale-x (+ (dec width) 0.01))
                                          (/ scale-y (+ (dec height) 0.01))]
                             :width scale-x})

     ;; -------------------------------------------------
     (-> (ssp/location-codebook {:height scale-y
                                 :resolution [(/ scale-x (+ (dec width) 0.01))
                                              (/ scale-y (+ (dec height) 0.01))]
                                 :width scale-x})
         :codebook
         (py.. (squeeze 1))
         (fhrr/bind number-hdvs)
         (fhrr/superposition)
         (fhrr/normalize)))))

(comment
  (def codebook (fhrr/seed 10))
  (vec
   (for [i (range 3)]
     (vec
      (for [j (range 3)]
        (py..
            (fhrr/cleanup-idx
             (fhrr/unbind
              (encode-grid
               (torch/tensor [[1 2 0]
                              [3 0 0]
                              [0 0 6]] :dtype torch/float)
               codebook)
              (ssp/spatial-semantic-pointer [i j]))
             codebook)
            item)))))
  [[1 2 0]
   [3 0 0]
   [0 0 6]])

(defn cleanup-as-grid
  [mem color-codebook [scale-x scale-y] [width height]]
  (let [positions (for [i (range height)
                        j (range width)]
                    [(* (/ i (- (+ width 0.01) 1)) scale-x)
                     (* (/ j (- (+ height 0.01) 1)) scale-y)])
        positions (py.. (ssp/spatial-semantic-pointer-many positions)
                        (view height width -1))]
    (py.. (torch/max (fhrr/similarity (fhrr/unbind mem positions)
                                      color-codebook)
                     :dim
                     -1)
          -indices)
    (torch/max (fhrr/similarity (fhrr/unbind mem positions)
                                color-codebook)
               :dim
               -1)))

(comment


  (encode-grid
   (torch/tensor [[1 2 4 8 8 4]
                  [3 4 4 8 8 4]
                  [4 4 6 7 7 4]] :dtype torch/float)
   codebook
   [1 1])


  (fhrr/similarity
   (fhrr/unbind
    (encode-grid
     (torch/tensor [[1 2 4]
                    [3 4 4]
                    [4 4 6]] :dtype torch/float)
     codebook
     [1 1])
    (ssp/spatial-semantic-pointer [0 0]))
   codebook)

  (ssp/readout
   (fhrr/unbind
    (encode-grid
     (torch/tensor [[1 2 4 8 8 4]
                    [3 4 4 8 8 4]
                    [4 4 6 7 7 4]] :dtype torch/float)
     codebook
     [1 1])
    (py/get-item codebook 8))
   {:width 1 :height 1})

  (fhrr/similarity
   (fhrr/unbind
    (encode-grid
     (torch/tensor [[1 2 4 8 8 4]
                    [3 4 4 8 8 4]
                    [4 4 6 7 7 4]] :dtype torch/float)
     codebook
     [1 1])
    (ssp/spatial-semantic-pointer [0 0]))
   codebook)






  (cleanup-as-grid
   (encode-grid
    (torch/tensor [[1 2 4 8 8 4]
                   [3 4 4 8 8 4]
                   [4 4 6 7 7 4]] :dtype torch/float)
    codebook
    [1 1])
   codebook
   [1 1]
   [1 3])

  (fhrr/similarity
   (fhrr/unbind
    (encode-grid
     (torch/tensor [[1 2 4 8 8 4]
                    [3 4 4 8 8 4]
                    [4 4 6 7 7 9]]
                   :dtype
                   torch/float)
     codebook
     [1 1])
    (ssp/spatial-semantic-pointer [1 1]))
   codebook)


  (vec (for [i (range 3)]
         (vec (for [j (range 3)]
                (py.. (fhrr/cleanup-idx
                       (fhrr/unbind
                        (encode-grid (torch/tensor [[1 2 0]
                                                    [3 0 0]
                                                    [0 0 6]]
                                                   :dtype
                                                   torch/float)
                                     codebook
                                     [1 2])
                        (ssp/spatial-semantic-pointer [1 2]))
                       codebook)
                  item)))))





  )
