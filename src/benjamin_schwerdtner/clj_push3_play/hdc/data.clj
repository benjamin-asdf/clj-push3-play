(ns benjamin-schwerdtner.clj-push3-play.hdc.data
  (:require [libpython-clj2.require :refer [require-python]]
            [libpython-clj2.python :refer [py. py..] :as py]
            [benjamin-schwerdtner.clj-push3-play.hdc.config :refer
             [*torch-device*]]
            [benjamin-schwerdtner.clj-push3-play.hdc.fhrr :as hd]))

(require-python '[torch :as torch])

;; ----------------

(defn bind-seq
  "Returns a hd representing a seq of hdvs using binds.
  The position of a vector in the seq is signaled by it's permute count.
  "
  [hdvs]
  (hd/bind
   (map-indexed (fn [idx hd] (hd/permute hd idx)) hdvs)))

(defn bind-sequence
  "Returns a hd representing a seq of hdvs using binds.
  The position of a vector in the seq is signaled by it's permute count.
  "
  [& hdvs]
  (bind-seq hdvs))


(comment
  (let [a (hd/seed) b (hd/seed)]
    (hd/similarity a (hd/permute a 0))
    (hd/similarity (hd/unbind
                    (bind-seq a b)
                    (hd/permute b 1)) [a b]))
  ;;


  )
