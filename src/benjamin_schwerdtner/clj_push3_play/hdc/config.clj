(ns benjamin-schwerdtner.clj-push3-play.hdc.config
  (:require
   [libpython-clj2.require :refer [require-python]]
   [libpython-clj2.python :refer [py. py..] :as py]))

(require-python '[torch :as torch])

(def ^:dynamic *torch-device*
  (cond
    (py.. torch/cuda is_available)
    :cuda
    (py.. torch/mps is_available)
    :mps
    :else
    :cpu))

#_(def ^:dynamic *torch-device* :cpu)
