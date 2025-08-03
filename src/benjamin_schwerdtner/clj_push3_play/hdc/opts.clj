(ns benjamin-schwerdtner.clj-push3-play.hdc.opts)

(def ^:dynamic *default-opts*
  {:bsbc/N (long 1e4)
   :bsbc/block-count 20
   :bsbc/block-size (/ (long 1e4) 20)
   :hrr/dimensions (long 1e4)
   :fhrr/dimensions (long 1e4)
   :map/dimensions (long 1e4)})

(def ^:dynamic *torch-device* :cuda)
