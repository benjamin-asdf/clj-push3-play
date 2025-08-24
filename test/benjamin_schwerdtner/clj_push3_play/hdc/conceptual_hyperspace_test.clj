(ns benjamin-schwerdtner.clj-push3-play.hdc.conceptual-hyperspace-test
  (:require
   [clojure.test :as t]
   [benjamin-schwerdtner.clj-push3-play.hdc.fhrr :as hd]
   [benjamin-schwerdtner.clj-push3-play.hdc.data :as hdd]
   [benjamin-schwerdtner.clj-push3-play.hdc.spatial-semantic-pointer :as ssp]
   [benjamin-schwerdtner.clj-push3-play.hdc.config :refer [*torch-device*]]

   [benjamin-schwerdtner.clj-push3-play.hdc.resonator :as resonator]
   [benjamin-schwerdtner.clj-push3-play.hdc.fractional-power-encoding :as fpe]

   [benjamin-schwerdtner.clj-push3-play.hdc.conceptual-hyperspace
    :as ch]

   [libpython-clj2.python :refer [py. py..] :as py]
   [libpython-clj2.require :refer [require-python]]))

(require-python '[torch :as torch]
                '[torch.nn.functional :as F])

(t/deftest hyperspace-test
  (doall
   (for [res (range 0.1 1 0.1)]
     (let [B (ch/bases-codebooks
              {:high 10 :k 3 :low -10 :resolution res})
           b (hd/bind [(fpe/fpe (first (:basis-seeds B)) 10.0)
                       (fpe/fpe (second (:basis-seeds B)) 2.2)
                       (fpe/fpe (first (rest (rest (:basis-seeds B))))
                                9.0)])
           outcome (ch/decode b B)]
       (t/is
        (< (py.. (torch/sum (torch/sub (torch/tensor [10.0 2.2 9.0]) outcome)) (item)) 1)
        "roughly factorizes")))))

;; ---------

(t/deftest categorical-mapping-example-test
  (let [p-purple [6.2 -6.2 5.3]
        p-blue [0 -10 5]
        p-orange [6.7 5.7 10]
        p-yellow [0.6 1.8 9.7]
        B (ch/bases-codebooks
           {:high 10 :k 3 :low -10 :resolution 0.1})]
    (let [[purple blue orange yellow]
          (mapv #(ch/encode-point % (:basis-seeds B))
                [p-purple p-blue p-orange p-yellow])
          x (ch/categorical-mapping purple blue orange)]
      (t/is (hd/similar? x yellow 0.9)
            "analogical mapping finds yellow")
      (t/is (< (py.. (torch/sum (torch/sub (ch/decode x B)
                                           (torch/tensor p-yellow)))
                 item)
               0.3)
            "decode yields yellow"))))
