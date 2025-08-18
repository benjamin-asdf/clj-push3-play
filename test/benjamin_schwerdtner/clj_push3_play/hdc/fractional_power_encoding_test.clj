(ns benjamin-schwerdtner.clj-push3-play.hdc.fractional-power-encoding-test
  (:require
   [clojure.test :as t]
   [benjamin-schwerdtner.clj-push3-play.hdc.fhrr :as hd]
   [libpython-clj2.require :refer [require-python]]
   [libpython-clj2.python :refer [py. py..] :as py]

   [benjamin-schwerdtner.clj-push3-play.hdc.fractional-power-encoding :as fpe]))

(require-python '[torch :as torch]
                '[torch.nn.functional :as F])

(t/deftest test-fpe
  (t/testing "invariants"
    (let [a (hd/seed)]
      (t/is (torch/allclose (hd/bind a a) (fpe/fpe a 2))
            "a ⊙ a = a^2")
      (t/is (torch/allclose a (fpe/fpe a 1)) "a = a^1")
      (t/is (torch/allclose (hd/ones) (fpe/fpe a 0)) "a = a^0")))
  (t/testing "sims"
    (let [a (hd/seed)]
      (t/is (< (py.. (hd/similarity a (fpe/fpe a 1.2)) item)
               (py.. (hd/similarity a (fpe/fpe a 1.1)) item))
            "closer to a")
      (t/is (= 1.0
               (py.. (hd/similarity (fpe/fpe a 1.5) (fpe/fpe a 1.5))
                     item))
            "same")
      (t/is (< 0.5
               (py.. (hd/similarity (fpe/fpe a 2.0) (fpe/fpe a 1.5))
                     item)
               1.0)
            "in between")
      (t/is (< 0.5
               (py.. (hd/similarity (fpe/fpe a 2.0) (fpe/fpe a 1.5))
                     item)
               1.0)
            "in between")
      (t/is
        ;; << 1.0
        (< (py.. (hd/similarity (fpe/fpe a 2.5) (fpe/fpe a 1.5)) item)
           0.1)
        "in between"))))
