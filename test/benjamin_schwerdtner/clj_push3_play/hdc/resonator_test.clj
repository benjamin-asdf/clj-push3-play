(ns benjamin-schwerdtner.clj-push3-play.hdc.resonator-test
  (:require
   [libpython-clj2.python :refer [py. py..] :as py]
   [libpython-clj2.require :refer [require-python]]
   [benjamin-schwerdtner.clj-push3-play.hdc.fhrr :as hd]
   [benjamin-schwerdtner.clj-push3-play.hdc.config :refer [*torch-device*]]
   [clojure.test :as t]
   [benjamin-schwerdtner.clj-push3-play.hdc.resonator :as resonator]))

(require-python '[torch :as torch])

(def reference-impl resonator/exhaustive-search-factorize)

(t/deftest resonator-test
  (binding [*torch-device* :cpu]
    (t/testing "reference impl"
      (doseq [trial (range 2)]
        (let [books (torch/stack [(hd/seed 10) (hd/seed 10)
                                  (hd/seed 10)])
              [a b c] (mapv (fn [b] (py/get-item b (rand-int 10)))
                        books)
              x (hd/bind [a b c])
              factors (reference-impl x books)]
          (t/is (torch/allclose (torch/stack [a b c])
                                (torch/stack (vec factors)))
                (str "Ref impl factorizes " trial)))))
    (doseq [trial (range 2)]
      (t/testing
        (let [books (torch/stack [(hd/seed 10) (hd/seed 10)
                                  (hd/seed 10)])
              [a b c] (mapv (fn [b] (py/get-item b (rand-int 10)))
                        books)
              x (hd/bind [a b c])
              factors-ref (vec (reference-impl x books))
              factors (:factors (resonator/resonator-fhrr x books))]
          (t/is (torch/allclose (torch/stack factors-ref)
                                (torch/stack factors))
                (str "Factorizes " trial)))))))



(comment
  ;; book-size = 50, factors = 3 is well within operational capacity.
  (for [book-size [10 15 20 40 50 100 150]]
    (for [trial (range 20)]
      (let [books (torch/stack [(hd/seed book-size)
                                (hd/seed book-size)
                                (hd/seed book-size)])
            [a b c] [(py/get-item books [0 -1])
                     (py/get-item books [1 -1])
                     (py/get-item books [2 -1])]
            x (hd/bind [a b c])
            out (resonator/resonator-fhrr x books)
            factors (:factors out)]
        (when (:success? out)
          (torch/allclose
           x
           (hd/bind factors))))))
  (def output *1)
  (map #(remove true? %) output)
  '(
    ;; book size
    ;; 10 - 50
    ()
    ()
    ()
    ()
    ()
    ;; 100:
    (nil)
    ;; 150:
    (nil nil nil nil nil))


























  (mapcat identity
          (for [trial (range 1)]
            (remove (comp true? first)
                    (for [book-size [500]]
                      (let [books (torch/stack [(hd/seed book-size)
                                                (hd/seed book-size)
                                                (hd/seed book-size)])
                            [a b c] [(py/get-item books [0 -1])
                                     (py/get-item books [1 -1])
                                     (py/get-item books [2 -1])]
                            x (hd/bind [a b c])
                            out (resonator/resonator-fhrr x
                                                          books
                                                          {:max-iterations 50})]
                        [(:success? out) :book-size book-size out])))))





















  (def books
    (torch/stack
     [(hd/seed 10)
      (hd/seed 10)
      (hd/seed 10)]))

  (hd/similarity
   books
   (hd/normalize (hd/superposition books)))
  (hd/similarity
   books
   (hd/superposition books)))
