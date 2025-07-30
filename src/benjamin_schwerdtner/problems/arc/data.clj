(ns benjamin-schwerdtner.problems.arc.data
  (:require
   [clojure.string :as str]
   [clojure.java.io :as io]
   [clojure.data.json :as json]
   [benjamin-schwerdtner.clj-push3-play.configuration.config :refer [program-config]]))

(defn arc-agi-data-path []
  (:arc/data-path (program-config)))

(defn training-tasks []
  (filter #(str/ends-with? % ".json")
          (map str (file-seq (io/file (arc-agi-data-path))))))

(defn evaluation-tasks []
  (filter #(str/ends-with? % ".json")
          (map str (file-seq (io/file (arc-agi-data-path))))))

(defn task-3x3? [task]
  (let [examples (concat (:train task) (:test task))]
    (every? (fn [example]
              (and (= 3 (count (:input example)))
                   (= 3 (count (first (:input example))))
                   (= 3 (count (:output example)))
                   (= 3 (count (first (:output example))))))
            examples)))

(defn read-task [file]
  (assoc
   (json/read
    (io/reader (io/file file))
    {:key-fn keyword})
   :file file))

;; (read-task (first (training-tasks)))
;; (first (filter task-3x3? (training-tasks)))

;; (read-task "/home/benj/repos/ARC-AGI-2/data/training/91413438.json")
