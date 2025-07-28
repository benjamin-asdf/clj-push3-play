;; ===============================
;; https://github.com/lspector/Clojush
;;
;;
;; This file contains source code from the Clojush project,
;; which is Licencend under Eclipse Public License - v 1.0
;;
;; THE ACCOMPANYING PROGRAM IS PROVIDED UNDER THE TERMS OF THIS ECLIPSE PUBLIC
;; LICENSE ("AGREEMENT"). ANY USE, REPRODUCTION OR DISTRIBUTION OF THE PROGRAM
;; CONSTITUTES RECIPIENT'S ACCEPTANCE OF THIS AGREEMENT.
;;
;;
;; You can find a full copy of the licence under NOTICE in the root of this repository
;;
;;
;; original File:
;;
;; https://github.com/lspector/Clojush/blob/master/src/clojush/util.clj
;;
;;
;; Benjamin:
;;
;; - changed math to Math
;; - left out some functions
;; - tiny code updates
;;
;; ===============================

(ns benjamin-schwerdtner.clj-push3-play.util
  (:require [clojure.zip :as zip]
            [clojure.walk :as walk]
            [clojure.string :as string]))

#_(defn keep-number-reasonable
  "Returns a version of n that obeys limit parameters."
  [n]
  (cond
    (integer? n)
    (cond
      (> n max-number-magnitude) max-number-magnitude
      (< n (- max-number-magnitude)) (- max-number-magnitude)
      :else n)
    :else
    (cond
      (Double/isNaN n) 0.0
      (= n Double/POSITIVE_INFINITY) (* 1.0 max-number-magnitude)
      (= n Double/NEGATIVE_INFINITY) (* 1.0 (- max-number-magnitude))
      (> n max-number-magnitude) (* 1.0 max-number-magnitude)
      (< n (- max-number-magnitude)) (* 1.0 (- max-number-magnitude))
      (and (< n min-number-magnitude) (> n (- min-number-magnitude))) 0.0
      :else n)))

(defn seq-zip
  "Returns a zipper for nested sequences, given a root sequence"
  {:added "1.0"}
  [root]
  (zip/zipper seq?
          seq
          (fn [node children] (with-meta children (meta node)))
          root))

(defn list-concat
  "Returns a (non-lazy) list of the items that result from calling concat
  on args."
  [& args]
  (apply list (apply concat args)))

(defn not-lazy
  "Returns lst if it is not a seq, or a non-lazy list of lst if it is."
  [lst]
  (if (seq? lst)
    (apply list lst)
    lst))

(defn ensure-list
  "Returns a non-lazy list of the contents of thing if thing is a seq.
  Returns a list containing thing otherwise."
  [thing]
  (if (seq? thing)
    (not-lazy thing)
    (list thing)))

(defn round-to-n-decimal-places
  "If a number, rounds float f to n decimal places."
  [f n]
  (if (not (number? f))
    f
    (let [factor (Math/pow 10 n)]
      (double (/ (Math/round (* f factor)) factor)))))

(defn count-parens
  "Returns the number of paren pairs in tree"
  [tree]
  (loop [remaining tree
         total 0]
    (cond (not (seq? remaining))
          total
          ;;
          (empty? remaining)
          (inc total)
          ;;
          (not (seq? (first remaining)))
          (recur (rest remaining)
                 total)
          ;;
          :else
          (recur (list-concat (first remaining)
                              (rest remaining))
                 (inc total)))))

(defn count-points
  "Returns the number of points in tree, where each atom and each pair of parentheses
   counts as a point."
  [tree]
  (loop [remaining tree
         total 0]
    (cond (not (seq? remaining))
          (inc total)
          ;;
          (empty? remaining)
          (inc total)
          ;;
          (not (seq? (first remaining)))
          (recur (rest remaining)
                 (inc total))
          ;;
          :else
          (recur (list-concat (first remaining)
                              (rest remaining))
                 (inc total)))))

(defn height-of-nested-list
  "Returns the height of the nested list called tree.
  Borrowed idea from here: https://stackoverflow.com/a/36865180/2023312
  Works by looking at the path from each node in the tree to the root, and
  finding the longest one.
  Note: does not treat an empty list as having any height."
  [tree]
  (loop [zipper (seq-zip tree)
         height 0]
    (if (zip/end? zipper)
      height
      (recur (zip/next zipper)
             (-> zipper
                 zip/path
                 count
                 (max height))))))


(defn code-at-point
  "Returns a subtree of tree indexed by point-index in a depth first traversal."
  [tree point-index]
  (let [index (mod (abs point-index) (count-points tree))
        zipper (seq-zip tree)]
    (loop [z zipper i index]
      (if (zero? i)
        (zip/node z)
        (recur (zip/next z) (dec i))))))

(defn insert-code-at-point
  "Returns a copy of tree with the subtree formerly indexed by
   point-index (in a depth-first traversal) replaced by new-subtree."
  [tree new-subtree point-index]
  (let [index (mod (abs point-index) (count-points tree))
        zipper (seq-zip tree)]
    (loop [z zipper i index]
      (if (zero? i)
        (zip/root (zip/replace z new-subtree))
        (recur (zip/next z) (dec i))))))

(defn remove-code-at-point
  "Returns a copy of tree with the subtree formerly indexed by
   point-index (in a depth-first traversal) removed. The old version would not
   allow removals that result in empty lists, but the current version allows
   this behavior."
  [tree point-index]
  (let [index (mod (abs point-index) (count-points tree))
        zipper (seq-zip tree)]
    (if (zero? index)
      tree ;; can't remove entire tree
      (loop [z zipper i index]
        (if (zero? i)
          (zip/root (zip/remove z))
          (if (and (= i 1) ;; zipper can't remove only item from list; instead, replace with empty list
                   (seq? (zip/node z))
                   (= 1 (count (zip/node z))))
            (zip/root (zip/replace z '())) ;; used to just return (zip/root z)
            (recur (zip/next z) (dec i))))))))




; Note: Well, I (Tom) think I figured out why truncate was there. When I tried running
; the change problem, it threw an exception trying to cast into an int a number
; that was too big. Maybe there's a different principled way to use casting, but
; I'm just going to add truncate back for now!
#_(defn truncate
  "Returns a truncated integer version of n."
  [n]
  (if (< n 0)
    (Math/round (Math/ceil n))
    (Math/round (Math/floor n))))


;; ---------------------------------------------

(defn walklist
  "Like walk, but only for lists."
  [inner outer form]
  (cond
    (list? form) (outer (apply list (map inner form)))
    (seq? form) (outer (doall (map inner form)))
    :else (outer form)))

(defn postwalklist
  "Like postwalk, but only for lists"
  [f form]
  (walklist (partial postwalklist f) f form))

(defn prewalkseq
  "Like prewalk but only for seqs and uses zippers."
  [f s]
  (loop [z (seq-zip s)] ;; note using modified version of seq-zip for now
    (if (zip/end? z)
      (zip/root z)
      (recur (zip/next (zip/replace z (f (zip/node z))))))))

(defn postwalklist-replace
  "Like postwalk-replace, but only for lists."
  [smap form]
  (postwalklist (fn [x] (if (contains? smap x) (smap x) x)) form))

(defn subst
  "Returns the given list but with all instances of that (at any depth)
   replaced with this. Read as 'subst this for that in list'. "
  [this that lst]
  (postwalklist-replace {that this} lst))

(defn postwalk-replace-in-list
  "Like [[subst]] but with different args."
  [smap lst]
  (postwalklist-replace smap lst))

(defn contains-subtree?
  "Returns true if tree contains subtree at any level."
  [tree subtree]
  (some #{subtree} (tree-seq seq? seq tree))
  ;; -----------------------------------------
  #_(cond (not (seq? tree)) nil
          (empty? tree) nil
          (= tree subtree) true
          :else (some #(contains-subtree tree %) tree)))

(comment
  (some
   #(= '(10) %)
   (tree-seq seq? seq 10))

  (some
   #(= '(10) %)
   (tree-seq seq? seq '(10))))



(defn containing-subtree
  "If tree contains subtree at any level then this returns the smallest
   subtree of tree that contains but is not equal to the first instance of
   subtree. For example, (contining-subtree '(b (c (a)) (d (a))) '(a)) => (c (a)).
   Returns nil if tree does not contain subtree."
  [tree subtree]
  (cond
    (not (seq? tree)) nil
    (empty? tree) nil
    (some #{subtree} tree) tree
    :else (some (fn [smaller-tree]
                  (containing-subtree smaller-tree subtree))
                tree)))

(comment
  (ensure-list [1 2 3]))
