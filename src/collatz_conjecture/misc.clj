(ns collatz-conjecture.misc
  (:require [quil.core :as q]
            [clatrix.core :as m]
            [clojure.zip :as zip])
  (:import (java.text DecimalFormat)))

(defn bf-tree-seq [branch? children & roots]
  (when (seq? roots)
    (lazy-cat roots
              (apply (partial bf-tree-seq branch? children)
                     (mapcat children roots)))))

(defn сollatz-conj-branch? [x]
  (< 0 x))

(defn сollatz-conj-children [x]
  (let [n (/ (dec x) 3)]
    (cond->> (list (* 2 x))
             (and (integer? n) (odd? n) (not= 1 n))
             (cons n))))

(defn children-locs [loc]
  (loop [loc (zip/down loc)
         children [loc]]
    (if-let [next-child (zip/right loc)]
      (recur next-child (conj children next-child))
      children)))

(defn height [loc]
  (if (zip/branch? loc)
    (inc (apply max (map height (children-locs loc))))
    0))

(defn float->round2str [x]
  (.format (new DecimalFormat "#.##") x))

(defn map-values
  ([f]
   (partial map-values f))
  ([f m]
   (map-values f m (keys m)))
  ([f m keys]
   (reduce #(update-in %1 [%2] f) m keys)))

(defn linspace
  [start end size]
  (let [delta (/ (- end start) (dec size))]
    (map #(+ start (* % delta))
         (range size))))

(defn proportional-map [from-min from-max into-min into-max x]
  (+ into-min
     (* (- x from-min)
        (/ (- into-max into-min)
           (- from-max from-min)))))

(defn astro-intensity [s r h g l]
  (let [psi (* 2 q/PI (+ (/ s 3) (* r l)))
        lg (q/pow l g)]
    (->> (m/* (* h lg (/ (- 1 lg) 2))
              (m/matrix [[-0.14861 1.78277]
                         [-0.29227 -0.90649]
                         [1.97294 0.0]])
              (m/matrix [[(q/cos psi)] [(q/sin psi)]]))
         (m/+ lg)
         (m/* 255)
         (m/map q/round)
         (m/to-vecs)
         (flatten))))

;; s 2
;; r 1
;; h 0.5
;; g 1


(defn setup-astro-intensity []
  (q/frame-rate 60)
  {:show-info {:frame-rate true}
   :palettes  (cycle
                (for [g (linspace 0.8 1.2 3)
                      r (concat (linspace 1 5 30) (linspace 5 1 30)) ;(concat (linspace 0 5 25) (linspace 5 0 24))
                      h (linspace 0 2 30)                   ;(concat (linspace 0 2 10) (linspace 2 0 9))]
                      s (linspace 0 3 36)]                  ;(linspace 1 3 9)
                  (for [l (linspace 0 1 (q/width))]
                    (astro-intensity s r h g l))))})

(defn update-astro-intensity [state]
  (update state :palettes rest))

(defn draw-astro-intensity [state]
  (q/background 255 255 255)
  (q/no-stroke)
  (doseq [[x col] (map-indexed vector (first (:palettes state)))]
    (do
      (apply q/fill col)
      (doseq [y (range (q/height))]
        (q/rect x y 1 1)))))
