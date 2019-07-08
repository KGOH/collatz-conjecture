(ns collatz-conjecture.scratch
  (:require [clojure.zip :as zip]
            [collatz-conjecture.misc :refer [height children-locs]]))

(defn сollatz-conj-branch? [x]
  (< 0 x 20000))

(defn сollatz-conj-children [x]
  (let [n (/ (dec x) 3)]
    (cond->> (list (* 2 x))
             (and (integer? n) (odd? n) (not= 1 n))
             (cons n))))

(def c (zip/zipper сollatz-conj-branch? сollatz-conj-children nil 16))
(println (children-locs c))
(println (zip/children c))
(println (height c))
;(zip/branch? c)