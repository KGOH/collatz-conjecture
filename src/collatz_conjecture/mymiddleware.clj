(ns collatz-conjecture.mymiddleware
  (:require [clojure.string :as str]
    [quil.core :as q :include-macros true]
    [quil.middleware :as m]
    [gil.core :as g]
    [collatz-conjecture.misc :refer [float->round2str]]))

(defn record-gif [options]
  (let [draw (:draw options (fn []))
        updated-draw (fn [state]
                       (draw state)
                       (g/save-animation "conjecture.gif" 600 0))]
    (assoc options :draw updated-draw)))

(defn show-info [options]
  (let [draw (:draw options (fn []))
        info-provider (:info-provider options)
        updated-draw (fn [state]
                       (draw state)
                       (let [info (map (fn [[k f]] [(name k) (f state)])
                                       (select-keys
                                         info-provider
                                         (keys (filter val (:show-info state)))))
                             info (str/join \newline (map (partial str/join ": ") info))]
                         (when-not (empty? info)
                           (q/fill 0 0 0)
                           (q/text-size 12)
                           (q/text info 0 10))))]
    (assoc options :draw updated-draw)))