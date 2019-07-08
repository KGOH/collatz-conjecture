(ns collatz-conjecture.core
  (:require [clojure.string :as str]
            [clojure.zip :as zip]
            [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [collatz-conjecture.mymiddleware :refer [show-info record-gif]]
            [collatz-conjecture.misc :refer [bf-tree-seq
                                             linspace
                                             astro-intensity
                                             map-values
                                             float->round2str
                                             proportional-map
                                             сollatz-conj-branch?
                                             сollatz-conj-children
                                             height]]))

(def info-provider
  {:frame-rate (fn [state] (float->round2str (q/current-frame-rate)))
   :delta      (fn [state] (str/join
                             \space
                             (map float->round2str
                                  (-> state :navigation-2d :position))))
   :zoom       (fn [state] (float->round2str
                             (-> state :navigation-2d :zoom)))})

(def my-color-scheme (partial astro-intensity 2 0.8 1.8 1.4))

(def my-color-scheme-memo (memoize my-color-scheme))

(defn reset-values [state]
  (merge state (:defaults state)))

(defn reset-collatz-zipper [{:keys [root max-val] :as state}]
  (assoc state
    :zipper
    (zip/zipper #(< 0 % max-val) сollatz-conj-children nil root)))

(defn reset-max-depth [state]
  (assoc state :max-depth (inc (height (:zipper state)))))

(defn setup []
  (q/frame-rate 30)
  (-> {:flag         true
       :max-depth    180
       :show-info    {:frame-rate true}
       :defaults     {:root    1
                      :max-val 2
                      :angles  {:init      20
                                :not-forks -2.8
                                :even      -20
                                :odd       8.5}}
       :decl         (cycle (concat
                              (repeat 25 -0.02) (linspace -0.02 0.02 50)
                              (repeat 25 0.02) (linspace 0.02 -0.02 50)))
       :key-bindings {32 :reset-all
                      81 :dec-not-forks-angle
                      87 :reset-not-forks-angle
                      69 :inc-not-forks-angle
                      65 :dec-even-angle
                      83 :reset-even-angle
                      68 :inc-even-angle
                      90 :dec-odd-angle
                      88 :reset-odd-angle
                      67 :inc-odd-angle
                      82 :inc-init-angle
                      84 :reset-init-angle
                      89 :dec-init-angle
                      70 :inc-root
                      71 :reset-root
                      72 :dec-root
                      86 :inc-max-val
                      66 :reset-max-val
                      78 :dec-max-val}}
      (reset-values)
      (reset-collatz-zipper)
      (reset-max-depth)))

(defn key-pressed [state event]
  (let [{:keys [key-code key]} event]
    ;(println key " " key-code)
    (case (get-in state [:key-bindings key-code] :no-binding)
      :reset-all
      (reset-values state)

      :dec-not-forks-angle
      (update-in state [:angles :not-forks] (partial + -0.1))
      :reset-not-forks-angle
      (assoc-in state [:angles :not-forks] (-> state :defaults :angles :not-forks))
      :inc-not-forks-angle
      (update-in state [:angles :not-forks] (partial + 0.1))

      :dec-even-angle
      (update-in state [:angles :even] (partial + -0.1))
      :reset-even-angle
      (assoc-in state [:angles :even] (-> state :defaults :angles :even))
      :inc-even-anglef
      (update-in state [:angles :even] (partial + 0.1))

      :dec-odd-angle
      (update-in state [:angles :odd] (partial + -0.1))
      :reset-odd-angle
      (assoc-in state [:angles :odd] (-> state :defaults :angles :odd))
      :inc-odd-angle
      (update-in state [:angles :odd] (partial + 0.1))

      :dec-init-angle
      (update-in state [:angles :init] (partial + -0.1))
      :reset-init-angle
      (assoc-in state [:angles :init] (-> state :defaults :angles :init))
      :inc-init-angle
      (update-in state [:angles :init] (partial + 0.1))

      :dec-root
      (-> (update state :root #(max 1 (+ % -1)))
          (reset-collatz-zipper)
          (reset-max-depth))
      :reset-root
      (-> (assoc state :root (-> state :defaults :root))
          (reset-collatz-zipper)
          (reset-max-depth))
      :inc-root
      (-> (update state :root inc)
          (reset-collatz-zipper)
          (reset-max-depth))

      :dec-max-val
      (-> (update state :max-val #(max 1 (+ % -100)))
          (reset-collatz-zipper)
          (reset-max-depth))
      :reset-max-val
      (-> (assoc state :max-val (-> state :defaults :max-val))
          (reset-collatz-zipper)
          (reset-max-depth))
      :inc-max-val
      (-> (update state :max-val #(+ % 100))
          (reset-collatz-zipper)
          (reset-max-depth))

      :no-binding
      state)))

(defn update-state [state]
  (-> (cond-> state
        (or (and (:flag state) (>= (:max-val state) 30000))
            (and (not (:flag state)) (<= (:max-val state) 100)))
        (update :flag not))
      (#(update % :max-val (partial + (if (:flag %) 100 -100))))
      (reset-collatz-zipper)
      (reset-max-depth)
      (update :decl rest)))


(defn draw-line [{:keys [angle x y]} declination len color width]
  (let [new-angle (+ angle declination)
        len (- len 0.3)
        new-x (+ x (* len (q/cos (q/radians new-angle))))
        new-y (+ y (* len (q/sin (q/radians new-angle))))]
    (q/stroke-weight width)
    (apply q/stroke color)
    (q/line x y new-x new-y))
  (let [new-angle (+ angle declination)
        new-x (+ x (* len (q/cos (q/radians new-angle))))
        new-y (+ y (* len (q/sin (q/radians new-angle))))]
    {:angle new-angle, :x new-x, :y new-y}))

(defn draw-branch
  ([max-depth declinations node pos]
   (draw-branch max-depth declinations node pos 0))
  ([max-depth {:keys [odd even not-forks] :as declinations} node pos step]
   (if (zip/branch? node)
     ;(q/text (str (zip/node node)) (:x pos) (:y pos))
     (let [next-node (zip/next node)
           next-step (inc step)
           next-len 8
           next-color (my-color-scheme-memo (proportional-map 0 max-depth 0 1 step))
           next-height (height next-node)
           next-opacity (proportional-map 0 max-depth 100 255 next-height)
           next-color (concat next-color [next-opacity])
           next-width (proportional-map max-depth 0 3 1.3 next-height)] ;(proportional-map max-val 7930 10 1 step)
       (if (= 2 (count (zip/children node)))
         (do
           (draw-branch max-depth declinations next-node (draw-line pos odd next-len next-color next-width) next-step)
           (let [next-node (zip/right next-node)
                 next-color (my-color-scheme-memo (proportional-map 0 max-depth 0 1 step))
                 next-height (height next-node)
                 next-opacity (proportional-map 0 (- max-depth next-step) 100 255 next-height)
                 next-color (concat next-color [next-opacity])
                 next-width (proportional-map max-depth 0 3 1 next-height)]
             (recur max-depth declinations next-node (draw-line pos even next-len next-color next-width) next-step)))
         (recur max-depth declinations next-node (draw-line pos not-forks next-len next-color next-width) next-step))))))

(defn draw-state [{:keys [zipper decl angles max-depth max-val]}]
  (q/background 255 255 255)
  (q/fill 0 0 0)
  (q/text-size 5)
  (draw-branch
    max-depth
    (-> angles
        (update :odd (partial + (nth decl 25)))
        (update :even (partial + (first decl)))
        (update :not-forks (partial + (nth decl 12))))
    zipper
    {:angle (angles :init), :x 20, :y 680}))

(q/defsketch collatz-conjecture
             :size [1334 750]
             :setup setup
             :update update-state
             :key-pressed key-pressed
             :draw draw-state
             :info-provider info-provider
             :middleware [m/fun-mode
                          show-info
                          record-gif])
                          ;m/navigation-2d])




