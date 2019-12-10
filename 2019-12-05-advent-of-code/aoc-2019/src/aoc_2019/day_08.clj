(ns aoc-2019.day-08
  (:require [clojure.string :as str]))

(def layers
  (->> (str/split-lines (slurp "resources/input-08.txt"))
       first
       seq
       (partition 25)
       (map vec)
       (partition 6)
       (map vec)))

(defn solve-p1
  [layers]
  (let [{ones \1
         twos \2}
        (->> layers
             (map (comp frequencies flatten))
             (apply min-key #(get % \0)))]
    (* ones twos)))

(comment
  (solve-p1 layers))

(def pixel
  {\0 " "
   \1 "X"
   \2 nil})

(defn flatten-pixel
  [layers]
  (some pixel layers))

(defn composite-image
  [layers]
  (for [y (range 6)]
    (for [x (range 25)]
      (flatten-pixel (map #(get (get % y) x) layers)))))
