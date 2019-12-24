(ns aoc-2019.day-24
  (:require [clojure.string :as str]
            [clojure.math.numeric-tower :refer [expt]]))

(defn parse-grid
  [s]
  (->> (mapcat
         (fn [y row]
           (map
             (fn [x cell]
               [[x y] cell])
             (range)
             row))
         (range)
         (str/split-lines s))
       (into {})))

(comment
  (parse-grid (slurp "resources/input-24.txt")))

(defn alive?
  [x n-neighbors]
  (or (and x (= 1 n-neighbors))
      (and (not x) (#{1 2} n-neighbors))))

(defn neighbors
  [p]
  (for [d [[0 1] [1 0] [0 -1] [-1 0]]]
    (mapv + d p)))

(defn step-conway
  [grid]
  (into {} (map (fn [[p c]]
                  (if (alive? (= \# c)
                              (->> (neighbors p)
                                   (map grid)
                                   (filter #{\#})
                                   count))
                    [p \#]
                    [p \.]))
                grid)))

(defn print-2D
  [grid-contents]
  (println)
  (let [x-min       (apply min (map first (keys grid-contents)))
        x-max       (apply max (map first (keys grid-contents)))
        y-min       (apply min (map second (keys grid-contents)))
        y-max       (apply max (map second (keys grid-contents)))
        render-cell identity]
    (doseq [y (range y-min (inc y-max))]
      (println
        (apply str
               (map
                 (fn [x] (render-cell (grid-contents [x y])))
                 (range x-min (inc x-max))))))))

(defn first-repeated-state
  [coll]
  (reduce
    (fn [ss s']
      (if (contains? ss s')
        (reduced s')
        (conj ss s')))
    #{}
    coll))

(defn spy
  [f x]
  (f x)
  x)

(def repeated (first-repeated-state (iterate step-conway (parse-grid (slurp "resources/input-24.txt")))))

(defn cell-rating
  [[x y]]
  (* (expt 2 x)
     (expt 32 y)))

(defn biodiversity-rating
  [grid]
  (->> grid
       (filter #(= \# (val %)))
       (map (comp cell-rating first))
       (reduce +)))

(defn solve-p1
  []
  (biodiversity-rating repeated))
