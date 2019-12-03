(ns aoc-2019.day-03
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def directions
  {"R" [0 1]
   "L" [0 -1]
   "U" [1 0]
   "D" [-1 0]})

(defn parse-wire
  [raw-wire]
  (->> (str/split raw-wire #",")
       (map #(re-matches #"(\w)(\d+)" %))
       (map (fn [[_ direction-code distance]]
              {:direction (directions direction-code)
               :distance (Integer/parseInt distance)}))))

(def wire-parametric-segments
  (->> (slurp "resources/input-03.txt")
       (str/split-lines)
       (map parse-wire)))

(defn spaces-traversed
  [start direction distance]
  (let [[x y] start
        [dx dy] direction]
    (set
      (for [x' (take distance (next (iterate #(+ dx %) x)))
            y' (take distance (next (iterate #(+ dy %) y)))]
        [x' y']))))

(defn step
  [{current :current occupied :occupied :as state}
   {direction :direction
    distance :distance}]
  (-> state
      (update :occupied into (spaces-traversed current direction distance))
      (update :current #(map + % (map * direction (repeat distance))))))

(def wires (map #(reduce step
                         {:occupied #{[0 0]}
                          :current [0 0]}
                         %)
                wire-parametric-segments))

(def intersections
  (->> wires
       (map :occupied)
       (map #(disj % [0 0]))
       (apply set/intersection)))3229

(defn manhattan-distance
  [[x y] [x' y']]
  (+ (Math/abs ^long (- x x'))
     (Math/abs ^long (- y y'))))

(first (sort (map (partial manhattan-distance [0 0]) intersections)))
