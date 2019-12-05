(ns aoc-2019.day-03
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.pprint :refer [pprint]]))

(def examples
  ["R8,U5,L5,D3\nU7,R6,D4,L4\n"
   "R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83\n"
   "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51\nU98,R91,D20,R16,D67,R40,U7,R15,U6,R7\n"])

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

(defn parse-wires
  [raw-wires]
  (->> raw-wires
       (str/split-lines)
       (map parse-wire)))

(defn wire-steps
  [wire]
  (lazy-cat
    (when-let [{dir  :direction
                dist :distance} (first wire)]
      (repeat dist dir))
    (when (next wire)
      (wire-steps (rest wire)))))

(defn wire-footprint
  [wire]
  (zipmap
    (drop 1 (reductions #(mapv + %1 %2) [0 0] (wire-steps wire)))
    (drop 1 (range))))

(defn manhattan-distance
  [[x y] [x' y']]
  (+ (Math/abs ^long (- x x'))
     (Math/abs ^long (- y y'))))

(defn find-closest-intersection
  [[wire-a wire-b]]
  (let [intersections (set/intersection (set (keys wire-a)) (set (keys wire-b)))]
    (->> (map (partial manhattan-distance [0 0]) intersections)
         (sort)
         first)))

(defn find-shortest-path-intersection
  [[wire-a wire-b]]
  (let [intersections (set/intersection (set (keys wire-a)) (set (keys wire-b)))]
    (->> (zipmap intersections (map #(+ (wire-a %) (wire-b %)) intersections))
         (sort-by second)
         (first)
         second)))

(defn solve-p1
  []
  (time
    (find-closest-intersection (map wire-footprint (parse-wires (slurp "resources/input-03.txt"))))))

(defn solve-p2
  []
  (time
    (find-shortest-path-intersection (map wire-footprint (parse-wires (slurp "resources/input-03.txt"))))))
