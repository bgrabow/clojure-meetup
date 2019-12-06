(ns aoc-2019.day-06
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn parse-orbit
  [s]
  (vec (reverse (str/split s #"\)"))))

(defn parse-orbits
  [s]
  (->> s
       (str/split-lines)
       (map parse-orbit)
       (into {})))

(def orbits
  (parse-orbits (slurp "resources/input-06.txt")))

(defn parent-orbits
  [orbits body]
  (take-while (comp not nil?)
              (iterate orbits body)))

(defn orbit-depth
  [orbits body]
  (dec (count (parent-orbits orbits body))))

(defn common-root-depth
  [orbits a b]
  (apply max (map (partial orbit-depth orbits)
                  (set/intersection (set (parent-orbits orbits a))
                                    (set (parent-orbits orbits b))))))

(def test-orbits
  (parse-orbits "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L\nK)YOU\nI)SAN"))

(defn solve-p1
  [orbits]
  (reduce +
          (map (partial orbit-depth orbits) (keys orbits))))

(defn solve-p2
  [orbits]
  (+ (- (dec (orbit-depth orbits "YOU")) (common-root-depth orbits "YOU" "SAN"))
     (- (dec (orbit-depth orbits "SAN")) (common-root-depth orbits "YOU" "SAN"))))

(comment
  (solve-p1 orbits)
  (solve-p2 orbits))
