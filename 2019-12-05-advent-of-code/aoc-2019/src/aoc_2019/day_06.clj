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
  (take-while some? (rest (iterate orbits body))))

(defn orbit-depth
  [orbits body]
  (count (parent-orbits orbits body)))

(defn common-root-depth
  [orbits a b]
  (apply max (map (partial orbit-depth orbits)
                  (set/intersection (set (parent-orbits orbits a))
                                    (set (parent-orbits orbits b))))))

(def test-orbits
  (parse-orbits "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L\nK)YOU\nI)SAN"))

(defn symmetric-difference
  [s1 s2]
  (set/difference (set/union s1 s2)
                  (set/intersection s1 s2)))

(defn solve-p1
  [orbits]
  (reduce +
          (map (partial orbit-depth orbits)
               (keys orbits))))

(defn solve-p2
  [orbits]
  (count
    (symmetric-difference (set (parent-orbits orbits "YOU"))
                          (set (parent-orbits orbits "SAN")))))

(comment
  (time (solve-p1 orbits))
  (time (solve-p2 orbits)))
