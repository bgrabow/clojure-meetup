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

(defn parent-bodies
  [orbits body]
  (take-while some? (rest (iterate orbits body))))

(defn symmetric-difference
  [s1 s2]
  (set/difference (set/union s1 s2)
                  (set/intersection s1 s2)))

(defn solve-p1
  [orbits]
  (count (mapcat (partial parent-bodies orbits) (keys orbits))))

(defn solve-p2
  [orbits]
  (count
    (symmetric-difference (set (parent-bodies orbits "YOU"))
                          (set (parent-bodies orbits "SAN")))))

(comment
  (time (solve-p1 orbits))
  (time (solve-p2 orbits)))
