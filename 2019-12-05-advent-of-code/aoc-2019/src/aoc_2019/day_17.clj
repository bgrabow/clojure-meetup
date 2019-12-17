(ns aoc-2019.day-17
  (:require [aoc-2019.day-09 :as intcode]
            [aoc-2019.day-13 :as day13]
            [clojure.string :as str]))

(def memory (intcode/parse-memory (slurp "resources/input-17.txt")))

(def grid-output
  (:outputs (intcode/run-until-fixed (intcode/initial-state memory))))

(def grid-str (apply str (map #(char %) grid-output)))

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

(defn is-scaffold?
  [m k]
  (= (m k) \#))

(defn neighbors
  [[x y]]
  (for [d [[0 1] [0 -1] [1 0] [-1 0]]]
    (mapv + d [x y])))

(defn is-intersection?
  [m k]
  (and (= (m k) \#)
       (->> (neighbors k)
            (map m)
            (every? #{\#}))))

(comment
  (day13/print-2D!
    identity (parse-grid grid-str))
  (->> (let [grid (parse-grid grid-str)]
         (filter (partial is-intersection? grid) (keys (parse-grid grid-str))))
       (map #(apply * %))
       (reduce +))
  (let [grid (parse-grid grid-str)]))
