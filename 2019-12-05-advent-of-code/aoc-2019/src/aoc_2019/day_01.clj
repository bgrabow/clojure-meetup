(ns aoc-2019.day-01
  (:require [clojure.string :as str]))

(def masses
  (map #(Integer/parseInt %)
       (str/split-lines (slurp "resources/input-01.txt"))))

(defn sum [coll] (reduce + 0 coll))

(defn fuel
  [mass]
  (-> mass
      (quot 3)
      (- 2)))

(defn fuel-iter [mass]
  (sum (take-while pos? (drop 1 (iterate fuel mass)))))

(def solution-1 (sum (map fuel masses)))

(def solution-2 (sum (map fuel-iter masses)))
