(ns aoc-2017.day-01
  (:require [clojure.string :as str]))

(def input
  (->> (slurp "resources/input-01.txt")
    (str/split-lines)
    first))

(def shifted-1
  (concat (drop 1 input) (take 1 input)))
