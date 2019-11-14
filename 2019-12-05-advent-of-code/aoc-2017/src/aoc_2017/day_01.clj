(ns aoc-2017.day-01
  (:require [clojure.string :as str]))

(def input
  (->> (slurp "resources/input-01.txt")
    (str/split-lines)
    first
    (map str)
    (map read-string)
    (into [])))

(defn captcha-1
  [xs]
  (->> (map vector xs (drop 1 (cycle xs)))
    (filter #(apply = %))
    (map first)
    (reduce +)))

(defn captcha-half
  [xs]
  (->> (map vector xs (drop (/ (count xs) 2) (cycle xs)))
    (filter #(apply = %))
    (map first)
    (reduce +)))


(comment
  (captcha-1 input)
  (captcha-half input))
