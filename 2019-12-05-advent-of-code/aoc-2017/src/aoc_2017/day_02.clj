(ns aoc-2017.day-02
  (:require [clojure.string :as str]))

(def input
  (->> (slurp "resources/input-02.txt")
    (str/split-lines)
    (mapv #(mapv read-string (str/split % #"\t")))))

(defn checksum-line
  [line]
  (let [low (apply min line)
        high (apply max line)]
    (- high low)))

(defn checksum
  [lines]
  (reduce + (map checksum-line lines)))

(comment

  (checksum input))
