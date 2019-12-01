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

(defn divisible-by [x y]
  (and (not= x y)
       (= 0 (rem y x))))

(comment
 (divisible-by 3 4))

(defn has-divisor [coll x]
  (some #(divisible-by % x) coll))

(comment
 (has-divisor [2 3] 4))

(defn has-multiple [coll x]
  (some #(divisible-by x %) coll))

(comment
 (has-multiple [4 3] 6)
 (has-multiple [4 3] 2))

(defn filter-divisible [line]
  (filter #(or (has-divisor line %)
               (has-multiple line %))
          line))

(defn line-divisible-checksum [line]
  (->> line
       filter-divisible
       (sort-by -)
       (apply quot)))

(comment
 (has-divisor [ 2 3 4] 3))


(defn divisible-checksum
  [lines]
  (reduce + (map line-divisible-checksum lines)))

(comment

  (checksum input)
  (divisible-checksum input)
  (reduce + (map line-divisible-checksum input))

  (let [test-input [[5 9 2 8]
                    [9 4 7 3]
                    [3 8 6 5]]]
    (->> (map filter-divisible test-input)
         (map #(sort-by - %))
         (map #(apply quot %))
         (reduce +))))
