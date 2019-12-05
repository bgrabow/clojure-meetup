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

(defn fuel2
  [mass]
  (let [x (fuel mass)]
    (if (> x 0)
      x 0)))

(comment
  (fuel2 8)
  (fuel2 9)

  (letfn [(fuel-iterative
            [module-weight]
            (loop [x module-weight
                   fuel-weights []]
              (let [f (fuel2 x)]
                (if (pos? f)
                  (recur f (conj fuel-weights f))
                  fuel-weights))))]

    (mapcat fuel-iterative masses)))

(defn fuel-iter [mass]
  (sum (take-while pos? (drop 1 (iterate fuel mass)))))

(def solution-1 (sum (map fuel masses)))

(def solution-2 (sum (map fuel-iter masses)))
