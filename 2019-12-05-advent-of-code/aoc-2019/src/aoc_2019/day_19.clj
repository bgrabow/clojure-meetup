(ns aoc-2019.day-19
  (:require [aoc-2019.day-09 :as intcode]
            [aoc-2019.day-11 :as day-11]))

(def memory (intcode/parse-memory (slurp "resources/input-19.txt")))

(defn solve-p1
  [memory]
  (->> (for [x (range 50)
             y (range 50)]
         (-> (intcode/run-until-fixed (assoc (intcode/initial-state memory)
                                        :inputs [x y]))
             :outputs
             first))
       frequencies
       (#(get % 1))))

(defn scan
  [[x y]]
  (-> (intcode/run-until-fixed (assoc (intcode/initial-state memory)
                                 :inputs [x y]))
      :outputs
      first))

(defn step-right
  [top-left]
  (letfn [(bottom-left [top-left]
            (mapv + top-left [0 99]))]
    (first
      (drop-while
        (comp zero? scan bottom-left)
        (iterate (fn [[x y]]
                   [(inc x) y])
                 top-left)))))

(defn step-down
  [top-left]
  (letfn [(top-right [top-left]
            (mapv + top-left [99 0]))]
    (first
      (drop-while
        (comp zero? scan top-right)
        (iterate (fn [[x y]]
                   [x (inc y)])
                 top-left)))))

(comment
  (letfn [(bottom-left [top-left]
                       (mapv + top-left [0 99]))]
    (->> (iterate (fn [[x y]]
                    [(inc x) y])
                  [0 0])
         (map (comp scan bottom-left))
         (take-while zero?)
         count)))

(defn spy [x] (println x) x)

(comment
  (take 100 (map spy (iterate (comp step-down step-right) [0 0])))

  (+ (* 1073 10000) 411))

(defn solve-p2
  []
  (let [[x y] (last
                (day-11/iterate-until-fixed
                  (comp step-down step-right)
                  [0 0]))]
    (+ y (* 10000 x))))


(comment
  (let [width 6
        [x y] [19 50]
        target-depth (* 100 (/ 50. 6))
        target-offset (* 19 (/ target-depth 50.))]
    target-offset))
