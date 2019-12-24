(ns aoc-2019.day-24b
  (:require [clojure.string :as str]))

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

(def initial-state (->> (parse-grid (slurp "resources/input-24.txt"))
                        (keep (fn [[p c]]
                                (when (= c \#)
                                  (conj p 0))))
                        (set)))

(def in-bounds
  (-> (for [x (range 5)
            y (range 5)]
        [x y])
      set
      (disj [2 2])))

(defn recursive-neighbors
  [[x y z]]
  (remove nil?
          (concat (->> (for [d [[0 1 0] [1 0 0] [0 -1 0] [-1 0 0]]]
                         (mapv + [x y z] d))
                       (filter (comp in-bounds #(subvec % 0 2))))
                  [(when (= 0 x)
                     [1 2 (dec z)])
                   (when (= 4 x)
                     [3 2 (dec z)])
                   (when (= 0 y)
                     [2 1 (dec z)])
                   (when (= 4 y)
                     [2 3 (dec z)])]
                  (when (= [1 2] [x y])
                    (for [y (range 5)]
                      [0 y (inc z)]))
                  (when (= [2 1] [x y])
                    (for [x (range 5)]
                      [x 0 (inc z)]))
                  (when (= [2 3] [x y])
                    (for [x (range 5)]
                      [x 4 (inc z)]))
                  (when (= [3 2] [x y])
                    (for [y (range 5)]
                      [4 y (inc z)])))))

(defn step-recursive-conway
  [bugs]
  (->> (mapcat recursive-neighbors bugs)
       frequencies
       (keep (fn [[p n]]
               (when (or (and (bugs p) (= 1 n))
                         (and (not (bugs p)) (#{1 2} n)))
                 p)))
       set))

(defn solve-p2
  []
  (count (nth (iterate step-recursive-conway initial-state) 200)))

(comment
  (recursive-neighbors [0 0 0])
  (recursive-neighbors [1 1 0])
  (sort-by (comp vec reverse) (recursive-neighbors [1 2 0]))
  (sort-by (comp vec reverse) (recursive-neighbors [3 2 0])))