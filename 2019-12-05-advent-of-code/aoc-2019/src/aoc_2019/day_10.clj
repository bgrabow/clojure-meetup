(ns aoc-2019.day-10
  (:require [clojure.string :as str]))



(def space
  (->> (into {}
             (mapcat
               (fn [y line]
                 (map
                   (fn [x space]
                     [[x y] space])
                   (range)
                   line))
               (range)
               (str/split-lines (slurp "resources/input-10.txt"))))
       (keep (fn [[p space]]
               (when (= space \#) p)))
       (into #{})))

(defn unit-vector
  "Vector pointing from p1 to p2"
  [p1 p2]
  (let [[dx dy] (map - p2 p1)]
    (if (zero? dx)
      [0 (if (pos? dy) 1 -1)]
      [(if (pos? dx) 1 -1)
       (if (pos? dx) (/ dy dx) (- (/ dy dx)))])))

(comment
  [1 -2]
  [1 -2]
  [-1 2]
  [-1 2]
  (unit-vector [0 0] [2 -1]))

(defn n-asteroids-in-los
  [asteroid space]
  (count (distinct (map (partial unit-vector asteroid) space))))

(def best-vantage-point
  (second
    (apply (partial max-key first)
           (for [asteroid space]
             [(n-asteroids-in-los asteroid (disj space asteroid))
              asteroid]))))

(defn solve-p1
  []
  (first
    (apply (partial max-key first)
           (for [asteroid space]
             [(n-asteroids-in-los asteroid (disj space asteroid))
              asteroid]))))

(defn manhattan-distance
  [p1 p2]
  (reduce + (map #(Math/abs ^long (- %1 %2)) p1 p2)))

(defn asteroid-headings
  [base others]
  (map vector (map (partial unit-vector base) others)
              (for [target others]
                {:coordinates target
                 :distance (manhattan-distance base target)})))

(defn angle
  [[dx dy]]
  (mod (+ (* 2 Math/PI) (Math/atan2 dx dy)) (* 2 Math/PI)))

(angle [0 1])
(angle [0 -1])
(angle [-1 0])
(angle [1 0])

(def firing-sequence
  (->> (reduce (fn [m [k v]]
                 (update m k conj v))
               {}
               (asteroid-headings [11 13] (disj space [11 13])))
       (map (fn [[k v]]
              [k (sort-by :distance v)]))
       (sort-by (comp angle first))
       (vec)))

(defn shoot-one
  [firing-sequence]
  (let [current-heading (first firing-sequence)
        remaining (update current-heading 1 rest)]
    (if (seq (second remaining))
      (conj (subvec firing-sequence 1) (update current-heading 1 rest))
      (subvec firing-sequence 1))))

(take 2 (iterate shoot-one firing-sequence))

(shoot-one firing-sequence)
