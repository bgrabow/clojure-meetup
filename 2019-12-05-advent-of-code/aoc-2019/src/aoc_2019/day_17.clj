(ns aoc-2019.day-17
  (:require [aoc-2019.day-09 :as intcode]
            [aoc-2019.day-13 :as day13]
            [clojure.string :as str]
            [clojure.set :as set]))

(def memory (delay (intcode/parse-memory (slurp "resources/input-17.txt"))))

(def grid-output
  (delay (:outputs (intcode/run-until-fixed (intcode/initial-state @memory)))))

(def grid-str (apply str (map #(char %) @grid-output)))

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
  (and (is-scaffold? m k)
       (every? (partial is-scaffold? m) (neighbors k))))

(def movement-routines
  {"A" "R4L82L82"
   "B" "L8R84R82R4"
   "C" "L8L8R82R4"})

(def final-path
  (reduce (fn [s [k v]] (str/replace s (re-pattern k) v))
          "R4L82L82L8R84R82R4R4L82L82L8R84R82R4R4L82L82L8L8R82R4L8R84R82R4L8L8R82R4R4L82L82L8L8R82R4"
          (set/map-invert movement-routines)))

(defn instruction
  [s]
  (map #(long %) (str (str/join \, s) \newline)))

(apply str (map char (instruction "R10L12")))

(comment
  (instruction "abc")
  (long \n))

(def full-input-instructions
  (mapcat
    instruction
    (concat
      ;; Final path
      [final-path]
      ;; Movement routines
      (vals (sort movement-routines))
      ;; yes video feed
      ["n"])))

(apply str (map char full-input-instructions))

(defn solve-p2
  []
  (-> @memory
      (assoc 0 2) ;; Movement mode
      (intcode/initial-state)
      (assoc :inputs full-input-instructions)
      intcode/run-until-fixed
      :outputs
      last))

(comment
  (str grid-str)
  (->> (let [grid (parse-grid grid-str)]
         (filter (partial is-intersection? grid) (keys (parse-grid grid-str))))
       (map #(apply * %))
       (reduce +))
  (let [grid (parse-grid grid-str)])
  ["R4L10L10L8R12R10R4R4L10L10L8R12R10R4R4L10L10L8L8R10R4L8R12R10R4L8L8R10R4R4L10L10L8L8R10R4"]
  {"R4L10L10"   "A"
   "L8R12R10R4" "B"
   "L8L8R10R4"  "C"}
  (-> "R4L10L10L8R12R10R4R4L10L10L8R12R10R4R4L10L10L8L8R10R4L8R12R10R4L8L8R10R4R4L10L10L8L8R10R4"
      (fn [s] (reduce (fn [s [k v]] (str/replace s (re-pattern k) v))
                      "R4L10L10L8R12R10R4R4L10L10L8R12R10R4R4L10L10L8L8R10R4L8R12R10R4L8L8R10R4R4L10L10L8L8R10R4"
                      {"R4L10L10"   "A"
                       "L8R12R10R4" "B"
                       "L8L8R10R4"  "C"}))))


