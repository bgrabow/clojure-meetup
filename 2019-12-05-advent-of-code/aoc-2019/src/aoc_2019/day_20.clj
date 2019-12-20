(ns aoc-2019.day-20
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn parse-maze
  [s]
  (->> (str/split-lines s)
       (mapcat
         (fn [y line]
           (map
             (fn [x cell]
               [[x y] cell])
             (range)
             line))
         (range))
       (into {})))

(def maze (parse-maze (slurp "resources/input-20.txt")))

(defn neighbors
  [p]
  (for [d [[0 1]
           [1 0]
           [0 -1]
           [-1 0]]]
    (mapv + p d)))

(defn portal
  [maze p]
  (when-let [id-1 (re-find #"[A-Z]" (str (maze p)))]
    (when-let [id-2 (some #(re-find #"[A-Z]" (str %)) (map maze (neighbors p)))]
      (when-let [entrance (first
                            (filter
                              (comp #{\.} maze)
                              (neighbors p)))]
        [entrance (sort [id-1 id-2])]))))

(defn format-portal
  [[[p1] [p2]]]
  (when (and p1 p2)
    [p1 p2]))

(def portals (->> (keep (partial portal maze) (keys maze))
                  (group-by second)
                  (map second)
                  (map format-portal)
                  (into {})
                  (#(merge % (set/map-invert %)))))

(def start (-> (keep (partial portal maze) (keys maze))
               (set/map-invert)
               (get (list "A" "A"))))

(def end (-> (keep (partial portal maze) (keys maze))
             (set/map-invert)
             (get (list "Z" "Z"))))


