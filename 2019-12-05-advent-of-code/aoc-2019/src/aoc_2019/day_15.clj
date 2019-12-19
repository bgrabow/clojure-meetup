(ns aoc-2019.day-15
  (:require [aoc-2019.day-09 :as intcode]
            [clojure.set :as set]))

(def memory (intcode/parse-memory (slurp "resources/input-15.txt")))

(def directions {1 [0 -1]
                 2 [0 1]
                 3 [-1 0]
                 4 [1 0]})

(def contents {0 :wall
               1 :empty
               2 :oxygen-system})

(defn explore
  [known-maze origin [dir-op step]]
  (when (= :empty (:contents (known-maze origin)))
    (let [droid (:droid (known-maze origin))
          {[contents-code] :outputs :as droid'} (intcode/run-until-fixed
                                                  (assoc droid :inputs [dir-op]))]
      [(mapv + step origin)
       {:droid    (dissoc droid' :outputs)
        :contents (contents contents-code)
        :distance (inc (:distance (known-maze origin)))}])))

(defn explore-neighbors
  [known-maze origin]
  (->> directions
       (remove (fn [[_ step]] (known-maze (mapv + origin step)))) ;; Skip already explored area
       (map (partial explore known-maze origin))
       (remove nil?)))

(defn step-djikstra
  [{:keys [known-maze frontier] :as s}]
  (let [neighbors (mapcat (partial explore-neighbors known-maze) frontier)]
    (-> s
        (update :known-maze into neighbors)
        (assoc :frontier (map first neighbors)))))

(defn trunc-droid
  [maze]
  (reduce-kv
    (fn [m k v]
      (assoc m k (dissoc v :droid)))
    {}
    maze))

(defn frontier-has-oxygen-system?
  [{:keys [known-maze frontier]}]
  (->> frontier
       (map (juxt identity known-maze))
       (filter (fn [[_ {c :contents}]]
                 (= c :oxygen-system)))
       first))

(defn solve-p1
  []
  (->> (iterate step-djikstra {:known-maze {[0 0] {:droid (intcode/initial-state memory)
                                                   :contents :empty
                                                   :distance 0}}
                               :frontier '([0 0])})
       (some frontier-has-oxygen-system?)))

(defn solve-p2
  []
  (let [[p {droid :droid}] (solve-p1)]
    (->> (iterate step-djikstra {:known-maze {p {:droid droid
                                                 :contents :empty
                                                 :distance 0}}
                                 :frontier (list p)})
         (drop-while (comp seq :frontier))
         first
         :known-maze
         vals
         (map :distance)
         (apply max))))
