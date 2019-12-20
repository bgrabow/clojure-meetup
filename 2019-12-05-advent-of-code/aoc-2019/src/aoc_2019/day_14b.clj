(ns aoc-2019.day-14b
  (:require [clojure.string :as str]
            [clojure.edn :as edn]))

(defn parse-reaction
  [s]
  (->> (re-seq #"(\d+) (\w+)" s)
       (map (comp vec rest))
       reverse
       (map #(update % 0 edn/read-string))
       (map reverse)
       ((fn [[product & reactants]]
          {(first product) {:yield (second product)
                            :deps (into {} (map vec reactants))}}))))

(defn parse-reactions
  [s]
  (->> s
       (str/split-lines)
       (map parse-reaction)
       (apply merge)))

(def reaction (parse-reactions (slurp "resources/input-14.txt")))

(defn map-vals
  [f m]
  (reduce-kv
    (fn [m k v] (assoc m k (f v)))
    {} m))

(defn next-dep
  [deps]
  (first
    (->> deps
         (filter (comp pos? second))
         (remove (comp #{"ORE"} first)))))

(defn ore-for-fuel
  [n]
  (-> (loop [deps {"FUEL" n}]
        (if-let [[element quantity] (next-dep deps)]
          (let [n (long (Math/ceil (/ quantity (:yield (reaction element)))))]
            (recur
              (merge-with +
                          deps
                          {element (- (* n (:yield (reaction element))))}
                          (map-vals #(* n %) (:deps (reaction element))))))
          deps))
      (get "ORE")))

(defn solve-p1
  []
  (ore-for-fuel 1))

(defn solve-p2
  []
  (- 1000000000000 (ore-for-fuel 8845261)))

(comment
  (reaction "FUEL")
  (quot 1000000000000 143173))
