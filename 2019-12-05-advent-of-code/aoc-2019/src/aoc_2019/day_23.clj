(ns aoc-2019.day-23
  (:require [aoc-2019.day-09 :as intcode]))

(def memory (intcode/parse-memory (slurp "resources/input-23.txt")))

(def n-computers 50)

(defn boot-computer
  [memory address]
  (intcode/run-until-fixed
    (assoc (intcode/initial-state memory)
      :inputs [address])))

(defn collate-packets
  [packets]
  (let [payload (reduce
                  (fn [packet-queues [addr x y]]
                    (update packet-queues addr (fnil conj []) x y))
                  {}
                  (partition 3 packets))
        defaults (merge (zipmap (range n-computers) (repeat [-1])))]
    (merge defaults payload)))

(defn step-network
  [computers]
  (let [packets (->> computers
                     (mapcat :outputs)
                     collate-packets)]
    (->> computers
         (map-indexed (fn [i c]
                        (-> (assoc c :inputs (packets i))
                            (dissoc :outputs))))
         (map intcode/run-until-fixed))))


(defn solve-p1
  []
  (let [computers (->> (range 50)
                       (mapv (partial boot-computer memory)))
        [_ y] (->> (iterate step-network computers)
                   (map #(mapcat :outputs %))
                   (map collate-packets)
                   (some #(get % 255)))]
    y))
