(ns aoc-2019.day-02
  (:require [clojure.string :as str]))


(def memory
  (-> (str/split-lines (slurp "resources/input-02.txt"))
      first
      (str/split #",")
      (->> (mapv #(Integer/parseInt %)))))

(defn process
  [memory location]
  (let [[opcode noun verb dest] (subvec memory location)]
    (case opcode
      99 memory
      1 (recur (assoc memory dest (+ (memory noun)
                                     (memory verb)))
               (+ 4 location))
      2 (recur (assoc memory dest (* (memory noun)
                                     (memory verb)))
               (+ 4 location)))))

(defn solve-1
  []
  (first (process (-> memory
                      (assoc 1 12)
                      (assoc 2 2))
                  0)))

(defn solve-2
  []
  (->> (for [noun (range 100)
             verb (range 100)]
         (when (= 19690720
                  (first (process (-> memory
                                      (assoc 1 noun)
                                      (assoc 2 verb))
                                  0)))
           (+ verb (* 100 noun))))
       (remove nil?)))
