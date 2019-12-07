(ns aoc-2019.day-05
  (:require [clojure.string :as str]))

(def memory
  (-> (str/split-lines (slurp "resources/input-05.txt"))
      first
      (str/split #",")
      (->> (mapv #(Integer/parseInt %)))))

(defn digits-low->high
  [x]
  (take 5
        (concat (->> (str x)
                     reverse
                     (map #(Character/getNumericValue ^char %)))
                (repeat 0))))

(defn parse-opcode
  [opcode]
  (let [[op1 op2 c b _] (digits-low->high opcode)]
    (case [op1 op2]
      [1 0] [[c b 1] 1]
      [2 0] [[c b 1] 2]
      [3 0] [[1] 3]
      [4 0] [[c] 4]
      [5 0] [[c b] 5]
      [6 0] [[c b] 6]
      [7 0] [[c b 1] 7]
      [8 0] [[c b 1] 8]
      [9 9] [[] 99])))

(comment
  (parse-opcode 1)
  (parse-opcode 3)
  (parse-opcode 1202)
  (parse-opcode 1101))

(defn lookup
  [memory mode val]
  (case mode
    0 (memory val)
    1 val))

(defn process
  [memory location output input]
  ;(println (map vector (range) memory))
  ;(println "loc" location "mem" (map memory (range location (+ 4 location))) "input" input)
  (let [[modes opcode] (parse-opcode (memory location))
        [x y z] (map (partial lookup memory)
                     modes
                     (subvec memory (inc location)))]
    ;(println "OPCODE" (memory location) "=>" [modes opcode])
    ;(println "MEMORY" (take 3 (subvec memory (inc location))) "=>" [x y z])
    (case opcode
      1 (recur (assoc memory z (+ x y))
               (+ 4 location)
               output
               input)
      2 (recur (assoc memory z (* x y))
               (+ 4 location)
               output
               input)
      3 (recur (assoc memory x (first input))
               (+ 2 location)
               output
               (rest input))
      4 (do #_(println "output" x) [x])
        #_(do
            (println "output" x)
            (recur memory
                   (+ 2 location)
                   (conj output x)
                   input))
      5 (recur memory (if (zero? x) (+ 3 location) y) output input)
      6 (recur memory (if (zero? x) y (+ 3 location)) output input)
      7 (recur (assoc memory z (if (< x y) 1 0)) (+ 4 location) output input)
      8 (recur (assoc memory z (if (= x y) 1 0)) (+ 4 location) output input)
      99 output)))

(defn solve-p1 []
  (process memory 0 nil [1]))

(defn solve-p2 []
  (process memory 0 nil [5]))

(comment
  (process [1101,100,-1,4,0] 0 nil nil)
  (process [1002,4,3,4,33] 0 nil nil)
  (process [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9] 0 nil [1])
  (memory 240))