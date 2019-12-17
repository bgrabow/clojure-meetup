(ns aoc-2019.day-09
  (:refer-clojure :exclude [abs])
  (:require [clojure.string :as str]
            [clojure.math.numeric-tower :refer [abs]]))

(defn parse-memory
  [s]
  (-> (str/split-lines s)
      first
      (str/split #",")
      (->> (mapv #(bigint %)))))

(def memory
  (-> (str/split-lines (slurp "resources/input-09.txt"))
      first
      (str/split #",")
      (->> (mapv #(bigint %)))))

(defn converge
  [f x]
  (butlast
    (reductions
      (fn [x x']
        (if (= x x')
          (reduced x')
          x'))
      (iterate f x))))

(defn digits-low->high
  [x]
  (->> (abs x)
       (iterate #(quot % 10))
       (take-while pos?)
       (mapv #(mod % 10))))

(defn arg-location
  [memory mode location rel-base]
  (case mode
    0 (memory location)
    1 location
    2 (+ rel-base (memory location))))

(comment
  (digits-low->high 99))

(defn range-from
  [start]
  (iterate inc start))

(defn parse-op
  [{:keys [memory location rel-base]}]
  (let [[op1 op2 & modes] (concat (digits-low->high (memory location)) (repeat 0))
        op (+ (* 10 op2) op1)
        arg-locations (map (fn [mode loc] (arg-location memory mode loc rel-base))
                           modes
                           (range-from (inc location)))]
    [op arg-locations]))

(defn plus
  [& args]
  (apply + (remove nil? args)))

(defn mult
  [& args]
  (apply * (remove nil? args)))

(comment
  (mult nil 5 6)
  (plus)
  (+)
  (*))

(defn step-state
  [{:keys [memory location inputs] :as state}]
  (let [[op [x y z]] (parse-op state)]
    ;(println location "  " (select-keys memory (range location (+ 4 location))) "  " op x y z)
    (case op

      ;; Addition
      1 (-> state (assoc-in [:memory z] (plus (memory x) (memory y)))
                  (update :location + 4))

      ;; Multiplication
      2 (-> state (assoc-in [:memory z] (mult (memory x) (memory y)))
                  (update :location + 4))

      ;; Read input
      3 (if (empty? inputs)
          state
          (-> state (assoc-in [:memory x] (first inputs))
                    (update :location + 2)
                    (update :inputs next)))

      ;; Write output
      4 (-> state (update :location + 2)
                  (update :outputs concat (list (memory x))))

      ;; Jump non-zero
      5 (assoc state :location (if ((fnil zero? 0) (memory x))
                                 (+ 3 location)
                                 (memory y)))

      ;; Jump zero
      6 (assoc state :location (if ((fnil zero? 0) (memory x))
                                 (memory y)
                                 (+ 3 location)))

      ;; Test less than
      7 (-> state (assoc-in [:memory z] (if (< (memory x) (memory y))
                                          1 0))
                  (update :location + 4))

      ;; Test equal
      8 (-> state (assoc-in [:memory z] (if (= (memory x) (memory y))
                                          1 0))
                  (update :location + 4))

      ;; Augment relative base
      9 (-> state (update :rel-base + (memory x))
                  (update :location + 2))

      ;; Halt
      99 (dissoc state :inputs))))

(defn run-until-fixed
  [initial-state]
  (reduce
    (fn [prev-state state]
      (if (= prev-state state)
        (reduced state)
        state))
    (iterate step-state initial-state)))

(defn initial-state
  [memory]
  {:memory (if (vector? memory)
             (zipmap (range) memory)
             memory)
   :location 0
   :rel-base 0
   :inputs nil})

(defn solve-p1
  [memory]
  (->> (run-until-fixed {:memory (zipmap (range) memory)
                         :location 0
                         :rel-base 0
                         :inputs [1]})
       :outputs
       first))

(defn solve-p2
  [memory]
  (->> (run-until-fixed {:memory (zipmap (range) memory)
                         :location 0
                         :rel-base 0
                         :inputs [2]})
       :outputs
       first))

(comment
  (solve-p1 memory)
  (solve-p2 memory))

(comment
  (step-state {:memory [1 0 2 1] :location 0})
  (step-state {:memory [1101, 100, -1, 4, 0] :location 0})
  (run-until-fixed {:memory [3 9 8 9 10 9 4 9 99 -1 8] :location 0 :inputs [8]})
  (step-state {:memory [3 9 8 9 10 9 4 9 99 1 8] :location 8 :inputs nil :outputs '(1)})
  (parse-op {:memory [3 9 8 9 10 9 4 9 99 1 8] :location 8 :inputs nil :outputs '(1)})
  (run-until-fixed {:memory (zipmap
                              (range)
                              [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99])
                    :location 0
                    :rel-base 0}))

(comment
  (process-step (process-step [[109 19 204 -34] 0 2000 nil nil]))
  (take 3 (iterate process-step [[109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99] 0 0 nil nil]))
  (run-until-fixed [(into [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99] (repeat 100 0)) 0 0 nil nil])

  (process-step [[9 0] 0 123 nil nil])
  (process-step [[109 5] 0 123 nil nil]))
