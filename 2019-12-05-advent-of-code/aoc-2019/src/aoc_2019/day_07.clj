(ns aoc-2019.day-07
  (:require [aoc-2019.day-05 :refer [process parse-opcode lookup]]
            [clojure.math.combinatorics :as c]
            [clojure.string :as str]
            [clojure.core.async :as a]))

(def phase-combinations (c/permutations (range 5)))

(def memory
  (-> (str/split-lines (slurp "resources/input-07.txt"))
      first
      (str/split #",")
      (->> (mapv #(Integer/parseInt %)))))

(defn amp
  [memory phase input]
  (first (process memory 0 nil [phase input])))

(defn solve-p1
  [memory]
  (apply max (for [phases phase-combinations]
               ((apply comp (map #(partial amp memory %) phases)) 0))))

(comment
  (solve-p1 [3, 15, 3, 16, 1002, 16, 10, 16, 1, 16, 15, 15, 4, 15, 99, 0, 0])
  (solve-p1 [3, 23, 3, 24, 1002, 24, 10, 24, 1002, 23, -1, 23,
             101, 5, 23, 23, 1, 24, 23, 23, 4, 23, 99, 0, 0])
  (solve-p1 [3, 31, 3, 32, 1002, 32, 10, 32, 1001, 31, -2, 31, 1007, 31, 0, 33,
             1002, 33, 7, 33, 1, 33, 31, 31, 1, 32, 31, 31, 4, 31, 99, 0, 0, 0])
  (solve-p1 memory))

(comment
  (defn process-chan
    [memory location input]
    (let [output (a/chan)]
      (a/go-loop [memory memory
                  location location]
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
                     (+ 4 location))
            2 (recur (assoc memory z (* x y))
                     (+ 4 location))
            3 (if-let [val (a/<! input)]
                (do (println "Read from input" val)
                    (recur (assoc memory x val) (+ 2 location)))
                (a/close! output))
            4 (do
                (do println "output" x)
                (a/>! output x)
                (recur memory
                       (+ 2 location)))
            5 (recur memory (if (zero? x) (+ 3 location) y))
            6 (recur memory (if (zero? x) y (+ 3 location)))
            7 (recur (assoc memory z (if (< x y) 1 0)) (+ 4 location))
            8 (recur (assoc memory z (if (= x y) 1 0)) (+ 4 location))
            99 (a/close! output))))
      output)))

(defn process-step
  [[memory location inputs outputs]]
  ;(println (map vector (range) memory))
  ;(println "loc" location "mem" (map memory (range location (+ 4 location))) "input" input)
  (let [[modes opcode] (parse-opcode (memory location))
        [x y z] (map (partial lookup memory)
                     modes
                     (subvec memory (inc location)))]
    ;(println "OPCODE" (memory location) "=>" [modes opcode])
    ;(println "MEMORY" (take 3 (subvec memory (inc location))) "=>" [x y z])
    (case opcode
      1 [(assoc memory z (+ x y)) (+ 4 location) inputs outputs]
      2 [(assoc memory z (* x y)) (+ 4 location) inputs outputs]
      3 (if (empty? inputs)
          [memory location inputs outputs] ;; Fixed point, emulate halt
          [(assoc memory x (first inputs)) (+ 2 location) (rest inputs) outputs])
      4 [memory (+ 2 location) inputs (conj outputs x)]
      5 [memory (if (zero? x) (+ 3 location) y) inputs outputs]
      6 [memory (if (zero? x) y (+ 3 location)) inputs outputs]
      7 [(assoc memory z (if (< x y) 1 0)) (+ 4 location) inputs outputs]
      8 [(assoc memory z (if (= x y) 1 0)) (+ 4 location) inputs outputs]
      99 [memory location inputs outputs]))) ;; Fixed point, halt

(defn run-until-fixed
  [initial-state]
  (reduce
    (fn [prev-state state]
      (if (= prev-state state)
        (reduced state)
        state))
    (iterate process-step initial-state)))

(defn process-one-input
  [state input]
  (run-until-fixed (update state 2 conj input)))

(defn primed-amp
  [memory phase]
  (run-until-fixed [memory 0 [phase] nil]))

(defn step-amps
  [[input amps]]
  (let [[_ _ _ outputs :as amp1] (process-one-input (first amps) input)]
    [(first outputs) (conj (subvec amps 1) (update amp1 3 rest))]))

(defn solve-p2
  [memory]
  (apply max
         (for [phases (c/permutations (range 5 10))]
           (let [amps (mapv (partial primed-amp memory) phases)]
             (->> (rest (iterate step-amps [0 amps]))
                (map first)
                (take-while some?)
                (partition 5)
                (last)
                (last))))))

(comment
  (time (solve-p2 memory))
  (solve-p2 [3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,
             27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5])
  (solve-p2 [3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,
             -5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,
             53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10]))

(comment
  (run-until-fixed [[3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,
                     27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5]
                    0 [9] nil]))

(comment ; Well, I tried...
  (defn amp-step
    [memory phase]
    (let [primed (run-until-fixed [memory 0 [phase] nil])]))

  (defn process-all-inputs
    [[memory location] inputs]
    (reduce
      (fn [prev-state [_ _ inputs :as state]]
        (cond
          (= prev-state state) (reduced state)
          (empty? inputs) (reduced state)
          :else state))
      (iterate process-step [memory location inputs nil])))

  (defn wait-for-output
    [[memory location]]
    (first
      (drop-while
        (fn [[_ _ _ outputs]] (empty? outputs))
        (iterate process-step [memory location [] nil]))))

  (defn amp-chan
    [memory input]
    (process-chan memory 0 input))

  (def e-outputs (atom []))

  (defn solve-p2
    [memory]
    (for [phases (c/permutations (range 5 10))]
      (let [inputs (map (fn [p] (let [c (a/chan 10)]
                                  (a/put! c p)
                                  c))
                        phases)
            outputs (map (partial amp-chan memory) inputs)]

        (a/put! (first inputs) 0)

        (doseq [from outputs
                to (next inputs)]
          (a/pipe from to))

        (a/go-loop
          []
          (when-let [e-val (a/<! (last outputs))]
            (do (swap! e-outputs conj e-val)
                (recur))))))))

(comment
  (solve-p2 [3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,
             27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5]))