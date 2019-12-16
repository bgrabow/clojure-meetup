(ns aoc-2019.day-16
  (:require [clojure.string :as str]))

(slurp "resources/input-16.txt")

(defn coefficients
  [digit-number]
  (drop 1 (cycle (mapcat (partial repeat digit-number) [0 1 0 -1]))))

(comment
  (take 50 (map #(take 50 %)
                (map #(coefficients %) (drop 1 (range))))))

(defn digits
  [s]
  (map #(Integer/parseInt %) (map str s)))

(defn next-phase
  [phase]
  (->> (map coefficients (drop 1 (range)))
       (take (count (digits phase)))
       (map #(reduce + (map * (digits phase) %)))
       (map (comp last str))
       (apply str)))

(comment
  (next-phase "12345678")
  (take 5 (iterate next-phase "12345678"))
  (nth (iterate next-phase "80871224585914546619083218645595") 100)
  (time (nth (iterate next-phase (first (str/split-lines (slurp "resources/input-16.txt")))) 100))

  (take 8 (drop 5971723 (coefficients 5971724)))
  (* 10000 (count (first (str/split-lines (slurp "resources/input-16.txt")))))
  (Integer/parseInt (apply str (take 7 (first (str/split-lines (slurp "resources/input-16.txt")))))))

(comment
  (nth (coefficients 5971724) 5971722)
  (map #(take 50 (drop 5971723 (coefficients %))) (range 5971724 (+ 8 5971724))))

(defn triangular-descendent
  [phase]
  (apply str
         (reduce
           (fn [digits d]
             (let [s (or (first digits) 0)]
               (cons (mod (+ d s) 10)
                     digits)))
           (list)
           (map #(Character/getNumericValue ^char %)
                (reverse phase)))))

(defn solve-p2
  [input]
  (let [start (Integer/parseInt (subs input 0 7))
        input (drop start (apply str (mapcat identity (repeat 10000 input))))
        phases (iterate triangular-descendent (apply str input))]
    (apply str (take 8 (nth phases 100)))))

(defn parse-phase
  [s]
  (first (str/split-lines s)))

(comment
  (digit-at "12345678" 4 7)
  (let [input (apply str (mapcat identity (repeat 10000 "03036732577212944063491565474664")))
        start (Integer/parseInt (subs input 0 7))]
    (digit-at input 100 start))
  (solve-p2 "03036732577212944063491565474664")
  (solve-p2 "02935109699940807407585447034323")
  (time (solve-p2 (parse-phase (slurp "resources/input-16.txt"))))
  (take 5 (iterate triangular-descendent "12345678")))

