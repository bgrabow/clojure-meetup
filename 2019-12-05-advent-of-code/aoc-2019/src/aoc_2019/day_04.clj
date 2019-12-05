(ns aoc-2019.day-04)

(def input
  (->> (re-seq #"\d+" (slurp "resources/input-04.txt"))
       (map #(Integer/parseInt %))))

(defn digits
  [x]
  (map #(Character/getNumericValue ^char %) (str x)))

(defn increasing-digits?
  [x]
  (apply <= (digits x)))

(defn has-consecutive-repeat?
  [x]
  (some #(apply = %) (partition 2 1 (digits x))))

(defn has-exclusive-pair?
  [x]
  (some #(= 2 %) (vals (frequencies (digits x)))))

(comment
  (->> (digits 112)
       frequencies
       vals))

(comment
  (has-exclusive-pair? 112))

(defn valid-password?
  [x]
  (and (increasing-digits? x)
       (has-consecutive-repeat? x)))

(defn solve-p1
  []
  (count (filter valid-password? (apply range input))))

(defn stronger-password?
  [x]
  (and (increasing-digits? x)
       (has-exclusive-pair? x)))

(defn solve-p2
  []
  (count (filter stronger-password? (apply range input))))
