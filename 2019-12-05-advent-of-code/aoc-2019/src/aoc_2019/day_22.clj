(ns aoc-2019.day-22
  (:require [clojure.string :as str]
            [clojure.edn :as edn]))

(defn relative-index
  [n size]
  (mod (+ n size) size))

(defn new-stack
  [coll]
  (vec (reverse coll)))

(defn cut
  [n coll]
  (->> (split-at (relative-index n (count coll))
                 coll)
       reverse
       flatten
       (into [])))

(defn deal-with-increment
  [n coll]
  (vec
    (take
      (count coll)
      (flatten
        (partition 1 n (cycle coll))))))

(defn parse-new-stack
  [s]
  (when (re-find #"deal into new stack" s)
    new-stack))

(comment
  ((parse-new-stack "deal into new stack") (range 7)))

(defn parse-cut
  [s]
  (when-let [[_ n] (re-find #"cut (.+)" s)]
    (partial cut (Long/parseLong n))))

(comment
  ((parse-cut "cut -2") (range 5)))

(defn parse-deal-with-increment
  [s]
  (when-let [[_ n] (re-find #"deal with increment (.+)" s)]
    (partial deal-with-increment (Long/parseLong n))))

(comment
  ((parse-deal-with-increment "deal with increment 3") (range 5)))

(defn deal-with-increment
  [n coll]
  (->> (map vector
            (flatten (partition 1 n (cycle (range (count coll)))))
            coll)
       sort
       (mapv second)))

(defn parse-deal
  [s]
  (some identity
        ((juxt parse-new-stack
               parse-cut
               parse-deal-with-increment) s)))

(comment
  (deal-with-increment 3 (vec (range 7)))
  [0 1 2 3 4 5 6]
  [0 5 3 1 6 4 2]

  (->> (map vector
            (flatten (partition 1 3 (cycle (range 7))))
            (range 7))
       sort
       (map second)))

(comment
  (split-at 2 [1 2 3 4 5])
  (cut 2 (range 10)))

(def final-deck (reduce (fn [deck f]
                          (f deck))
                        (vec (range 10007))
                        (map parse-deal (str/split-lines
                                          (slurp "resources/input-22.txt")))))

(defn solve-p1
  []
  (some identity (map-indexed (fn [n x]
                                (when (= x 2019)
                                  n)) final-deck)))

(def deck-size 119315717514047)
(def shuffle-n 101741582076661)

(deal-with-increment 7 (range 10))
(take 10 (drop 6 (cycle (reverse (deal-with-increment 7 (reverse (range 10)))))))

(defn numerator
  [x]
  (if (ratio? x)
    (clojure.core/numerator x)
    x))

(defn position-before-new-stack
  [size pos]
  (- size pos))

(comment

  (= (/ (+ (* a size) pos') n) pos)

  (= (mod (* n pos) size) pos')
  (= (* n pos) (+ pos' (* a size)))
  (= (+ (* a size) pos') (* n pos))
  (= (/ (+ (* a size) pos') n) pos)
  (= (numerator (/ (+ size pos') n) pos))

  (mod (numerator (/ (+ 10 1) 3)) 10)

  (position-after-dwi 10 3 3)
  (range 10)
  (deal-with-increment 3 (range 10))
  (position-before-deal-with-increment 10 3 0)
  (map (comp long (partial position-before-deal-with-increment 10 3)) (range 10)))

(defn position-before-cut
  [size n pos]
  (relative-index (+ pos n) size))

(comment
  (position-before-cut 10 3 9)
  (cut 3 (range 10))

  (deal-with-increment 12 (range 31))

  (position-before-new-stack deck-size 2020))

(defn position-after-cut
  [size n pos]
  (relative-index (- pos n) size))

(defn position-after-reverse
  [size pos]
  (- size pos))

(defn position-after-dwi
  [size n pos]
  (mod (* n pos) size))

(comment
  (- 68 (rem deck-size 68))
  (- 68 (rem (* 2 deck-size) 68))
  (- 68 (rem (* 3 deck-size) 68))
  (rem (* 2 45) 68)
  (rem (* 3 45) 68)
  (get (zipmap (map #(rem (* % 45) 68) (range 68)) (range 68))
       48)

  (quot 2020 68)
  (rem 2020 68)
  (+ (* 60 68) 29)
  (position-after-dwi deck-size 68 2749)
  (position-after-dwi deck-size 68 4109)
  (quot 186932 68))

(defn interleaved-offset*
  [size n v]
  (let [y0 (- n (rem size n))]
    (get (zipmap (map #(rem (* % y0) n) (range n)) (range n)) v)))

(def interleaved-offset (memoize interleaved-offset*))

(defn pos-before-dwi
  [size n pos]
  (if (zero? pos)
    0
    (let [v (rem pos n)
          y (interleaved-offset size n v)
          a (+ (quot (* y size) n) (inc (quot (dec pos) n)))]
      a)))

(comment
  (do (doseq [x (range 1000)]
        (let [p (pos-before-dwi deck-size 68 x)]
          (println x (position-after-dwi deck-size 68 p)
                   (for [y (range (dec p) (inc (inc p)))]
                     (position-after-dwi deck-size 68 y)))))
      nil)


  (frequencies (for [x (range 100000)]
                 (let [p (pos-before-dwi deck-size 68 x)]
                   (= x (position-after-dwi deck-size 68 p)))))

  (pos-before-dwi deck-size 68 0)
  (rem 0 68)
  (- 68 (rem deck-size 68))
  (get (zipmap (map #(rem (* % 45) 68) (range 68)) (range 68)) 0)
  (+ (quot 0 68) (quot 0 68))

  (+ (quot (* 60 deck-size) 68))
  (position-after-dwi deck-size 68 105278574277130)

  (quot deck-size 68)
  (position-after-dwi deck-size 68 1754642904619)
  (* 68 1754642904618)

  (quot deck-size 68)
  (quot (* 2 deck-size) 68)
  (quot (* 3 deck-size) 68))
