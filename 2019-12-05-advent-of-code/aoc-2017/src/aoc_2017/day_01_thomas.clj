(ns aoc-2017.day-01-thomas)

(def input-chars (butlast (slurp "resources/input-01.txt")))

(reduce
 (fn [acc c])
 []
 input-chars)

(defn my-add
  ([] 0)
  ([x] x)
  ([x y] (+ x y))
  ([x y & more] (my-add (+ x y) (first more) (rest more))))

(def matches
  (loop [x (first input-chars)
         y (fnext input-chars)
         remaining (concat (nnext input-chars) [x])
         matches []]
    (if (nil? y)
      matches
      (if (= x y)
        (recur y
               (first remaining)
               (next remaining)
               (conj matches x))
        (recur y
               (first remaining)
               (next remaining)
               matches)))))

(defn sum [xs] (reduce + 0 xs))

(sum
  (map (comp #(Character/getNumericValue ^char %) first)
    (filter (fn [[x y]]
              (= x y))
            (map vector
                 input-chars
                 (drop (quot (count input-chars) 2) (cycle input-chars))))))


(reduce + 0 (map #(Character/getNumericValue %) matches))

(fn [v] (= (first v) (second v)))
(fn [[x y]] (= x y))
