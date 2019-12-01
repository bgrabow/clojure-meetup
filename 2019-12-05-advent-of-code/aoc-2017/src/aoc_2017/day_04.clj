(ns aoc-2017.day-04
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as cs]))

(def input
  (->> (slurp "resources/input-04.txt")
    (str/split-lines)
    (map #(str/split % #"\s"))))

(defn valid-passphrase? [words]
  (= (count words) (count (set words))))

(defn anagrams? [x y]
  (= (frequencies x)
     (frequencies y)))

(defn no-anagrams? [words]
  (every? (fn [[a b]] (not (anagrams? a b))) 
          (cs/combinations words 2)))

(comment
 (anagrams? "abc" "cba")
 (anagrams? "aabc" "cba")
 (anagrams? "aabc" "caba"))

(comment
 (count (filter valid-passphrase? input))
 (count (filter no-anagrams? input)))

(doc set)
(doc str/split)
(require '[clojure.repl :refer :all])
