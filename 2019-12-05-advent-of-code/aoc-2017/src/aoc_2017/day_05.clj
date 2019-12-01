(ns aoc-2017.day-05
  (:require [clojure.string :as str]
            [clojure.edn :as edn]))

(defn my-if [condition true-branch false-branch]
  (if condition true-branch false-branch))

(my-if true (println "true") (println "false"))

(if true (println "True") (println "False"))

{:token 'if
 :children ['true
            {:token 'println
             :children ['"True"]}
            {:token 'println
             :children ['"False"]}]}

(+ (- 10 5) (* 5 9))

(def input
  (mapv #(Integer/parseInt %)
       (str/split-lines (slurp "resources/input-05.txt"))))

(defn next-value [old-value]
  (if (>= old-value 3)
    (dec old-value)
    (inc old-value)))

(defn solve [xs current n-iterations]
  (let [jump (get xs current)]
    (if jump
      (recur (update xs current next-value)
             (+ jump current)
             (inc n-iterations))
      n-iterations)))

(solve input 0 0)

(defn step [[xs current]]
  (let [jump (get xs current)]
    [(update xs current next-value)
     (+ jump current)]))

(def sample-input [0 3 0 1 -3])

(defn in-bounds? [[coll ix]]
  (<= 0 ix (quot (count coll) 2)))

(comment
  (in-bounds? [[0 1 2] 3]))

(def steps (iterate step [input 0]))
(count (take-while in-bounds? steps))

(def person {:fname "Thomas"
             :lname "Grabow"})

(def employee {:fname "Thomas"
               :lname "Grabow"
               :emp-id "abc123"
               :salary "150000"
               :email "thomas.grabow@raytheon.com"})

(def contact {:fname "Thomas"
              :lname "Grabow"
              :email "thomas.grabow@raytheon.com"})

(merge employee contact)
