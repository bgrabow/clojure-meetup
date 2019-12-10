(ns aoc-2019.intcode-test
  (:require [clojure.test :refer :all]
            [aoc-2019.day-09 :as d9]))

(defn step-09
  [{:keys [memory loc rel-base]}]
  (let [[m l rb] (d9/process-step
                   [memory loc rel-base])]
    {:memory m
     :location l
     :rel-base rb}))

(defn submap?
  [inner outer]
  (= inner (select-keys outer (keys inner))))

(deftest parse-op-test
  (= (aoc-2019.day-09/parse-op {:memory [21004 3 5 7]
                                :location 0
                                :rel-base 50})))

(deftest step-one-computer
  #_(are [initial stepped]
      (submap? stepped (step-09 initial))

      ;; Addition
      {:memory [1 2 3 0] :location 0}
      {:memory [3 2 3 0] :location 4}

      {:memory [1101 2 3 1] :location 0}
      {:memory [1101 5 3 1] :location 4}


      ;; Multiplication


      ;; Rel-loc modification

      {:memory [9 2 10] :location 0 :rel-base 5}
      {:memory [9 2 10] :location 2 :rel-base 15}

      {:memory [109 2 10] :location 0 :rel-base 5}
      {:memory [109 2 10] :location 2 :rel-base 7})

  (are [initial stepped]
    (submap? stepped (aoc-2019.day-09/step-state initial))

    ;; Addition
    {:memory [1 2 3 0] :location 0}
    {:memory [3 2 3 0] :location 4}

    {:memory [1101 2 3 1] :location 0}
    {:memory [1101 5 3 1] :location 4}


    ;; Multiplication


    ;; Rel-loc modification

    {:memory [9 2 10] :location 0 :rel-base 5}
    {:memory [9 2 10] :location 2 :rel-base 15}

    {:memory [109 2 10] :location 0 :rel-base 5}
    {:memory [109 2 10] :location 2 :rel-base 7}))
