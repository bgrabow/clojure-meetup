(ns aoc-2019.day-11
  (:require [aoc-2019.day-09 :as intcode]
            [clojure.set :as set]
            [clojure.data :as data]))

(def memory (intcode/parse-memory (slurp "resources/input-11.txt")))

(defn panel-color
  [hull p]
  (get hull p :black))

(def color-code {:black 0 :white 1})
(def color (set/map-invert color-code))

(comment
  (intcode/run-until-fixed {:memory   memory
                            :location 0
                            :rel-base 0
                            :inputs   '(0)}))

(defn turn
  [v dtheta]
  (-> {[0 1]  {90 [-1 0] -90 [1 0]}
       [1 0]  {90 [0 1] -90 [0 -1]}
       [0 -1] {90 [1 0] -90 [-1 0]}
       [-1 0] {90 [0 -1] -90 [0 1]}}
      (get v)
      (get dtheta)))

(comment
  ;; +x right +y down +theta clockwise by right hand rule

  ;; Down turns to left
  (= [-1 0] (turn [0 1] 90))
  ;; Left turns to up
  (= [0 -1] (turn [-1 0] 90)))

(def direction {0 -90
                1 90})

(defn step-robot
  [{:keys [hull brain robot-location robot-heading] :as state}]
  (let [c (panel-color hull robot-location)

        {[paint-code turn-code] :outputs :as new-brain}
        (intcode/run-until-fixed (update brain :inputs conj (color-code c)))]

    (cond-> (assoc state :brain (dissoc new-brain :outputs))
            paint-code (assoc-in [:hull robot-location] (color paint-code))
            turn-code (-> (update :robot-heading turn (direction turn-code))
                          (update :robot-location #(mapv + % (turn robot-heading (direction turn-code))))))))

(defn initial-state
  [memory starting-color]
  {:brain          {:memory   (zipmap (range) memory)
                    :location 0
                    :rel-base 0}
   :hull           {[0 0] starting-color}
   :robot-location [0 0]
   :robot-heading  [0 -1]})

(defn iterate-until-fixed
  [f x]
  (reductions
    (fn [x x']
      (if (= x x')
        (reduced x')
        x'))
    (iterate f x)))

(defn solve-p1
  []
  (->> (initial-state memory :black)
       (iterate-until-fixed step-robot)
       (last)
       (:hull)
       (count)))

(defn print-2D
  [grid-contents]
  (let [x-min       (apply min (map first (keys grid-contents)))
        x-max       (apply max (map first (keys grid-contents)))
        y-min       (apply min (map second (keys grid-contents)))
        y-max       (apply max (map second (keys grid-contents)))
        render-cell #(case %
                       :white \X
                       :black \space
                       \space)]
    (doseq [y (range y-min (inc y-max))]
      (println
        (apply str
               (map
                 (fn [x] (render-cell (grid-contents [x y])))
                 (range x-min (inc x-max))))))))

(defn solve-p2
  []
  (->> (initial-state memory :white)
       (iterate-until-fixed step-robot)
       (last)
       (:hull)
       (print-2D)))

(comment
  (solve-p1)
  (solve-p2))

(comment
  (step-robot (step-robot (initial-state memory)))

  (do (last (take 10 (iterate-until-fixed
                       step-robot
                       (initial-state memory))))
      nil)
  (last (iterate-until-fixed step-robot (initial-state memory)))

  (->> (iterate-until-fixed step-robot (initial-state memory))
       (mapv (comp print-2D :hull))
       (#(do % nil)))

  (take 2 (iterate step-robot (initial-state memory)))
  (count memory))
