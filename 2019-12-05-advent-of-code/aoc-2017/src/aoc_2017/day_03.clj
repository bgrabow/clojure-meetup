(ns aoc-2017.day-03)

(def input
  (read-string (re-find #"\d+" (slurp "resources/input-03.txt"))))

(def distances
  (flatten
   (list '(0)
         (for [low (drop 1 (range))
               :let
               [start (dec (* low 2))
                high (inc (* low 2))]]
           (repeat 4 (list (range start low -1) (range low high)))))))

(defn distance [n]
  (nth distances (dec n)))

(comment
 (distance input)
 (distance 1))

[[[1 0]]
 [(repeat 1 [0 1])
  (repeat 2 [-1 0])
  (repeat 2 [0 -1])
  (repeat 3 [1 0])]
 [(repeat 3 [0 1])
  (repeat 4 [-1 0])
  (repeat 4 [0 -1])
  (repeat 5 [1 0])]
 [(repeat 5 [0 1])
  (repeat 6 [-1 0])
  (repeat 6 [0 -1])
  (repeat 7 [1 0])]]

(defn ring [n]
  (let [side (* 2 n)]
    (concat
     (repeat (dec side) [0 1])
     (repeat side [-1 0])
     (repeat side [0 -1])
     (repeat (inc side) [1 0]))))

(def steps
  (apply concat (map ring (range))))

(def spiral-out
  (reductions (fn [p d] (mapv + p d)) [0 0] steps))

(defn manhattan-distance [p1 p2]
  (let [[x1 y1] p1
        [x2 y2] p2]
    (+ (Math/abs (- x1 x2))
       (Math/abs (- y1 y2)))))

(defn neighbors [[x y]]
  (for [dx [-1 0 1]
        dy [-1 0 1]
        :when (not= [x y] [0 0])]
    (mapv + [x y] [dx dy])))

(defn neighbor-value [m p]
  (->> (neighbors p)
       (map m)
       (remove nil?)
       (reduce + 0)))

(defn neighbor-values [start init path]
  (reductions (fn [m p]
                (assoc m p (neighbor-value m p)))
              {start init}
              (path)))


(comment
 (manhattan-distance [0 0] (nth spiral-out (dec input)))
 (first (drop-while
          #(< % input)
          (map (fn [m p] (get m p)) (neighbor-values [0 0] 1 (rest spiral-out)) spiral-out))))
