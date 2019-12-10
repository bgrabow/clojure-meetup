(ns aoc-2019.day-10
  (:require [clojure.string :as str]))

(defn parse-space
  [space]
  (->> (into {}
             (mapcat
               (fn [y line]
                 (map
                   (fn [x space]
                     [[x y] space])
                   (range)
                   line))
               (range)
               (str/split-lines space)))
       (keep (fn [[p space]]
               (when (= space \#) p)))
       (into #{})))

(def space
  (parse-space (slurp "resources/input-10.txt")))

(defn unit-vector
  "Vector pointing from p1 to p2"
  [p1 p2]
  (let [[dx dy] (map - p2 p1)]
    (if (zero? dx)
      [0 (if (pos? dy) 1 -1)]
      [(if (pos? dx) 1 -1)
       (if (pos? dx) (/ dy dx) (- (/ dy dx)))])))

(comment
  [1 -2]
  [1 -2]
  [-1 2]
  [-1 2]
  (unit-vector [0 0] [2 -1]))

(defn n-asteroids-in-los
  [asteroid space]
  (count (distinct (map (partial unit-vector asteroid) space))))

(defn find-best-vantage-point
  [space]
  (apply (partial max-key first)
         (for [asteroid space]
           [(n-asteroids-in-los asteroid (disj space asteroid))
            asteroid])))

(def best-vantage-point
  (second (find-best-vantage-point space)))

(defn solve-p1
  []
  (first
    (find-best-vantage-point space)))

(defn manhattan-distance
  [p1 p2]
  (reduce + (map #(Math/abs ^long (- %1 %2)) p1 p2)))

(defn asteroid-headings
  [base others]
  (map vector (map (partial unit-vector base) others)
              (for [target others]
                {:coordinates target
                 :distance (manhattan-distance base target)})))

(defn angle
  [[dx dy]]
  (let [[dx' dy'] [dx (- dy)]]
    (mod (+ (* 2 Math/PI) (Math/atan2 dx' dy')) (* 2 Math/PI))))

(comment
  ;; Image coordinates (+x right +y down)
  (= Math/PI (angle [0 1]))
  (= 0.0 (angle [0 -1]))
  (= (* Math/PI (/ 3 2)) (angle [-1 0]))
  (= (* Math/PI (/ 1 2)) (angle [1 0])))

(defn compute-firing-sequence
  [base space]
  (->> (reduce (fn [m [k v]]
                 (update m k conj v))
               {}
               (asteroid-headings base (disj space base)))
       (map (fn [[k v]]
              [k (sort-by :distance v)]))
       (sort-by (comp angle first))
       (vec)))

(defn shoot-one
  [firing-sequence]
  (let [current-heading (first firing-sequence)
        remaining (update current-heading 1 rest)]
    (if (seq (second remaining))
      (conj (subvec firing-sequence 1) (update current-heading 1 rest))
      (subvec firing-sequence 1))))

(defn next-target
  [firing-sequence]
  (-> firing-sequence
      first
      second
      first))

(defn solve-p2
  []
  (let [[_ base] (find-best-vantage-point space)
        [[[_ [{[x y] :coordinates}]]]] (->> (compute-firing-sequence base space)
                                            (iterate shoot-one)
                                            (drop 199)
                                            (take 1))]
    (+ y (* 100 x))))

(comment
  (solve-p1)
  (solve-p2))

(comment
  (let [test-space (parse-space ".#....#####...#..\n##...##.#####..##\n##...#...#.#####.\n..#.....X...###..\n..#.#.....#....##")
        test-space-2 (parse-space ".#..##.###...#######\n##.############..##.\n.#.######.########.#\n.###.#######.####.#.\n#####.##.#.##.###.##\n..#####..#.#########\n####################\n#.####....###.#.#.##\n##.#################\n#####.##.###..####..\n..######..##.#######\n####.##.####...##..#\n.#####..#.######.###\n##...#.##########...\n#.##########.#######\n.####.#.###.###.#.##\n....##.##.###..#####\n.#.#.###########.###\n#.#.#.#####.####.###\n###.##.####.##.#..##")]
    (map next-target (take 20 (drop 0 (iterate shoot-one (compute-firing-sequence [8 3] test-space)))))
    (map next-target (take 4 (drop 198 (iterate shoot-one (compute-firing-sequence (second (find-best-vantage-point test-space-2)) test-space-2)))))
    (map next-target (take 4 (drop 198 (iterate shoot-one (compute-firing-sequence (second (find-best-vantage-point space)) space)))))))
