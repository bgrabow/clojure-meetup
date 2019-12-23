(ns aoc-2019.day-20
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn parse-maze
  [s]
  (->> (str/split-lines s)
       (mapcat
         (fn [y line]
           (map
             (fn [x cell]
               [[x y] cell])
             (range)
             line))
         (range))
       (into {})))

(def maze (parse-maze (slurp "resources/input-20.txt")))

(defn neighbors
  [p]
  (for [d [[0 1]
           [1 0]
           [0 -1]
           [-1 0]]]
    (mapv + p d)))

(defn portal
  [maze p]
  ;; One of the portal's labels
  (when-let [id-1 (re-find #"[A-Z]" (str (maze p)))]
    ;; The other of the portal's labels
    (when-let [id-2 (some #(re-find #"[A-Z]" (str %)) (map maze (neighbors p)))]
      ;; The portal's jumping off point
      (when-let [entrance (first
                            (filter
                              (comp #{\.} maze)
                              (neighbors p)))]
        [entrance (sort [id-1 id-2])]))))

(def center
  [(/ (apply max (map first (keys maze))) 2)
   (/ (apply max (map second (keys maze))) 2)])

(defn signum
  [x]
  (if (zero? x)
    0
    (/ x (Math/abs ^long x))))

(defn inside-ring?
  [p]
  (let [test-point (->> (map - center p)
                        (map signum)
                        (map #(* % 3))
                        (mapv + p))]
    (= \space (maze test-point))))

(defn format-portal
  [[[p1] [p2]]]
  (when (and p1 p2)
    (vec (sort-by inside-ring? [p1 p2]))))

(def portals {:outward (->> (keep (partial portal maze) (keys maze))
                            (group-by second)
                            (map second)
                            (map format-portal)
                            (into {}))
              :inward (set/map-invert
                        (->> (keep (partial portal maze) (keys maze))
                             (group-by second)
                             (map second)
                             (map format-portal)
                             (into {})))})

(def flat-portals (merge (:inward portals) (:outward portals)))

(def start (-> (keep (partial portal maze) (keys maze))
               (set/map-invert)
               (get (list "A" "A"))))

(def end (-> (keep (partial portal maze) (keys maze))
             (set/map-invert)
             (get (list "Z" "Z"))))

(defn non-euclidean-neighbors
  [location]
  (->> (conj (neighbors location) (flat-portals location))
       (filter (comp #{\.} maze))))

(defn step-search
  [{:keys [frontier visited distance]}]
  (let [new-frontier (->> (mapcat non-euclidean-neighbors frontier)
                          (remove visited)
                          set)]
    {:frontier new-frontier
     :visited (into visited new-frontier)
     :distance (inc distance)}))

(defn solve-p1
  []
  (->> (iterate step-search {:frontier #{start}
                             :visited #{start}
                             :distance 0})
       (drop-while #(not ((:visited %) end)))
       first
       :distance))

(defn recursive-neighbors
  [p]
  (let [inward-neighbor (some-> ((:inward portals) (subvec p 0 2))
                                (conj (inc (p 2))))
        outward-neighbor (when
                           (pos? (p 2))
                           (some-> ((:outward portals) (subvec p 0 2))
                                   (conj (dec (p 2)))))
        flat-neighbors (->> (neighbors (subvec p 0 2))
                            (filter (comp #{\.} maze))
                            (map #(conj % (p 2))))]
    (remove nil? (conj flat-neighbors inward-neighbor outward-neighbor))))

(defn step-recursive-search
  [{:keys [frontier visited distance]}]
  (let [new-frontier (->> (mapcat recursive-neighbors frontier)
                          (remove visited)
                          set)]
    {:frontier new-frontier
     :visited (into visited new-frontier)
     :distance (inc distance)}))

(defn solve-p2
  []
  (->> (iterate step-recursive-search {:frontier #{(conj start 0)}
                                       :visited #{(conj start 0)}
                                       :distance 0})
       (drop-while #(not ((:visited %) (conj end 0))))
       first
       :distance))

(comment
  (recursive-neighbors [59 92 0]))

(comment
  (->> (iterate step-recursive-search {:frontier #{(conj start 0)}
                                       :visited #{(conj start 0)}
                                       :distance 0})))
