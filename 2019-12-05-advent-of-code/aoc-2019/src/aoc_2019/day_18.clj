(ns aoc-2019.day-18
  (:require [clojure.string :as str]
            [clojure.set :as set])
  (:import (clojure.lang PersistentTreeSet)))

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

(def maze (parse-maze (slurp "resources/input-18.txt")))

(defn interesting-contents?
  [c]
  (re-find #"[a-z]" (str c)))

(defn door?
  [c]
  (some-> (re-find #"[A-Z]" (str c))
          (.toLowerCase)
          first))

(comment
  (interesting-contents? \A)
  (interesting-contents? \a)
  (interesting-contents? \@))

(def target-locations
  (->> (filter (fn [[_ c]]
                 (interesting-contents? c))
               maze)
       (set/map-invert)))

(def start [40 40])

(defn neighbors
  [p]
  (for [d [[0 1]
           [1 0]
           [0 -1]
           [-1 0]]]
    (mapv + p d)))

(defn conj*
  [coll x]
  (if x (conj coll x) coll))

(defn explore-neighbors
  [explored-maze origin]
  (let [path-stats (explored-maze origin)]
    (->> (neighbors origin)
         (remove explored-maze)
         (remove (comp #{\#} maze))
         (map (fn [p]
                [p (-> path-stats
                       (update :distance inc)
                       (update :deps conj* (door? (maze p))))])))))

(defn step-dijkstra
  [{:keys [explored-maze frontier] :as s}]
  (let [neighbors (mapcat (partial explore-neighbors explored-maze) frontier)]
    (-> s
        (update :explored-maze into neighbors)
        (assoc :frontier (map first neighbors)))))

(defn paths
  [origin]
  (->> (iterate step-dijkstra {:explored-maze {origin {:distance 0
                                                       :deps     #{}}}
                               :frontier      (list origin)})
       (drop-while (comp seq :frontier))
       first
       :explored-maze
       (#(dissoc % origin))
       (keep (fn [[k v]]
               (when (interesting-contents? (maze k))
                 (assoc v :terminals #{(maze k) (maze origin)}))))))

(defn path-distance-comparator
  [x y]
  (let [c (compare (:distance x) (:distance y))]
    (if (zero? c)
      (compare (apply str (:terminals x))
               (apply str (:terminals y)))
      c)))

(def all-paths
  (->> (conj (vals target-locations) start)
       (mapcat paths)
       set
       (apply sorted-set-by
              path-distance-comparator)))

(defn satisfied-deps?
  [deps visited]
  (every? visited deps))

(defn legal-path?
  [location visited {:keys [deps terminals]}]
  ;(println "Legal-path?" location visited deps terminals)
  (and (terminals location) ;; We're on this path.
       (seq (set/difference terminals visited)) ;; We haven't visited both ends of the path yet.
       (satisfied-deps? deps visited))) ;; We can open all the doors on the way.

(defn take-step
  [{:keys [location] :as s} {:keys [terminals] :as step}]
  ;(println terminals location)
  (let [destination (first (set/difference terminals #{location}))]
    ;(println terminals location destination)
    (-> s
        (update :distance + (:distance step))
        (assoc :location destination)
        (update :visited conj destination))))

(defn step-neighbors
  [{:keys [location visited] :as s}]
  (let [paths-to-neighbors (filter (partial legal-path? location visited) all-paths)]
    ;(println paths-to-neighbors)
    (map (partial take-step s) paths-to-neighbors)))

(step-neighbors
  {:location \@
   :visited #{\@}
   :distance 0})

(defn dijkstra
  [neighbors-fn frontier-comparator start]
  (letfn [(step [^PersistentTreeSet frontier]
            (let [head (first frontier)]
              (reduce #(.cons %1 %2)
                      (.disjoin frontier head)
                      (neighbors-fn head))))]
    (iterate step (sorted-set-by frontier-comparator start))))

(defn key-progress-comparator
  [x y]
  (let [c1 (compare (:distance x) (:distance y))]
    (if (zero? c1)
      (let [[visited-x visited-y]
            (map #(apply str (sort (:visited %))) [x y])
            c2 (- (compare (count visited-x) (count visited-y)))]
        (if (zero? c2)
          (compare visited-x visited-y)
          c2))
      c1)))

(comment
  (key-progress-comparator
    {:distance 1
     :visited #{\a \b \c \d}}
    {:distance 0
     :visited #{\b \a \c \e \f}})

  (take (dijkstra step-neighbors
                  key-progress-comparator
                  {:location \@
                   :visited #{\@}
                   :distance 0}))

  (take-while identity
              (iterate (comp first step-neighbors)
                       {:location \r, :visited #{\@ \r}, :distance 28})))

(defn find-path
  [terminals]
  (first (filter #(= terminals (:terminals %)) all-paths)))

(defn solve-p1
  []
  (let [path "yjpmrqsxavdoewcbkilhuzgnft"]
    (reductions
      (fn [{:keys [distance location]} k]
        {:distance (+ distance (:distance (find-path #{location k})))
         :location k})
      {:distance 0
       :location \@}
      path)))

(defn solve-p2
  []
  (let [paths ["jpmyc"
               "rqsxauzt"
               "doew"
               "vbkilhgnf"]]
    (->> (for [path paths]
           (:distance
             (last
               (reductions
                 (fn [{:keys [distance location]} k]
                   {:distance (+ distance (:distance (find-path #{location k})))
                    :location k})
                 {:distance -2
                  :location \@}
                 path))))
         (apply +))))

(def segments
  #{"oew"
    "gnf"
    "jpm"})
