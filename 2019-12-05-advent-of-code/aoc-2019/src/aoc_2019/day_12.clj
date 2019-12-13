(ns aoc-2019.day-12
  (:require [clojure.string :as str]
            [gnuplot.core :as g]))

(defn parse-moons
  [s]
  (->> (str/split-lines s)
       (map #(re-seq #"-*\d+" %))
       (map (fn [coords]
              {:r (mapv #(Integer/parseInt %) coords)
               :v [0 0 0]}))))


(def moons
  (parse-moons (slurp "resources/input-12.txt")))

(defn norm
  [x]
  (if (zero? x) 0 (quot x (Math/abs x))))

(defn attraction
  "The pull of a towards b"
  [a b]
  (->> (map - (:r b) (:r a))
       (mapv norm)))

(defn d-velocity
  [moon others]
  (->> others
       (map (partial attraction moon))
       (apply map +)))

(defn step-moons
  [moons]
  (for [{v :v :as moon} moons
        :let [others (disj (set moons) moon)
              dv (d-velocity moon others)
              v' (mapv + v dv)]]
    (-> moon
        (assoc :v v')
        (update :r #(mapv + % v')))))

(defn potential-energy
  [r]
  (reduce + (map #(Math/abs %) r)))

(defn kinetic-energy
  [v]
  (reduce + (map #(Math/abs %) v)))

(defn total-energy
  [moon]
  (* (potential-energy (:r moon))
     (kinetic-energy (:v moon))))

(defn system-energy
  [moons]
  (reduce + (map total-energy moons)))

(defn solve-p1
  []
  (let [moons (first (drop 1000 (iterate step-moons moons)))]
    (system-energy moons)))

(defn nth-component
  [n moon]
  (-> moon
      (update :r subvec n (inc n))
      (update :v subvec n (inc n))))

(defn lcm
  ([x y]
   (* (/ x y)
      (denominator (/ x y))
      y))
  ([x y & more]
   (reduce lcm x (cons y more))))

(comment
  (lcm 2 10)
  (lcm 15 9)
  (lcm 6 9 15))

(comment
  (let [first-repeated-x
        (->> (iterate step-moons moons)
             (reductions
               (fn [past-states s']
                 (let [x-component
                       (map #(nth-component 2 %))]
                   (if (contains? states x-component))))))])

  (let [moons moons]
    (count
      (butlast
        (take-while
          (complement reduced?)
          (drop 1
                (reductions
                  (fn [states s']
                    (let [x-component (map (partial nth-component 2) s')]
                      (if (contains? states x-component)
                        (reduced s')
                        (conj states x-component))))
                  #{}
                  (iterate step-moons moons)))))))

  (let [repeated-state
        (->> (map #(map (partial nth-component 1) %)
                  (iterate step-moons moons))
             (reduce
               (fn [ss s']
                 (if (contains? ss s')
                   (reduced ss)
                   (conj ss s')))
               #{})
             count)])

  (lcm 22958 286332 231614)

  (map system-energy (take 20 (iterate step-moons moons)))

  (->> (iterate step-moons moons)
       (take 2)
       (map #(map () %)))


  (g/raw-plot!
    [[:set :title "test"]
     [:plot (g/range 0 1000)
      (g/list ["-" :title "energy" :with :lines])]]
    [(map vector
          (range)
          (map system-energy
               (mapcat
                 identity
                 (take
                   1000
                   (partition
                     1 1000
                     (iterate step-moons moons))))))]))


