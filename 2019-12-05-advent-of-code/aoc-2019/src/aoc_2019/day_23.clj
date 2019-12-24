(ns aoc-2019.day-23
  (:require [aoc-2019.day-09 :as intcode]))

(def memory (intcode/parse-memory (slurp "resources/input-23.txt")))

(def n-computers 50)

(defn boot-computer
  [memory addr]
  (intcode/run-until-fixed
    (assoc (intcode/initial-state memory)
      :inputs [addr])))

(defn collate-packets
  [packets]
  (let [inputs (reduce (fn [packet-queues [addr x y]]
                         (update packet-queues addr (fnil conj []) x y))
                       {}
                       (partition 3 packets))
        defaults (zipmap (range n-computers) (repeat [-1]))]
    (merge defaults inputs)))

(defn step-network
  [computers]
  (let [packets (collate-packets (mapcat :outputs computers))]
    (map-indexed (fn [i c]
                   (intcode/run-until-fixed
                     (-> (assoc c :inputs (packets i))
                         (dissoc :outputs))))
                 computers)))

(defn solve-p1
  []
  (let [computers (map (partial boot-computer memory) (range n-computers))
        [_ y] (->> (iterate step-network computers)
                   (map #(collate-packets (mapcat :outputs %)))
                   (some #(get % 255)))]
    y))

(defn iterate-until-fixed
  [f x]
  (reductions
    (fn [x x']
      (if (= x x')
        (reduced x')
        x'))
    (iterate f x)))

(defn step-NAT-network
  [{:keys [computers nat]}]
  (let [nat (or (->> (iterate-until-fixed step-network computers)
                     (map #(collate-packets (mapcat :outputs %)))
                     (keep #(get % 255))
                     (last)
                     (partition 2)
                     (last))
                nat)
        computers (vec (last (iterate-until-fixed step-network computers)))]
    {:computers (-> computers
                    (assoc-in [0 :inputs] nat)
                    (->> (map intcode/run-until-fixed)))
     :nat nat}))

(defn first-duplicate-value
  [coll]
  (reduce (fn [y y'] (if (= y y')
                       (reduced y')
                       y'))
          coll))

(defn solve-p2
  []
  (let [computers (map (partial boot-computer memory) (range n-computers))]
    (->> (iterate step-NAT-network {:computers computers :nat nil})
         (map :nat)
         (map second)
         first-duplicate-value)))
