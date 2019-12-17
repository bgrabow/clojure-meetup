(ns aoc-2019.day-14
  (:require [clojure.string :as str]))

(def test-inputs
  {"10 ORE => 10 A\n1 ORE => 1 B\n7 A, 1 B => 1 C\n7 A, 1 C => 1 D\n7 A, 1 D => 1 E\n7 A, 1 E => 1 FUEL" 31
   "9 ORE => 2 A\n8 ORE => 3 B\n7 ORE => 5 C\n3 A, 4 B => 1 AB\n5 B, 7 C => 1 BC\n4 C, 1 A => 1 CA\n2 AB, 3 BC, 4 CA => 1 FUEL" 165
   "157 ORE => 5 NZVS\n165 ORE => 6 DCFZ\n44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL\n12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ\n179 ORE => 7 PSHF\n177 ORE => 5 HKGWZ\n7 DCFZ, 7 PSHF => 2 XJWVT\n165 ORE => 2 GPVTF\n3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT" 13312
   "2 VPVL, 7 FWMGM, 2 CXFTF, 11 MNCFX => 1 STKFG\n17 NVRVD, 3 JNWZP => 8 VPVL\n53 STKFG, 6 MNCFX, 46 VJHF, 81 HVMC, 68 CXFTF, 25 GNMV => 1 FUEL\n22 VJHF, 37 MNCFX => 5 FWMGM\n139 ORE => 4 NVRVD\n144 ORE => 7 JNWZP\n5 MNCFX, 7 RFSQX, 2 FWMGM, 2 VPVL, 19 CXFTF => 3 HVMC\n5 VJHF, 7 MNCFX, 9 VPVL, 37 CXFTF => 6 GNMV\n145 ORE => 6 MNCFX\n1 NVRVD => 8 CXFTF\n1 VJHF, 6 MNCFX => 4 RFSQX\n176 ORE => 6 VJHF" 180697
   "171 ORE => 8 CNZTR\n7 ZLQW, 3 BMBT, 9 XCVML, 26 XMNCP, 1 WPTQ, 2 MZWV, 1 RJRHP => 4 PLWSL\n114 ORE => 4 BHXH\n14 VRPVC => 6 BMBT\n6 BHXH, 18 KTJDG, 12 WPTQ, 7 PLWSL, 31 FHTLT, 37 ZDVW => 1 FUEL\n6 WPTQ, 2 BMBT, 8 ZLQW, 18 KTJDG, 1 XMNCP, 6 MZWV, 1 RJRHP => 6 FHTLT\n15 XDBXC, 2 LTCX, 1 VRPVC => 6 ZLQW\n13 WPTQ, 10 LTCX, 3 RJRHP, 14 XMNCP, 2 MZWV, 1 ZLQW => 1 ZDVW\n5 BMBT => 4 WPTQ\n189 ORE => 9 KTJDG\n1 MZWV, 17 XDBXC, 3 XCVML => 2 XMNCP\n12 VRPVC, 27 CNZTR => 2 XDBXC\n15 KTJDG, 12 BHXH => 5 XCVML\n3 BHXH, 2 VRPVC => 7 MZWV\n121 ORE => 7 VRPVC\n7 XCVML => 6 RJRHP\n5 BHXH, 4 VRPVC => 5 LTCX" 2210736})

(defn parse-reaction
  [s]
  (let [reactants (->> (re-seq #"(\d+) (\w+)" s)
                       (map rest)
                       (map (fn [[n element]]
                              {:quantity (Integer/parseInt n)
                               :product element})))
        inputs (butlast reactants)
        out (last reactants)]
    (merge out {:inputs (set inputs)})))

(comment
  (parse-reaction "10 ORE => 10 A")
  (map rest (re-seq #"(\d+) (\w+)" "10 ORE => 10 A")))

(defn parse-reactions
  [input]
  (->> (str/split-lines input)
       (map parse-reaction)
       vec))

(def reactions (parse-reactions (first (nth (seq test-inputs) 1))))

(defn n-reactions
  [n-needed n-products]
  (long (Math/ceil (/ n-needed n-products))))

(defn mult-inputs
  [n inputs]
  (->> inputs
       (map #(update % 0 * n))))

(let [inventory #{{:product "ORE"
                   :quantity 1
                   :inputs #{[1 "ORE"]}
                   :byproducts #{}}}
      elems #(set (map :product inventory))]
  elems)

(defn filter-on
  [key-fn pred coll]
  (filter #(pred (key-fn %)) coll))

(defn find-reaction-by-product
  [reactions e]
  (first (filter #(= e (:product %)) reactions)))

(defn cheapest-inputs
  [reactions element quantity]
  (if (= "ORE" element)
    {:product "ORE"
     :quantity quantity}
    (let [reaction (find-reaction-by-product reactions element)]
      (set (map #(-> (cheapest-inputs reactions
                                     (:product %)
                                     (:quantity %))
                     (update :quantity * (n-reactions quantity
                                                      (:quantity reaction))))
                (:inputs reaction))))))

(comment
  (mult-inputs 3 (-> (get reactions "FUEL") :inputs))
  (find-reaction-by-product reactions "FUEL")
  (cheapest-inputs reactions "FUEL" 1))

(comment
  (reactants reactions ["FUEL" 10])
  (reactions "FUEL")

  {:needed {"FUEL" 1}
   :extra {}}
  {:adding ({"A" 7
             "E" 1})})

(comment (parse-reactions (slurp "resources/input-14.txt"))
         (parse-reactions (first test-inputs))
         (->> (str/split-lines (slurp "resources/input-14.txt"))
              (map parse-reaction)
              (map first)
              count)

  {"B" [5 3 "A"]
   "A" [5 5 "ORE"]
   "C" [1 1 "A" 1 "B"]})
