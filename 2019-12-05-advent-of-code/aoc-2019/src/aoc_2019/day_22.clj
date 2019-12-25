(ns aoc-2019.day-22
  (:require [clojure.string :as str]
            [clojure.math.numeric-tower :refer [expt]]))

(defn relative-index
  [n size]
  (mod (+ n size) size))

(defn new-stack
  [coll]
  (vec (reverse coll)))

(defn cut
  [n coll]
  (->> (split-at (relative-index n (count coll))
                 coll)
       reverse
       flatten
       (into [])))

(defn parse-new-stack
  [s]
  (when (re-find #"deal into new stack" s)
    new-stack))

(comment
  ((parse-new-stack "deal into new stack") (range 7)))

(defn parse-cut
  [s]
  (when-let [[_ n] (re-find #"cut (.+)" s)]
    (partial cut (Long/parseLong n))))

(comment
  ((parse-cut "cut -2") (range 5)))

(comment
  ((parse-deal-with-increment "deal with increment 3") (range 5)))

(defn deal-with-increment
  [n coll]
  (->> (map vector
            (flatten (partition 1 n (cycle (range (count coll)))))
            coll)
       sort
       (mapv second)))

(defn parse-deal-with-increment
  [s]
  (when-let [[_ n] (re-find #"deal with increment (.+)" s)]
    (partial deal-with-increment (Long/parseLong n))))

(defn parse-deal
  [s]
  (some identity
        ((juxt parse-new-stack
               parse-cut
               parse-deal-with-increment) s)))

(comment
  (deal-with-increment 3 (vec (range 7)))
  [0 1 2 3 4 5 6]
  [0 5 3 1 6 4 2]

  (->> (map vector
            (flatten (partition 1 3 (cycle (range 7))))
            (range 7))
       sort
       (map second)))

(comment
  (split-at 2 [1 2 3 4 5])
  (cut 2 (range 10)))

(def final-deck (reduce (fn [deck f]
                          (f deck))
                        (vec (range 10007))
                        (map parse-deal (str/split-lines
                                          (slurp "resources/input-22.txt")))))

(defn solve-p1
  []
  (some identity (map-indexed (fn [n x]
                                (when (= x 2019)
                                  n)) final-deck)))

(def deck-size 119315717514047)

(def shuffle-n 101741582076661)

(defn position-before-new-stack
  [size pos]
  (- size pos 1))

(defn position-before-cut
  [size n pos]
  (relative-index (+ pos n) size))

(comment
  (position-before-cut 10 3 9)
  (cut 3 (range 10))

  (deal-with-increment 12 (range 31))

  (position-before-new-stack deck-size 2020))

(defn position-after-cut
  [size n pos]
  (relative-index (- pos n) size))

(defn position-after-reverse
  [size pos]
  (- size pos 1))

(defn position-after-dwi
  [size n pos]
  (mod (* n pos) size))

(comment
  (- 68 (rem deck-size 68))
  (- 68 (rem (* 2 deck-size) 68))
  (- 68 (rem (* 3 deck-size) 68))
  (rem (* 2 45) 68)
  (rem (* 3 45) 68)
  (get (zipmap (map #(rem (* % 45) 68) (range 68)) (range 68))
       48)

  (quot 2020 68)
  (rem 2020 68)
  (+ (* 60 68) 29)
  (position-after-dwi deck-size 68 2749)
  (position-after-dwi deck-size 68 4109)
  (quot 186932 68))

(defn interleaved-offset*
  [size n v]
  (let [y0 (- n (mod size n))]
    (get (zipmap (map #(mod (* % y0) n) (range n)) (range n)) v)))

(def interleaved-offset (memoize interleaved-offset*))

(defn pos-before-dwi
  [size n pos]
  (if (zero? pos)
    0
    (let [v (mod pos n)
          y (interleaved-offset size n v)
          a (+ (quot (* y size) n) (inc (quot (dec pos) n)))]
      a)))

(defn parse-line
  [s]
  (-> (or (re-find #"(deal into new stack)" s)
          (re-find #"(deal with increment) (.+)" s)
          (re-find #"(cut) (.+)" s))
      rest
      vec
      (update 1 #(and % (Long/parseLong %)))))

(comment
  (parse-line "cut 123")
  (parse-line "deal with increment -123")
  (parse-line "deal into new stack"))

(def shuffles
  (map parse-line (str/split-lines (slurp "resources/input-22.txt"))))

(def scale 2753313055858169530885444089949679824227348332738405990400000N)

(def offset 197703788739329533688101990240838234672990199269131600232741510N)

(comment
  (reduce
    (fn [{:keys [offset scale]}
         [shuffle n]]
      (case shuffle
        "deal into new stack" {:offset (-' 0 offset 1)
                               :scale  (-' scale)}
        "cut" {:offset (-' offset n)
               :scale  scale}
        "deal with increment" {:offset (mod (*' offset n) deck-size)
                               :scale  (mod (*' scale n) deck-size)}))
    {:offset 0
     :scale  1}
    (apply concat (repeat 1E5 shuffles))))

(defn inverse-shuffle
  [size]
  {"deal into new stack" (fn [_ pos] (position-before-new-stack size pos))
   "cut" (partial position-before-cut size)
   "deal with increment" (partial pos-before-dwi size)})

(defn forward-shuffle
  [size]
  {"deal into new stack" (fn [_ pos] (position-after-reverse size pos))
   "cut" (partial position-after-cut size)
   "deal with increment" (partial position-after-dwi size)})

(defn reverse-deal
  [size pos shuffles]
  (->> shuffles
       (map (fn [[shuffle n]]
              (partial ((inverse-shuffle size) shuffle) n)))
       reverse
       (reduce (fn [pos f]
                 (f pos))
               pos)))

(defn deal
  [size pos shuffles]
  (->> shuffles
       (map (fn [[shuffle n]]
              (partial ((forward-shuffle size) shuffle) n)))
       (reduce (fn [pos f]
                 (f pos))
               pos)))

(defn offset-and-scale
  [shuffles]
  (reduce
    (fn [{:keys [offset scale]}
         [shuffle n]]
      (case shuffle
        "deal into new stack" {:offset (-' 0 offset 1)
                               :scale  (-' scale)}
        "cut" {:offset (-' offset n)
               :scale  scale}
        "deal with increment" {:offset (*' offset n)
                               :scale  (*' scale n)}))
    {:offset 0
     :scale  1}
    (take 100 (cycle shuffles))))

(defn calculate-forward-position
  [{:keys [scale offset]} size pos]
  (-> pos
      (* scale)
      (+ offset)
      (mod size)))

(defn mod-expt
  [base pow m]
  (loop [result 1
         base (mod base m)
         e pow]
    (if (pos? e)
      (recur (if (= 1 (mod e 2))
               (mod (* result base) m)
               result)
             (mod (* base base) m)
             (bit-shift-right e 1))
      result)))

(defn compose-deal
  [m a b]
  {:scale (-> (* (:scale a) (:scale b))
              (mod m))
   :offset (-> (* (:offset a) (:scale b))
               (+ (:offset b))
               (mod m))})

(defn n-deals
  [deck-size n {:keys [scale offset]}]
  (nth (iterate (partial compose-deal
                         deck-size
                         {:scale scale :offset offset})
                {:scale scale :offset offset})
       (dec n)))

(defn map-keys
  [f m]
  (reduce-kv (fn [m k v]
               (assoc m k (f v)))
             {}
             m))

(comment
  (map-keys #(mod % deck-size) (offset-and-scale shuffles))
  (map-keys #(mod % deck-size) (offset-and-scale (apply concat (repeat 4 shuffles))))
  (n-deals deck-size 4 (offset-and-scale shuffles))
  (compose-deal deck-size
                {:offset 83085969022373N, :scale 2904709793044N}
                {:offset 20489917522142N, :scale 17734190228660N}))

(def deals
  {1 {:scale scale :offset offset}
   1E5 {:scale 108618155525806N, :offset 74983495570957N}
   1E10 {:scale 56803806224935N, :offset 16487730098526N}
   76661 {:scale 22507082576N, :offset 70832731650325N}
   15820E5 {:scale 96328478868135N, :offset 43315831151340N}
   10174E10 {:scale 73470038416937N, :offset 864421165858N}
   shuffle-n {:scale 84304260892534N, :offset 13645212627296N}})

(comment
  (- shuffle-n (+ 76661 15820E5 10174E10))
  (n-deals deck-size 1E5 {:scale scale :offset offset})
  (n-deals deck-size 1E5 (n-deals deck-size 1E5 {:scale scale :offset offset}))
  (n-deals deck-size 76661 {:scale scale :offset offset})
  (n-deals deck-size 15820 (n-deals deck-size 1E5 {:scale scale :offset offset}))
  (n-deals deck-size 10174 (n-deals deck-size 1E5 (n-deals deck-size 1E5 {:scale scale :offset offset})))
  (compose-deal deck-size {:scale scale :offset offset} {:scale scale :offset offset})
  (reduce (partial compose-deal deck-size)
          [(n-deals deck-size 76661 {:scale scale :offset offset})
           (n-deals deck-size 15820 (n-deals deck-size 1E5 {:scale scale :offset offset}))
           (n-deals deck-size 10174 (n-deals deck-size 1E5 (n-deals deck-size 1E5 {:scale scale :offset offset})))]))


(comment
  (->> (compose-deal deck-size (deals 76661) (deals 15820E5))
       (compose-deal deck-size (deals 10174E10)))
  (n-deals deck-size 10 {:scale scale :offset offset}))

(defn modular-inverse
  [m n]
  (mod-expt n (- m 2) m))

(defn original-position
  [pos' {:keys [scale offset]} deck-size]
  (-> pos'
      (- offset)
      (* (modular-inverse deck-size scale))
      (mod deck-size)))

(comment
  (deal 10007 2019 shuffles)
  (mod (+ (* scale 2019) offset) 10007)
  (mod (* (- 5169 offset) (modular-inverse 10007 scale)) 10007))

(defn solve-p2
  []
  (original-position 2020 (deals shuffle-n) deck-size))

(comment
  (Long/toBinaryString deck-size)
  (deal 10007 2019 (take 100 shuffles))
  (offset-and-scale shuffles)
  (offset-and-scale (apply concat (repeat 1 shuffles)))
  (calculate-forward-position
    {:offset offset
     :scale scale}
    10007
    2019)
  (mod-expt scale shuffle-n deck-size)

  (deal 10007 2019 (take 1 shuffles))
  (deal 10007 2019 shuffles)
  (reverse-deal 10007 5169 shuffles)

  (mod (- (* 2019 scale) offset) 10007)
  (-> 2019
      (- offset)
      (* scale)
      -
      (mod 10007))

  (mod (expt (mod scale deck-size) (- deck-size 2)) deck-size)


  (time (->> (iterate reverse-deal 2020)
             (take 1000000)
             (map vector (range))
             (filter #(= 2020 (second %)))
             vec))

  (time (->> (iterate reverse-deal 2020)
             (take 1000000)
             (map vector (range))
             frequencies
             (filter #(< 1 (second %)))))

  (time (->> (iterate reverse-deal 2020)
             (take 1000)
             (map vector (range))))

  (time (->> (iterate reverse-deal 2020)
             (take 1000)
             (partition 2 1)
             (map #(apply - %))))

  (- (reverse-deal 2020) 2020)
  (reverse-deal deck-size 2021))

(comment
  (- (- (reverse-deal 2020) 2020) (- (reverse-deal 2019) 2019))
  (->> (map reverse-deal (range 100))
       (partition 2 1)
       (map #(apply - %))
       (map #(mod % deck-size)))
  (map #(apply - %) (partition 2 1 (map (comp reverse-deal) (range 10))))

  (- (reverse-deal 2020) 2020)
  (- (reverse-deal 2019) 2019))

(comment
  (do (doseq [x (range 1000)]
        (let [p (pos-before-dwi deck-size 68 x)]
          (println x (position-after-dwi deck-size 68 p)
                   (for [y (range (dec p) (inc (inc p)))]
                     (position-after-dwi deck-size 68 y)))))
      nil)


  (frequencies (for [x (range 100000)]
                 (let [p (pos-before-dwi deck-size 68 x)]
                   (= x (position-after-dwi deck-size 68 p)))))

  (pos-before-dwi deck-size 68 0)
  (rem 0 68)
  (- 68 (rem deck-size 68))
  (get (zipmap (map #(rem (* % 45) 68) (range 68)) (range 68)) 0)
  (+ (quot 0 68) (quot 0 68))

  (+ (quot (* 60 deck-size) 68))
  (position-after-dwi deck-size 68 105278574277130)

  (quot deck-size 68)
  (position-after-dwi deck-size 68 1754642904619)
  (* 68 1754642904618)

  (quot deck-size 68)
  (quot (* 2 deck-size) 68)
  (quot (* 3 deck-size) 68))
