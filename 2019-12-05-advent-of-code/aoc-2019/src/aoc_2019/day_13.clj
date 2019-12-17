(ns aoc-2019.day-13
  (:require [aoc-2019.day-09 :as intcode]
            [clojure.core.async :as a]
            [gnuplot.core :as g])
  (:import (clojure.lang PersistentQueue)))

(def memory (delay (intcode/parse-memory (slurp "resources/input-13.txt"))))

(def tile {0 :empty
           1 :wall
           2 :block
           3 :h-paddle
           4 :ball})

(defn draw-one
  [state [x y tile-id]]
  (if (= [x y] [-1 0])
    (assoc-in state [:score] tile-id)
    (assoc-in state [:screen [x y]] (tile tile-id))))

(def recent-states (atom (PersistentQueue/EMPTY)))

(defn add-state
  [q s]
  (let [q' (.cons q s)]
    (if (> 20 (.count q'))
      (.pop q')
      q')))

(defn draw-outputs
  [{outputs :outputs}]
  (reduce draw-one {} (partition 3 outputs)))

(defn solve-p1
  []
  (count
    (filter #{:block}
            (vals
              (:screen
                (draw-outputs
                  (intcode/run-until-fixed
                    (intcode/initial-state memory))))))))

(defn min-max
  [xs]
  [(apply min xs) (apply max xs)])

(defn bounds-2D
  "Find [[x-min x-max][y-min y-max]] for a map with keys of the form [x y]"
  [m]
  [(min-max (map first (keys m)))
   (min-max (map second (keys m)))])

(comment
  (bounds-2D (:screen test-state)))

(defn print-2D!
  [render-cell grid-contents]
  (let [[[x-min x-max] [y-min y-max]] (bounds-2D grid-contents)]
    (doseq [y (range y-min (inc y-max))]
      (println
        (apply str
               (map
                 (fn [x] (render-cell (grid-contents [x y])))
                 (range x-min (inc x-max))))))))

(def game-scores (atom {242 44
                        241 118
                        236 269
                        235 307
                        233 439
                        232 512
                        231 575
                        228 643
                        227 653
                        226 684
                        225 743
                        224 833
                        223 856
                        221 952
                        220 1049
                        219 1067}))

(defn render-screen!
  [{:keys [screen score]}]
  (let [n-blocks (count (filter #{:block} (vals screen)))]
    (swap! game-scores assoc n-blocks score)
    (dotimes [_ 0]
      (println))
    (print-2D!
      {:block    "O"
       :h-paddle "="
       :ball     "*"
       :empty    " "
       :wall     "X"}
      screen)
    (println "Score:" (or score 0) "Blocks:" n-blocks)))

(comment
  (def test-state
    (draw-outputs (intcode/run-until-fixed
                    (intcode/initial-state memory)))))

(comment
  (render-screen! test-state))

(defn game-loop!
  [initial-state joystick-chan]
  (a/go-loop [state initial-state]
    (let [ready-for-input (intcode/run-until-fixed state)]
      (swap! recent-states add-state ready-for-input)
      (render-screen! (draw-outputs ready-for-input))
      (when-let [joystick (a/<! joystick-chan)]
        (recur (assoc ready-for-input :inputs [joystick]))))))

(defn insert-quarters
  [memory]
  (assoc-in (intcode/initial-state memory) [:memory 0] 2))

(defn play-game!
  []
  (let [joystick-chan (a/chan)]
    (game-loop!
      (insert-quarters memory)
      joystick-chan)
    joystick-chan))

(def left -1)

(def center 0)

(def right 1)

(defn quit [chan] (a/close! chan))

(defn filter-on
  [key-fn pred coll]
  (filter (comp pred key-fn) coll))

(defn ball-position
  [display-state]
  (ffirst (filter-on second #{:ball} display-state)))

(defn paddle-position
  [display-state]
  (ffirst (filter-on second #{:h-paddle} display-state)))

(defn signum
  [x]
  (let [x' (or x nil)]
    (cond
      (pos? x') 1
      (neg? x') -1
      :else 0)))

(defn bot-joystick
  [{screen :screen} {screen' :screen}]
  (let [ball (ball-position screen')
        v-ball (map -
                    (ball-position screen')
                    (or (ball-position screen)
                        (ball-position screen')))
        ball'' (mapv + ball v-ball v-ball)
        paddle (paddle-position screen')
        ideal-movement (map - ball'' paddle)
        _ (println ball v-ball ball'' paddle ideal-movement)
        joystick-direction (signum (first ideal-movement))]
    joystick-direction))

(defn project-to-above-paddle
  [ball v-ball paddle]
  (let [steps-from-collision (dec (- (paddle 1) (ball 1)))]
    (apply map + ball (repeat steps-from-collision v-ball))))

(comment
  (project-to-above-paddle
    [10 10] [1 1] [15 20]))

;; (paddle-y - 1)  x + vx*n = x'  y + vy*n = paddle-y - 1  n = (paddle-y - 1 - y)/vy
;; x' = x + vx * (paddle-y - 1 - y) / vy
;; ball-n = ball + n * v-ball

(defn bot-joystick
  [{screen :screen} {screen' :screen}]
  (let [ball' (ball-position screen')
        [vx vy] (map - (ball-position screen')
                     (or (ball-position screen)
                         (ball-position screen')))
        paddle (paddle-position screen')
        ideal-position (if (neg? vy)
                         ;; Ball rising. Try to get under it.
                         ball'
                         ;; Ball falling. Try to get to where it's going
                         (project-to-above-paddle ball' [vx vy] paddle))]
    (signum (first (map - ideal-position paddle)))))

(defn step-ai-game
  [[state state']]
  ;(println (count (:outputs state)) (count (:outputs state')))
  ;(println (count (draw-outputs state')))
  (let [display-state (draw-outputs state)
        display-state' (draw-outputs state')
        joystick (bot-joystick display-state display-state')]
    [state' (intcode/run-until-fixed (assoc state' :inputs [joystick]))]))

(defn stopped?
  [s s']
  (= s s'))

(defn has-blocks?
  [{screen :screen}]
  (some #{:block} (vals screen)))

(defn not-done?
  [[s s']]
  (and (has-blocks? (draw-outputs s'))
       (not (stopped? (draw-outputs s) (draw-outputs s')))))

(defn auto-play
  []
  (let [computer-state (intcode/run-until-fixed (insert-quarters memory))]
    (doseq [[_ state] (map first (partition 1 100 (iterate step-ai-game [nil computer-state])))]
      ;(Thread/sleep 500)
      (render-screen! (draw-outputs state)))))

(comment
  (bot-joystick {:screen
                 {[10 19] :ball
                  [12 20] :h-paddle}}
                {:screen
                 {[11 19] :ball
                  [12 20] :h-paddle}})
  (bot-joystick nil
                {:screen
                 {[11 11] :ball
                  [13 20] :h-paddle}}))


(defn gcd
  ([x y]
   (/ y (denominator (/ x y))))
  ([x y & more]
   (reduce gcd x (cons y more))))

(defn solve-p2 [] 11641)

(comment
  (gcd 6 10)
  (gcd 2 5)
  (gcd 24 100 250))

(comment
  (def joystick-chan (play-game!))
  (a/put! joystick-chan left)
  (a/put! joystick-chan center)
  (a/put! joystick-chan right)
  (quit joystick-chan)
  (let [scores @game-scores]
    (g/raw-plot!
      [[:set :title "Scores"]
       [:plot (g/range 0 250)
        (g/list ["-" :title "Scores" :with :lines])]]
      [(sort scores)])))

