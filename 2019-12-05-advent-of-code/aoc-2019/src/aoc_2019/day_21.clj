(ns aoc-2019.day-21
  (:require [aoc-2019.day-09 :as intcode]
            [clojure.string :as str]))

(def memory (intcode/parse-memory (slurp "resources/input-21.txt")))

(defn output-str
  [outputs]
  (apply str (map #(char %) outputs)))

(defn instruction
  [s]
  (map #(long %) (str s \newline)))

(defn instruction-set
  [ss]
  (mapcat instruction ss))

(def programs
  {:eager-jump {:description "Jump as often as the landing space is ground"
                :instructions ["NOT D T"
                               "NOT T J"
                               "WALK"]}
   :patient-jump {:description "Don't jump until the edge of a hole"
                  :instructions ["NOT A J"
                                 "WALK"]}
   :stingy {:description "Don't bother jumping until there is something to clear and the landing is solid"
            :instructions ["NOT A T"
                           "NOT B J"
                           "OR T J"
                           "NOT C T"
                           "OR T J"
                           "AND D J"
                           "WALK"]}
   :stingy-run {:description "Don't bother jumping until there is something to clear and the landing is solid"
                :instructions ["NOT A T"
                               "NOT B J"
                               "OR T J"
                               "NOT C T"
                               "OR T J"
                               "AND D J"
                               "RUN"]}
   :lazy {:description "Jump only if next turn has no legal move"
          :instructions ["NOT A T"
                         "NOT B J"
                         "OR T J"
                         "NOT C T"
                         "OR T J"
                         "AND D J"
                         "NOT J T" ;; Store J in T
                         "NOT T T" ;; Store J in T
                         "AND A T" ;; We can delay jump 1 space
                         "AND E T" ;; We have a landing pad next space
                         "NOT T T"
                         "AND T J"
                         "RUN"]}
   :simple {:description "Jump if the landing is two spaces long or death is imminent"
            :instructions ["NOT D T"
                           "NOT T T"
                           "AND E T"
                           "NOT A J"
                           "OR T J"
                           "RUN"]}
   :foresight {:description ":simple jump OR jump if doing so gives us a legal move"
               :instructions ["NOT E T"
                              "NOT T T"
                              "NOT H J"
                              "NOT J J"
                              "OR J T"
                              "AND D T"
                              "NOT A J"
                              "OR T J"
                              "RUN"]}
   :foresight-2 {:description ":foresight plus don't jump unless there's something to clear"
                 :instructions ["NOT E T"
                                "NOT T T"
                                "NOT H J"
                                "NOT J J"
                                "OR J T"
                                "AND D T"
                                "NOT A J"
                                "OR T J"
                                "NOT B T"
                                "NOT T T"
                                "AND C T"
                                "NOT T T"
                                "AND T J"
                                "RUN"]}
   :foresight-3 {:description ":foresight plus don't jump unless there's something to clear (fixed)"
                 :instructions ["NOT E T"
                                "NOT T T"
                                "NOT H J"
                                "NOT J J"
                                "OR J T"
                                "AND D T"
                                "NOT A J"
                                "OR T J"
                                "NOT A T"
                                "NOT T T"
                                "AND B T"
                                "AND C T"
                                "NOT T T"
                                "AND T J"
                                "RUN"]}})

;; ((D AND E) OR (D AND H)) OR (NOT A)
;; ((D AND (E OR H)) OR (NOT A)) AND ((NOT B) OR (NOT C) OR (NOT A))
;; NOT ((NOT ((D AND (E OR H)) OR (NOT A))) OR (B AND C))

(defn jump?
  [situation]
  (let [[a b c d e f g h i] (map #{\#} (rest situation))]
    (and (or (and d e) (and d h) (not a)) (or (not b) (not c)))
    (and (or (and d e) (and d h) (not a)) (not (and b c)))))

(def pathological-cases
  ["###.#.##."
   "###.#.#.#...#"
   "###..##..#..##"])

(def test-cases
  {"^##.#..##." :jump

   "^#..##..#." :jump
   "^####..##." :walk})

(map jump? (keys test-cases))

(defn run-robot
  [program]
  (output-str (:outputs (intcode/run-until-fixed
                          (-> (intcode/initial-state memory)
                              (assoc :inputs (instruction-set
                                               program)))))))

(comment
  (run-robot (:instructions (:stingy programs)))
  (run-robot (:instructions (:lazy programs)))
  (run-robot (:instructions (:simple programs)))
  (run-robot (:instructions (:foresight programs)))
  (run-robot (:instructions (:foresight-2 programs)))
  (run-robot (:instructions (:foresight-3 programs))))
