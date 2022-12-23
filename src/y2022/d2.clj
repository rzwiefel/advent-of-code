(ns y2022.d2
  (:require
   [clojure.string :as s]
   [util]))

(def input (util/read-resource "2022-d2.txt"))

(defn winner
  [input]
  (case input
    [:A :X] 3
    [:A :Y] 6
    [:A :Z] 0
    [:B :X] 0
    [:B :Y] 3
    [:B :Z] 6
    [:C :X] 6
    [:C :Y] 0
    [:C :Z] 3))

(defn chosen-piece-score
  [[_ mine]]
  (mine {:X 1 :Y 2 :Z 3}))

;A for Rock, B for Paper, and C for Scissors.
;X for Rock, Y for Paper, and Z for Scissors
(def input-ex "A Y
B X
C Z")

(->> input
     (s/split-lines)
     (map #(s/split % #"\s"))
     (map #(map keyword %))
     (map (juxt winner chosen-piece-score))
     flatten
     (reduce +))