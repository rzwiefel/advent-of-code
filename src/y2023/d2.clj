(ns y2023.d2
  (:require 
   [util]
   [clojure.string :as string]))

(def input (util/file->vec "2023-d2.txt"))

(def max-cubes {:red 12 :green 13 :blue 14})

(def sample-input (string/split-lines "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"))

(defn parse-set
  ; a set will look like ["3 blue" "7 green" "10 red"]
  [set]
  (let [[num color] (string/split set #"\s")]
    {(keyword color) (util/parse-long num)}))

;"Game 1: 3 blue, 7 green, 10 red; 4 green, 4 red; 1 green, 7 blue, 5 red; 8 blue, 10 red; 7 blue, 19 red, 1 green"
(defn parse-line 
  [line]
  (let [[_ game-num games] (re-matches #"Game (\d+): (.*)" line)
        game-num (Long/parseLong game-num)
        games (mapv #(apply merge (map parse-set (string/split % #",\s"))) (string/split games #";\s"))]
    [game-num games]))


(defn check-game-possible
; [1 [[[:blue 3] [:green 7] [:red 10]] [[:green 4] [:red 4]] [[:green 1] [:blue 7] [:red 5]]]]
  [[game-num sets]]
  [game-num 
   (every? true?
           (for [s sets]
             (reduce (fn [acc [color num]] (and acc (<= num (color max-cubes)))) true s)))])

(defn p1
  [input]
  (->> input
       (map parse-line)
       (map check-game-possible)
       (filter second)
       (map first)
       (reduce +)))

(defn check-min-cubes-possible [[_ sets]]
  (reduce (fn [acc set] (merge-with max acc set)) {} sets))

(defn p2
  [input]
  (->> input
       (map parse-line)
       (map check-min-cubes-possible)
       (map vals)
       (map #(apply * %))
       (reduce +)))

(comment
  (parse-set "3 blue")
  (parse-line (first input))
  (check-game-possible (parse-line (first input)))
  (p1 sample-input)
  (p1 input)
  ;; => 2162

  (check-min-cubes-possible (parse-line (first input)))
  (p2 input)
  ;; => 72513


  
  )