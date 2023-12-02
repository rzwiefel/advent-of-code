(ns y2023.d1
  (:require 
   [util]
   [clojure.string :as string]))

(def input (util/file->vec "2023-d1.txt"))

(def sample-input (string/split-lines "two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen"))

(def l1 "5ffour295")

(defn filter-digits
  [input]
  (filter #(Character/isDigit %) input))

(defn p1
  [input]
  (->> input
       (map filter-digits)
       (map (juxt first last))
       (map string/join)
       (map util/parse-long)
       (reduce +)))

(defn word->num
  [line]
  (string/replace
   line
   #"one|two|three|four|five|six|seven|eight|nine"
   {"one" "1"
    "two" "2"
    "three" "3"
    "four" "4"
    "five" "5"
    "six" "6"
    "seven" "7"
    "eight" "8"
    "nine" "9"}))

(defn find-nums
  [line]
  (map second 
       (re-seq #"(?=(\d|one|two|three|four|five|six|seven|eight|nine))" line)))

(defn p2
  [input]
  (->> input
       (map find-nums)
       (map string/join)
       (map word->num)
       p1))

(comment
  (p1 input)
  
  (p2 sample-input)
  (word->num "ptwonefive2threekfrtvnbmplpsevenseven")
  (find-nums "ptwonefive2threekfrtvnbmplpsevenseven")

  (map vector
   (->> input
         (map word->num)
         (map filter-digits)
         (map (juxt first last)))
   input)

  (map word->num sample-input)
  (p2 input)
  )