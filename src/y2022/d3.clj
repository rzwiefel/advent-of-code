(ns y2022.d3
  (:require
   [clojure.string :as s]
   [clojure.set :as set]
   [util]))

(def sample-input "vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw")

(def input (util/read-resource "2022-d3.txt"))


(def priorities
  (into {}
        (concat
         (map vector
              (map char (range 97 123))
              (range 1 27))
         (map vector
              (map char (range 65 91))
              (range 27 53)))))

(->> input
     (s/split-lines)
     (map #(partition (/ (count %) 2) %))
     (map (fn [x] (map set x)))
     (map #(apply set/intersection %))
     (map first)
     (map priorities)
     (reduce +))

(->> input
     (s/split-lines)
     (partition 3)
     (map #(map set %))
     (map #(apply set/intersection %))
     (map first)
     (map priorities)
     (reduce +)
)