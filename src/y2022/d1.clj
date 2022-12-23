(ns y2022.d1
  (:require
   [clojure.string :as s]
   [util]))


(def input (util/read-resource "2022-d1.txt"))

(as-> input i 
    (s/split i #"\n\n")
    (map s/split-lines i)
    (map #(map util/parse-long %) i)
    (map #(apply + %) i)
    (sort i)
    (reverse i)
    (take 3 i))

(apply + '(68467 68143 66810))



