(ns aoc-2021
  (:require
   [clojure.java.io :as io]
   [clojure.string :as s]
   [clojure.test :refer [deftest is are]]
   [clojure.math.combinatorics :as c]
   [clojure.pprint :refer [pprint]]
   [portal.api :as p]))

#_(:require
   [clojure.java.io :as io]
   [clojure.set :as cset]
   [clojure.string :as s]
   [clojure.spec.alpha :as spec]
   [clojure.test :refer [deftest is are]]
   [clojure.math.combinatorics :as c]
   [clojure.pprint :refer [pprint]]
   [portal.api :as p])

(defn read-resource
  [name]
  (slurp (io/resource name)))

(defn file->vec
  [name]
  (s/split-lines (read-resource name)))

(defn file->numvec
  [name]
  (map #(Long/parseLong %) (file->vec name)))


; --------------------------------------------------------
; Day 1 Problem 1

(def d1-input (file->numvec "2021-d1.txt"))
#_(def d1-input
    [199
     200
     208
     210
     200
     207
     240
     269
     260
     263])

(defn calc-deltas
  [input]
  (->> input
       (partition 2 1)
       (apply map vector)
       (concat [-])
       (apply map)
       (map #(* -1 %))
       (filter pos?)
       (count)))

(deftest d1p1 (is (= 1759 (calc-deltas d1-input))))

;; p2

(defn calc-3-window
  [input]
  (->> input
       (partition 3 1)
       (apply map vector)
       (concat [+])
       (apply map)))

(deftest d1p2
  (is (= 1805 (calc-deltas (calc-3-window d1-input)))))