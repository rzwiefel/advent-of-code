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


; --------------------------------------------------------
; Day 2

(def d2-input (file->vec "2021-d2.txt"))
(def d2-test (s/split-lines "forward 5
down 5
forward 8
up 3
down 8
forward 2"))

(defn parse-substructions
  [input-vec]
  (for [line input-vec
        :let [[_ dir amount] (re-find #"(\w+)\s(\d)" line)]]
    [(keyword dir) (Integer/parseInt amount)]))

(defn calculate-sub-position
  [substructions]
  (reduce (fn [acc [dir amount]]
            (update acc dir + amount))
          {:forward 0 :down 0 :up 0}
          substructions))

(deftest d2p1
  (is (= 150
         (let [{:keys [forward down up]} (calculate-sub-position
                                          (parse-substructions d2-test))]
           (* forward (- down up)))))
  (is (= 1524750
         (let [{:keys [forward down up]} (calculate-sub-position
                                          (parse-substructions d2-input))]
           (* forward (- down up))))))

(defn calculate-sub-position-aimed
  [substructions]
  (reduce (fn [acc [dir amount]]
            (case dir
              :down    (update acc :aim + amount)
              :up      (update acc :aim - amount)
              :forward (-> acc
                           (update :forward + amount)
                           (update :depth + (* (:aim acc) amount)))))
          {:forward 0 :aim 0 :depth 0}
          substructions))

(deftest d2p1
  (is (= 900
         (let [{:keys [depth forward]} (calculate-sub-position-aimed
                                        (parse-substructions d2-test))]
           (* depth forward))))
  (is (= 1592426537
         (let [{:keys [depth forward]} (calculate-sub-position-aimed
                                        (parse-substructions d2-input))]
           (* depth forward)))))