(ns aoc-2020
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.test :refer [deftest is are]]
            [clojure.math.combinatorics :as c]))


(defn read-resource [name]
  (slurp (io/resource name)))

(defn file->vec [name]
  (s/split-lines (read-resource name)))

(defn file->numvec [name]
  (map #(Integer/parseInt %) (file->vec name)))


; --------------------------------------------------------
; Day 1 Problem 1
(def d1-input (file->numvec "2020-d1-p1.txt"))

(defn find-2020-num [lines]
  (let [remain (set (map #(- 2020 %) lines))]
    (first (drop-while nil? (map remain lines)))))

(defn find-2020-prod [lines]
  (let [num (find-2020-num lines)]
    (if (nil? num)
      nil
      (* num (- 2020 num)))))

(deftest test-find-2020-num
  (is (= (find-2020-num [1010]) 1010))
  (is (= (find-2020-num [1 2 3 4]) nil))
  (is (= (find-2020-num [1 2 3 4 5 1900 120]) 1900)))

(deftest expense-product
  (is (= (find-2020-prod d1-input) 1010299)))

; Day 1 Problem 2

(defn find-2020-trip [lines]
  (first
    (drop-while
      #(not (= (apply + %) 2020))
      (c/combinations lines 3))))

(deftest test-2020-trips
  (is (= (find-2020-trip d1-input) '(1488 60 472)))
  (is (= (apply * (find-2020-trip d1-input)) 42140160)))


; --------------------------------------------------------
; Day 2 Part 1
(def day2-input
  (->> "2020-d2.txt"
       file->vec
       parts))

(defn parts [lines]
  (map #(s/split % #" ") lines))

(defn check-sled-pass [line]
  (let [[min max] (map #(Integer/parseInt %) (s/split (first line) #"-"))
        letter (first (second line))
        pwd (last line)
        let-count (count (re-seq (re-pattern (str letter)) pwd))]
    ;(println min max)
    ;(println letter)
    ;(println pwd)
    ;(println let-count)
    (and (>= let-count min) (<= let-count max))))

(defn count-valid-sled []
  (->> day2-input
       (map check-sled-pass)
       (filter true?)
       count))

(deftest test-parts
  (is (= (first (parts ["1-3 a: abcde"])) ["1-3" "a:" "abcde"]))
  (is (= (check-sled-pass ["1-3" "a:" "abcde"]) true))
  (is (= (check-sled-pass ["1-3" "b:" "cdefg"]) false)))

(deftest check-sled-pass-report
  (is (= (count-valid-sled) 396)))

; Day 2 Part 2
(defn check-letter [letter pos pwd]
  (= letter (nth pwd pos)))

(defn check-toboggan-pass [line]
  (let [[p1 p2] (map #(dec (Integer/parseInt %)) (s/split (first line) #"-"))
        letter (first (second line))
        pwd (last line)
        p1? (check-letter letter p1 pwd)
        p2? (check-letter letter p2 pwd)]
    (or (and p1? (not p2?)) (and p2? (not p1?)))))

(defn count-toboggan-valid []
  (->> day2-input
       (map check-toboggan-pass)
       (filter true?)
       count))

(deftest toboggen-pass-test
  (is (= (check-toboggan-pass ["1-3" "a:" "abcde"]) true))
  (is (= (count-toboggan-valid) 428)))




