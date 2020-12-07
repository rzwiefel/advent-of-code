(ns aoc-2020
  (:require [clojure.java.io :as io]
            [clojure.set :as cset]
            [clojure.string :as s]
            [clojure.spec.alpha :as spec]
            [clojure.test :refer [deftest is are]]
            [clojure.math.combinatorics :as c]
            [clojure.pprint :refer [pprint]]
            [portal.api :as p]))


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
(defn parts [lines]
  (map #(s/split % #" ") lines))

(def day2-input
  (->> "2020-d2.txt"
       file->vec
       parts))

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


; --------------------------------------------------------
; Day 3 Part 1 and 2

(def d3-input
  (->> "2020-d3.txt"
       file->vec))

(def d3-test-input
  (->> "2020-d3-test.txt"
       file->vec))

(defn traverse [treemap col drow dcol trees-hit open-spots]
  (let [cur-row (first treemap)]
    (if (nil? cur-row)
      [trees-hit open-spots]
      (let [cur-spot (nth cur-row col)
            new-col (mod (+ col dcol) (count cur-row))]
        (if (= \# cur-spot)
          (recur (nthrest treemap drow) new-col drow dcol (inc trees-hit) open-spots)
          (recur (nthrest treemap drow) new-col drow dcol trees-hit (inc open-spots)))))))

(deftest test-traverse
  (is (= [7 4] (traverse d3-test-input 0 1 3 0 0)))
  (is (= [84 239] (traverse d3-input 0 1 1 0 0)))
  (is (= 5140884672
         (->> (for [[drow dcol] [[1 1] [1 3] [1 5] [1 7] [2 1]]
                    :let [args [d3-input 0 drow dcol 0 0]]]
                (apply traverse args))
              (map first)
              (reduce *)))))

; --------------------------------------------------------
; Day 4 Part 1 and 2

(def d4-input
  (s/split (->> "2020-d4.txt"
                io/resource
                slurp)
           #"\n\n"))

(def req-keys #{"byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"})

(defn break [line]
  (let [parts (s/split line #"\n|\s")]
    (apply hash-map (flatten (map #(s/split % #":") parts)))))

(defn valid-passport [data]
  (let [imp (keep req-keys (keys data))]
    (empty? (remove (set imp) req-keys))))

(defn valid-num-range [key lower upper]
  (fn [data]
    (let [name (get data key)]
      (if (nil? name)
        false
        (let [namenum (Integer/parseInt name)]
          (<= lower namenum upper))))))

(defn valid-hgt [data]
  (let [hgt (get data "hgt")]
    (if (nil? hgt)
      false
      (let [[_ numstr unit] (re-matches #"(\d+)(\w+)" hgt)
            num (Integer/parseInt numstr)]
        (if (= "cm" unit)
          (<= 150 num 193)
          (<= 59 num 76))))))


(defn re-validater [key re]
  (fn [data]
    (let [val (get data key)]
      (if (nil? val)
        false
        (some? (re-matches re val))))))

(defn check-all-passport [data]
  ((every-pred
     valid-passport
     (valid-num-range "byr" 1920 2002)
     (valid-num-range "eyr" 2020 2030)
     (valid-num-range "iyr" 2010 2020)
     (re-validater "hcl" #"#[0-9a-f]{6}")
     (re-validater "pid" #"[0-9]{9}")
     (re-validater "ecl" #"amb|blu|brn|gry|grn|hzl|oth")
     valid-hgt)

   data))

(deftest checksplit
  (is (= 233 (->> d4-input
                  (map break)
                  (map valid-passport)
                  (filter true?)
                  count))))

(deftest checkpassportisvalid
  (is (= true (check-all-passport {"hgt" "67in",
                                   "pid" "361065637",
                                   "byr" "2000",
                                   "eyr" "2027",
                                   "iyr" "2020",
                                   "ecl" "gry",
                                   "hcl" "#623a2f"})))
  (is (= 111 (->> d4-input
                  (map break)
                  (map check-all-passport)
                  (filter true?)
                  count))))

; --------------------------------------------------------
; Day 5 Part 1 and 2

(def d5-input
  (->> "2020-d5.txt"
       file->vec))

(defn binarize [line]
  (-> line
      (s/replace "B" "1")
      (s/replace "F" "0")
      (s/replace "L" "0")
      (s/replace "R" "1")))

(def d5-sorted-input (->> d5-input
                          (map binarize)
                          (map #(Integer/parseInt % 2))
                          sort))

(deftest test-binarize
  (is (= "1110100101" (binarize "BBBFBFFRLR"))))

(deftest test-highest-seat
  (is (= 994 (last d5-sorted-input))))

(deftest find-missing-seat
  (let [full-list (range
                    (apply min d5-sorted-input)
                    (inc (apply max d5-sorted-input)))
        missing (remove (set d5-sorted-input) full-list)]
    (is (= '(741) missing))))


; --------------------------------------------------------
; Day 6 Part 1 and 2

(def d6-input
  (s/split (->> "2020-d6.txt"
                io/resource
                slurp)
           #"\n\n"))

(defn d6-p1 [group]
  (let [letterset (set (s/replace group "\n" ""))]
    (count letterset)))

(deftest test-d6
  (is (= 6947 (reduce + (map d6-p1 d6-input)))))

(defn d6-separate-group [line]
  (s/split line #"\n"))

(deftest test-separate
  (is (= '(#{\a \b} #{\a \c} (d6-separate-group "ab\nac"))))
  (is (= '(#{\a \b} #{\a \c} (map set (d6-separate-group "ab\nac")))))
  (is (= '(#{\a} (cset/intersection
                   (map set
                        (d6-separate-group "ab\nac")))))))

(defn d6-p2 [line]
  (apply cset/intersection (->> line
                                d6-separate-group
                                (map set))))

(deftest test-d6-p2
  (is (= #{\d \f \r \y} (d6-p2 "yfdr\nyfrd\nroyfd\ndryf"))))

(deftest d6-pt2-answer
  (is (= 3398 (->> d6-input
                   (map d6-p2)
                   (map count)
                   (reduce +)))))

; --------------------------------------------------------
; Day 7 Part 1 and 2
(defn d7-split [line]
  (let [[bag inside-bags] (map s/trim (s/split line #"bags contain"))]
    (if (= inside-bags "no other bags.")
      (list bag '())
      (list bag (as-> inside-bags v
                      (s/trim v)
                      (s/split v #",\s?")
                      (map #(s/replace % #"\s?bags?\.?" "") v)
                      (map #(re-matches #"(\d+)\s(.+)" %) v)
                      (map rest v)
                      (map #(let [[num color] %] (list (Integer/parseInt num) color)) v))))))

(def d7-input
  (->> "2020-d7.txt"
       file->vec
       (map d7-split)
       (reduce #(conj %1 (vec %2)) {})))

(def d7-input-vec
  (vec d7-input))

(def d7-test
  (->> "light red bags contain 1 bright white bag, 2 muted yellow bags.\ndark orange bags contain 3 bright white bags, 4 muted yellow bags.\nbright white bags contain 1 shiny gold bag.\nmuted yellow bags contain 2 shiny gold bags, 9 faded blue bags.\nshiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.\ndark olive bags contain 3 faded blue bags, 4 dotted black bags.\nvibrant plum bags contain 5 faded blue bags, 6 dotted black bags.\nfaded blue bags contain no other bags.\ndotted black bags contain no other bags."
       s/split-lines
       (map d7-split)
       (reduce #(conj %1 (vec %2)) {})))


(def d7-test2
  (->> "shiny gold bags contain 2 dark red bags.\ndark red bags contain 2 dark orange bags.\ndark orange bags contain 2 dark yellow bags.\ndark yellow bags contain 2 dark green bags.\ndark green bags contain 2 dark blue bags.\ndark blue bags contain 2 dark violet bags.\ndark violet bags contain no other bags."
       s/split-lines
       (map d7-split)
       (reduce #(conj %1 (vec %2)) {})))


(defn can-hold [color outer-bag]
  (let [[outer holdings] outer-bag]
    (if ((set (map second holdings)) color) true false)))

(defn find-holders [color bags]
  (for [bag bags
        :when (can-hold color bag)]
    bag))

(defn find-all-holders [colors last-size]
  (let [newset (into colors
                     (set
                       (flatten
                         (map #(map first (find-holders % d7-input-vec)) colors))))]
    (if (= last-size (count newset))
      newset
      (find-all-holders newset (count newset)))))

(deftest find-any-shiny-gold-holders
  (is (= 300 (count (disj (find-all-holders #{"shiny gold"} 0) "shiny gold")))))

(defn count-innerbags [input color]
  (let [innerbags (get input color)]
    (if (empty? innerbags)
      0
      (apply + (for [[in-num in-col] innerbags]
                 (+ in-num (* in-num (count-innerbags input in-col))))))))


(deftest count-shiny-gold-holdings
  (is (= 32 (count-innerbags d7-test "shiny gold")))
  (is (= 126 (count-innerbags d7-test2 "shiny gold")))
  (is (= 8030 (count-innerbags d7-input "shiny gold"))))



