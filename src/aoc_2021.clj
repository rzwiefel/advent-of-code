(ns aoc-2021
  (:require
   [clojure.java.io :as io]
   [clojure.string :as s]
   [clojure.test :refer [deftest is are]]
   [clojure.math.combinatorics :as c]
   [clojure.pprint :refer [pprint]]
   [portal.api :as p]
   [clojure.set :as set]))


(comment
 (require '[portal.api :as p])
 (def portal
   (p/open {:portal.colors/theme :portal.colors/material-ui
            :launcher            :vs-code}))
 (add-tap #'p/submit)
 (tap> :ready-to-go)
)

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

(deftest d2p2
  (is (= 900
         (let [{:keys [depth forward]} (calculate-sub-position-aimed
                                        (parse-substructions d2-test))]
           (* depth forward))))
  (is (= 1592426537
         (let [{:keys [depth forward]} (calculate-sub-position-aimed
                                        (parse-substructions d2-input))]
           (* depth forward)))))

; --------------------------------------------------------
; Day 3

(def d3-input (file->vec "2021-d3.txt"))

(def d3-test
  (s/split-lines
   "00100\n11110\n10110\n10111\n10101\n01111\n00111\n11100\n10000\n11001\n00010\n01010"))

(defn common-bits
  [input comparerer]
  (->> input
       (apply map vector)
       (map frequencies)
       (map #(if (comparerer (get % \0 0) (get % \1 0)) 0 1))
       (apply str)))

(defn sub-power-consumption
  [input]
  (let [common-bin   (common-bits input >=)
        uncommon-bin (common-bits input <)
        common       (Integer/parseInt common-bin 2)
        uncommon     (Integer/parseInt uncommon-bin 2)]
    (* common uncommon)))

(deftest d3p1
  (is (= 198 (sub-power-consumption d3-test)))
  (is (= 3813416 (sub-power-consumption d3-input))))

(defn sub-c-or-2
  [ratings pos comparer]
  (if (next ratings)
    (let [filterer         (common-bits ratings comparer)
          filtered-ratings (filter #(= (nth % pos) (nth filterer pos)) ratings)]
      (sub-c-or-2 filtered-ratings
                  (inc pos)
                  comparer))
    (first ratings)))

(defn sub-life-support
  [input]
  (let [o2  (sub-c-or-2 input 0 >)
        co2 (sub-c-or-2 input 0 <=)]
    (->> [o2 co2]
         (map #(Integer/parseInt % 2))
         (apply *))))

(time (sub-life-support d3-input))
(deftest d3p2
  (is (= 230 (sub-life-support d3-test)))
  (is (= 2990784 (sub-life-support d3-input))))

; --------------------------------------------------------
; Day 4

(def d4-input (slurp (io/resource "2021-d4.txt")))

(def d4-test
  "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7
")

(defn split-callouts-and-boards
  [input]

  (let [[callouts & boards] (s/split input #"\n\n")
        callouts            (map #(Integer/parseInt %) (s/split callouts #","))
        boards              (for [board boards]
                              (->> board
                                   s/split-lines
                                   (map #(s/split % #"\s"))
                                   (map #(filter seq %))
                                   (map #(map (fn [i] (Long/parseLong i)) %))))]
    [callouts boards]))

(defn check-bingo
  [nums board]
  (let [numset        (apply hash-set nums)
        board-cols    (apply map list board)
        rows-and-cols (concat board board-cols)]
    (when (some #(every? numset %) rows-and-cols)
      [nums board])))

(defn check-boards
  [nums boards curnums won-boards]
  (let [board-checks (for [board boards
                           :let  [check (check-bingo curnums board)]
                           :when (some? check)]
                       check)]
    (if (empty? nums)
      (concat won-boards board-checks)
      (check-boards (rest nums)
                    (remove (set (map second board-checks)) boards)
                    (conj curnums (first nums))
                    (concat won-boards board-checks)))))

(defn d4p1-solve
  [input board-picker]
  (let [[nums boards]    (split-callouts-and-boards input)
        [win-nums board] (board-picker (check-boards nums boards [] []))]
    (* (last win-nums) (apply + (remove (set win-nums) (flatten board))))))

(deftest d4p1
  (is (= 4512 (d4p1-solve d4-test first)))
  (is (= 49860 (d4p1-solve d4-input first))))

(deftest d4p2
  (is (= 1924 (d4p1-solve d4-test last)))
  (is (= 24628 (d4p1-solve d4-input last)))
)

; --------------------------------------------------------
; Day 5

(defn d5-process-line
  [input]
  (let [[_ x1 y1 x2 y2] (re-find #"(\d+),(\d+)\s->\s(\d+),(\d+)" input)
        x1              (Long/parseLong x1)
        y1              (Long/parseLong y1)
        x2              (Long/parseLong x2)
        y2              (Long/parseLong y2)]
    [[x1 y1] [x2 y2]]))

(def d5-input (mapv d5-process-line (file->vec "2021-d5.txt")))

(def d5-test
  (mapv
   d5-process-line
   (s/split-lines
    "0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2")))

(defn generate-line-points
  [[[x1 y1] [x2 y2]]]
  (let [len   (inc (max (Math/abs (- x1 x2)) (Math/abs (- y1 y2))))
        xnums (cond
                (= x1 x2) (repeat x1)
                (> x1 x2) (range x1 (dec x2) -1)
                (< x1 x2) (range x1 (inc x2)))
        ynums (cond
                (= y1 y2) (repeat y1)
                (> y1 y2) (range y1 (dec y2) -1)
                (< y1 y2) (range y1 (inc y2)))]
    (take len (map vector xnums ynums))))

(defn get-dangerous-points
  [input]
  (->> input
       (map generate-line-points)
       (apply concat)
       frequencies
       (filter #(>= (second %) 2))))

(defn horizontal-and-vertical-only
  [coll]
  (filter (fn [[[x1 y1] [x2 y2]]] (or (= x1 x2) (= y1 y2))) coll))

(deftest d5p1
  (is (= 5
         (count (get-dangerous-points (horizontal-and-vertical-only d5-test)))))
  (is (= 5608
         (count (get-dangerous-points (horizontal-and-vertical-only
                                       d5-input))))))

(deftest d5p2
  (is (= 12 (count (get-dangerous-points d5-test))))
  (is (= 20299
         (count (get-dangerous-points d5-input)))))

; --------------------------------------------------------
; Day 6

(def d6-input
  (map #(Long/parseLong %)
       (s/split (s/trim (slurp (io/resource "2021-d6.txt"))) #",")))
(def d6-test (map #(Long/parseLong %) (s/split "3,4,3,1,2" #",")))


#_(defn simulate-lanternfish
    [fish]
    (concat (map (comp #(mod % 7) dec) fish)))

(defn simulate-lanternfish-step
  [fish]
  (-> fish
      (assoc 0 (get fish 1))
      (assoc 1 (get fish 2))
      (assoc 2 (get fish 3))
      (assoc 3 (get fish 4))
      (assoc 4 (get fish 5))
      (assoc 5 (get fish 6))
      (assoc 6 (+ (get fish 0) (get fish 7)))
      (assoc 7 (get fish 8))
      (assoc 8 (get fish 0))
  ))

(defn simulate-lanternfish
  [fish days]
  (if (> days 0)
    (recur (simulate-lanternfish-step fish) (dec days))
    fish))

(defn fish-list
  [fish]
  (merge {0 0 1 0 2 0 3 0 4 0 5 0 6 0 7 0 8 0}
         (frequencies fish)))

(deftest d6
  (is (= 26 (apply + (vals (simulate-lanternfish (fish-list d6-test) 18)))))
  (is (= 5934 (apply + (vals (simulate-lanternfish (fish-list d6-test) 80)))))
  (is (= 372300
         (apply + (vals (simulate-lanternfish (fish-list d6-input) 80)))))
  (is (= 1675781200288
         (apply + (vals (simulate-lanternfish (fish-list d6-input) 256))))))

(comment
 (tap> (take 18
             (iterate simulate-lanternfish-step
                      (merge {0 0 1 0 2 0 3 0 4 0 5 0 6 0 7 0 8 0}
                             (frequencies d6-test)))))

 (simulate-lanternfish (merge {0 0 1 0 2 0 3 0 4 0 5 0 6 0 7 0 8 0}
                              (frequencies d6-test))
                       18)

)


; --------------------------------------------------------
; Day 7

(def d7-input
  (map #(Long/parseLong %)
       (s/split (s/trim (slurp (io/resource "2021-d7.txt"))) #",")))
(nth d7-input 499)

(def d7-test [16 1 2 0 4 2 7 1 2 14])

(comment
 (def n [200 200 1 1 1 1 1 1 1 2 20])
 (float (/ (apply + n) (count n)))
 (take 1 (drop (/ (count n) 2) (sort n)))
 (apply max d7-input)
)

(defn d7-p1
  [input]
  (first (sort-by second
                  (for [target (range (apply max input))]
                    [target
                     (reduce +
                             (map #(Math/abs (- % target))
                                  input))]))))

(defn d7-p2
  [input]
  (first (sort-by second
                  (for [target (range (apply max input))]
                    [target
                     (reduce +
                             (map #(let [delta (Math/abs (- % target))]
                                     (/ (* delta (+ 1 delta)) 2))
                                  input))]))))

(deftest d7
  (is (= 37 (second (d7-p1 d7-test))))
  (is (= 364898 (time (second (d7-p1 d7-input)))))
  (is (= 168 (second (d7-p2 d7-test))))
  (is (= 104149091 (second (d7-p2 d7-input)))))

; --------------------------------------------------------
; Day 8

(def d8-input (file->vec "2021-d8.txt"))
(def d8-test (s/split-lines "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"))


(comment :p1
  (as-> d8-input it
    (map #(s/split % #"\|") it)
    (map second it)
    (map #( s/split % #"\s") it)
    (flatten it)
    (map count it)
    (frequencies it)
    (apply + (vals (select-keys it [2 4 3 7]))))
  )


(comment
(as-> d8-test it
    (map #(s/split % #"\s\|\s") it)
    (map first it)
    (map #( s/split % #"\s") it)
    (map #(map set %) it)
    
    )


    (find-1 (first (d8-data d8-test)))
    (find-7 (first (d8-data d8-test)))

    ((set "be"))
)



(defn d8-data [input]
  (as-> input it
    (map #(s/split % #"\s\|\s") it)
    (map (fn [[left right]] [(map set (s/split left #"\s"))
                             (map set (s/split right #"\s"))]) it)))

(defn find-1 [words]
 (first (filter #(= 2 (count %)) words)))

(defn find-7
  [words]
  (first (filter #(= 3 (count %)) words)))

(defn find-4
  [words]
  (first (filter #(= 4 (count %)) words)))

  (defn find-8
  [words]
  (first (filter #(= 7 (count %)) words)))

(defn find-num-mapping
[words]
(let [num-2-3-5 (filter #(= 5 (count %)) words)
      num-1 (find-1 words)
      num-3  (first (filter #(set/subset? num-1 %) num-2-3-5))
      num-4 (find-4 words)
      seg-b  (first (set/difference num-4 num-3))
      num-5 (first (filter #(% seg-b) num-2-3-5))
      num-2 (first (set/difference (set num-2-3-5) #{num-3 num-5}))
      num-0-6-9 (filter #(= 6 (count %)) words)
      num-9 (first (filter #(set/subset? num-4 %) num-0-6-9))
      num-0-6 (remove #{num-9} num-0-6-9)
      num-0 (first (filter #(set/subset? num-1 %) num-0-6))
      num-6 (first (remove #{num-0} num-0-6))
      num-7 (find-7 words)
      num-8 (find-8 words)]
  {num-0 0 num-1 1 num-2 2 num-3 3 num-4 4 num-5 5 num-6 6 num-7 7 num-8 8 num-9 9}))

(defn translate-num
[input mapping]
(Long/parseLong (s/join (map mapping input))))

(comment 
  (for [[left right] (d8-data d8-test)
        :let [mapping (find-num-mapping left)]]
        (translate-num right mapping))
)


