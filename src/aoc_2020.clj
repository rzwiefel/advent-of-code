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
  (map #(Long/parseLong %) (file->vec name)))


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

; --------------------------------------------------------
; Day 8 Part 1 and 2

(def d8-input (->> "2020-d8.txt" file->vec))
(def d8-test
  (->> "nop +0\nacc +1\njmp +4\nacc +3\njmp -3\nacc -99\nacc +1\njmp -4\nacc +6"
       (s/split-lines)))

(defn parse-instruct [line]
  (let [[opstr argstr] (s/split line #"\s")]
    (list (keyword opstr) (Integer/parseInt argstr))))

(deftest test-parse-instruct
  (is (= '(:nop 0) (parse-instruct "nop +0")))
  (is (= '(:acc 1) (parse-instruct "acc +1")))
  (is (= '(:jmp 4) (parse-instruct "jmp +4")))
  (is (= '(:jmp -3) (parse-instruct "jmp -3"))))

(defn pre-execute-state-update [code ptr state]
  (-> state
      (update-in [:linecount ptr] #(if (nil? %) 1 (inc %)))
      (update-in [:instruct-count] inc)))

(defn evaluate-status [code ptr state]
  (cond
    (> (get-in state [:linecount ptr]) 1) {:exit :executed-line-twice :line ptr}
    (> (:instruct-count state) 10000) {:exit :exceeded-instruct-count}
    (>= ptr (count code)) {:exit :success}))

(defn process
  ([boot-code] (process boot-code {}))
  ([boot-code supplied-state]
   (loop [code boot-code
          ptr 0
          state (merge {:acc 0 :instruct-count 0} supplied-state)]
     (let [state (pre-execute-state-update code ptr state)
           status (evaluate-status code ptr state)]
       (if (some? status)
         {:code code :ptr ptr :state state :status status}
         (let [[op arg] (parse-instruct (nth code ptr))]
           (condp = op
             :nop (recur code (inc ptr) state)
             :acc (recur code (inc ptr) (update state :acc + arg))
             :jmp (recur code (+ ptr arg) state))))))))

(deftest test-process
  (is (= 5 (:acc (:state (process d8-test)))))
  (is (= 1331 (:acc (:state (process d8-input))))))

(defn boot-code-permus [boot-code]
  (keep-indexed
    (fn [idx itm]
      (let [[op arg] (parse-instruct itm)]
        (when (#{:nop :jmp} op)
          (assoc
            boot-code
            idx
            (s/replace itm #"nop|jmp" {"nop" "jmp" "jmp" "nop"})))))
    boot-code))

(defn find-working-boot-code [boot-code]
  (for [new-boot (boot-code-permus boot-code)
        :let [res (process new-boot)]
        :when (= :success (get-in res [:status :exit]))]
    res))


(deftest check-bootcode-permu-solver
  (is (= 8 (-> d8-test
               find-working-boot-code
               first
               (get-in [:state :acc]))))
  (is (= 1121 (-> d8-input
                  find-working-boot-code
                  first
                  (get-in [:state :acc])))))

; --------------------------------------------------------
; Day 9 Part 1 and 2

(def d9-input (->> "2020-d9.txt" file->numvec vec))

(def d9-test
  (->> "35\n20\n15\n25\n47\n40\n62\n55\n65\n95\n102\n117\n150\n182\n127\n219\n299\n277\n309\n576"
       s/split-lines
       (map #(Long/parseLong %))
       vec))

(defn d9-check [lines pre-len idx]
  (let [num (nth lines (+ idx pre-len))
        preamble (subvec lines idx (+ idx pre-len))
        combs (c/combinations preamble 2)
        sums (map (fn [pair]
                    (let [[l r] pair sum (+ l r)]
                      [sum [l r]])) combs)
        matches (first (for [[sum pair] sums
                             :when (= sum num)]
                         [sum pair]))]
    (when (empty? matches)
      (list (+ pre-len idx) (lines (+ pre-len idx))))))


(defn d9-find-wrong [lines pre-len]
  (->> (range (- (count lines) pre-len 1))
       (map #(d9-check lines pre-len %))
       (filter some?)
       first
       second))

(deftest d9-solve-pt1
  (is (= 127 (d9-find-wrong d9-test 5)))
  (is (= 14360655 (d9-find-wrong d9-input 25))))

(defn check-contig-num-sum [target nums]
  (loop [sum 0
         nums nums
         used []]
    (cond
      (> sum target) nil
      (= sum target) used
      :else (recur (+ sum (first nums)) (rest nums) (conj used (first nums))))))

(defn find-contig-sum [target nums]
  (if (empty? nums)
    nil
    (let [ans (check-contig-num-sum target nums)]
      (if (nil? ans)
        (recur target (rest nums))
        ans))))

(deftest d9-solve-pt2
  (is (= 62 (let [ans (find-contig-sum 127 d9-test)]
              (+ (apply min ans) (apply max ans)))))
  (is (= 1962331 (let [ans (find-contig-sum 14360655 d9-input)]
                   (+ (apply min ans) (apply max ans))))))

; --------------------------------------------------------
; Day 10

(def d10-input (->> "2020-d10.txt" file->numvec vec))
(def d10-input (vec (sort (conj d10-input (+ (apply max d10-input) 3) 0))))

(def d10-test (->> "16\n10\n15\n5\n1\n11\n7\n19\n6\n12\n4"
                   s/split-lines
                   (map #(Long/parseLong %))
                   vec))
(def d10-test (vec (sort (conj d10-test (+ (apply max d10-test) 3) 0))))

(def d10-test2
  (->> "28\n33\n18\n42\n31\n14\n46\n20\n48\n47\n24\n23\n49\n45\n19\n38\n39\n11\n1\n32\n25\n35\n8\n17\n7\n9\n4\n2\n34\n10\n3"
       s/split-lines
       (map #(Long/parseLong %))
       vec))
(def d10-test2 (vec (sort (conj d10-test2 (+ (apply max d10-test2) 3) 0))))

(defn find-joltage-diff [lines]
  (frequencies (map #(apply - (reverse %)) (partition 2 1 (sort lines)))))

(deftest test-joltage-diff
  (is (= {1 7, 3 5} (find-joltage-diff d10-test)))
  (is (= {1 22, 3 10} (find-joltage-diff d10-test2)))
  (is (= {1 64, 3 31} (find-joltage-diff d10-input))))

(defn find-joltage-arrangements [joltages]
  (->> joltages
       sort
       (partition 2 1)
       (map reverse)
       (map #(apply - %))
       (partition-by identity)
       (filter #(= 1 (first %)))
       (filter #(< 1 (count %)))
       (map (fn [items] ({2 2 3 4 4 7} (count items))))
       (apply *)))

(deftest test-joltage-arrangements
  (is (= 8 (find-joltage-arrangements d10-test)))
  (is (= 19208 (find-joltage-arrangements d10-test2)))
  (is (= 3543369523456 (find-joltage-arrangements d10-input))))


; --------------------
; Day 11

(def d11-input (->> "2020-d11.txt" file->vec (map vec) vec))
(def d11-test (->> "L.LL.LL.LL\nLLLLLLL.LL\nL.L.L..L..\nLLLL.LL.LL\nL.LL.LL.LL\nL.LLLLL.LL\n..L.L.....\nLLLLLLLLLL\nL.LLLLLL.L\nL.LLLLL.LL" s/split-lines (map vec) vec))
(def d11-test2 (->> ".......#.\n...#.....\n.#.......\n.........\n..#L....#\n....#....\n.........\n#........\n...#....." s/split-lines (map vec) vec))


(defn follow-los [board row col rowf colf]
  (let [spot (get (get board (rowf row)) (colf col))]
    (if (contains? #{\L \# nil} spot)
      spot
      (recur board (rowf row) (colf col) rowf colf))))

(defn get-adjacent-los-seats [board row col]
  (let [n (follow-los board row col dec identity)
        ne (follow-los board row col dec inc)
        e (follow-los board row col identity inc)
        se (follow-los board row col inc inc)
        s (follow-los board row col inc identity)
        sw (follow-los board row col inc dec)
        w (follow-los board row col identity dec)
        nw (follow-los board row col dec dec)]
    [n ne e se s sw w nw]))

(defn get-adjacent-seats [board row col]
  (let [n (get (get board (dec row)) col)
        ne (get (get board (dec row)) (inc col))
        e (get (get board row) (inc col))
        se (get (get board (inc row)) (inc col))
        s (get (get board (inc row)) col)
        sw (get (get board (inc row)) (dec col))
        w (get (get board row) (dec col))
        nw (get (get board (dec row)) (dec col))]
    [n ne e se s sw w nw]))

(def ^:dynamic adjacency-func get-adjacent-los-seats)
(def ^:dynamic min-occupied 5)

(defn d11-evaluate [board]
  (let [newboard (for [row (range (count board))
                       col (range (count (first board)))
                       :let [seat ((board row) col)
                             adjacent (adjacency-func board row col)
                             freqs (frequencies adjacent)]
                       :when (not= seat \.)]
                   (do
                     (if (= seat \L)
                       (if (nil? (get freqs \#))
                         (list row col \#)
                         (list row col \L))
                       (if (>= (get freqs \# 0) min-occupied)
                         (list row col \L)
                         (list row col \#)))))]
    (reduce (fn [nb [row col seat]]
              (update-in nb [row col] (fn [old] seat)))
            board
            newboard)))

(defn d11-proces-till-stagnant [oldboard newboard]
  (if (= oldboard newboard)
    newboard
    (recur newboard (d11-evaluate newboard))))

#_(deftest d11-doit
    (binding [adjacency-func get-adjacent-seats
              min-occupied 4]
      (is (= 37 ((frequencies (flatten (d11-proces-till-stagnant nil d11-test))) \#)))
      (is (= 2344 ((frequencies (flatten (d11-proces-till-stagnant nil d11-input))) \#))))
    (is (= 26 ((frequencies (flatten (d11-proces-till-stagnant nil d11-test))) \#)))
    (is (= 2076 ((frequencies (flatten (d11-proces-till-stagnant nil d11-input))) \#))))


; --------------------
; Day 12

(defn d12-parseline [line]
  (let [action (first line)
        num (Long/parseLong (apply str (rest line)))]
    (list action num)))

(def d12-input (->> "2020-d12.txt" file->vec (map d12-parseline)))
(def d12-test (->> "F10\nN3\nF7\nR90\nF11" s/split-lines (map d12-parseline)))
(def d12-test2 (->> ["N1" "E1" "S1" "W1" "R360" "L360"
                     "F10"
                     "N3"
                     "F7"
                     "R90"
                     "L90"
                     "R90"
                     "F11"] (map d12-parseline)))

(defn d12-evaluate [lines xpos ypos direction]
  (if (empty? lines)
    (list xpos ypos)
    (let [[action num] (first lines)]
      (println xpos ypos direction "-" action num)
      (condp = action
        \F (let [x (* (Math/cos (Math/toRadians direction)) num)
                 y (* (Math/sin (Math/toRadians direction)) num)]
             (recur (rest lines) (+ xpos x) (+ ypos y) direction))
        \L (recur (rest lines) xpos ypos (mod (+ direction num) 360))
        \R (recur (rest lines) xpos ypos (mod (- direction num) 360))
        \E (recur (rest lines) (+ num xpos) ypos direction)
        \W (recur (rest lines) (- xpos num) ypos direction)
        \N (recur (rest lines) xpos (+ num ypos) direction)
        \S (recur (rest lines) xpos (- ypos num) direction)))))

(defn d12-evaluate-pt2 [lines xpos ypos xway yway]
  (let [xpos (Math/round (double xpos))
        ypos (Math/round (double ypos))
        xway (Math/round (double xway))
        yway (Math/round (double yway))]
    (if (empty? lines)
      (list xpos ypos)
      (let [[action num] (first lines)]
        (println xpos ypos "   " xway yway "   next> " action num)
        (condp = action
          \F (recur (rest lines) (+ xpos (* num xway)) (+ ypos (* num yway)) xway yway)
          \L (let [angle-rads (Math/atan2 yway xway)
                   dist (Math/sqrt (+ (* yway yway) (* xway xway)))
                   newangle (+ angle-rads (Math/toRadians num))]
               (recur (rest lines) xpos ypos (* (Math/cos newangle) dist) (* (Math/sin newangle) dist)))
          \R (let [angle-rads (Math/atan2 yway xway)
                   dist (Math/sqrt (+ (* yway yway) (* xway xway)))
                   newangle (- angle-rads (Math/toRadians num))]
               (recur (rest lines) xpos ypos (* (Math/cos newangle) dist) (* (Math/sin newangle) dist)))
          \N (recur (rest lines) xpos ypos xway (+ yway num))
          \S (recur (rest lines) xpos ypos xway (- yway num))
          \E (recur (rest lines) xpos ypos (+ xway num) yway)
          \W (recur (rest lines) xpos ypos (- xway num) yway))))))