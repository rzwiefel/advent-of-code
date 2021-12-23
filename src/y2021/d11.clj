(ns y2021.d11
  (:require
   [clojure.string :as s]
   [clojure.pprint :refer [pprint]]
   [portal.api :as p]
   [clojure.set :as set]
   [util :as util]))

(def d11-input
  (util/grid-mapv
   (comp util/parse-long str)
   (mapv vec (util/file->vec "2021-d11.txt"))))

(def d11-test
  (util/grid-mapv
   (comp util/parse-long str)
   (mapv
    vec
    (s/split-lines
     "5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526"))))



(defn get-point
  [grid [x y]]
  (let [xMax (count (first grid))
        yMax (count grid)]
    (if (or (< x 0) (>= x xMax) (< y 0) (>= y yMax))
      nil
      (nth (nth grid y) x))))

(defn set-point
  [grid [x y] value]
  (if (not (and (< -1 x (count (first grid)))
                (< -1 y (count grid))))
    grid
    (let [row (nth grid y)]
      (assoc grid y (assoc row x value)))))

(defn update-point
  [grid [x y] fn]
  (if (not (and (< -1 x (count (first grid)))
                (< -1 y (count grid))))
    grid
    (let [row (nth grid y)]
      (assoc grid y (assoc row x (fn (get-point grid [x y])))))))

(defn get-surrounding
  [[x y] grid]

  (for [xi (range (dec x) (+ 2 x))
        yi (range (dec y) (+ 2 y))]
    [xi yi]))

(defn doflash
  [grid [[x y] & remaining] flashed]
  (if (nil? (or x y))
    [grid flashed]
    (if (and (> (get-point grid [x y]) 9)
             (not (flashed [x y])))
      (let [surrounding-points (for [xi    (range (dec x) (+ 2 x))
                                     yi    (range (dec y) (+ 2 y))
                                     :when (not (or (and (= xi x) (= yi y))
                                                    (nil? (get-point grid [xi yi]))))]
                                 [xi yi])]
        (recur (reduce (fn [acc point] (update-point acc point inc))
                       grid
                       surrounding-points)
               (concat surrounding-points remaining)
               (conj flashed [x y])))
      (recur grid remaining flashed))))

(defn flash
  [grid]
  (let [all-points (for [x (range 0 (count grid))
                         y (range 0 (count grid))]
                     [x y])]
    (doflash grid all-points #{})
    #_(reduce #(check-flash-surrounding %2 %1) grid all-points)))

(defn p1-step
  [[input num-flashed]]
  (let [increased      (util/grid-mapv inc input)
        [grid flashed] (flash increased)
        set-zero       (util/grid-mapv #(if (> % 9) 0 %) grid)]
    [set-zero (+ num-flashed (count flashed))]))

(comment
  (get-point d11-test [0 1])
  (def input d11-test)

  (first (drop 100 (iterate p1-step [d11-input 0])))
  
  comment)

(defn p2-step
  [[input num-flashed step-num]]
  (let [increased      (util/grid-mapv inc input)
        [grid flashed] (flash increased)
        set-zero       (util/grid-mapv #(if (> % 9) 0 %) grid)]
    [set-zero (count flashed) (inc step-num)]))

(comment
  (take 10 (drop 190 (iterate p2-step [d11-test 0 0])))
  (first (drop 100 (iterate p2-step [d11-test 0])))

  ( (drop-while (fn [[_ num-flashed _]] (not= num-flashed 100))
                     (iterate p2-step [d11-input 0 0])))

  comment
  )