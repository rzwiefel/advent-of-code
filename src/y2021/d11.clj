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

(defn doflash [grid [[x y] & remaining] flashed]
  (if (nil? (or x y))
    [grid flashed]
    (if (> (get-point grid [x y]) 9)
      
      (recur (update-point grid [x y] inc)
             (concat (for [xi    (range (dec x) (+ 2 x))
                           yi    (range (dec y) (+ 2 y))
                           :when (not (and (= xi x) (= yi y)))]
                       [xi yi]) remaining) 
             (conj flashed [x y]))
      (recur grid remaining flashed))))

(defn flash
  [grid]
  (let [all-points (for [x (range 0 (count grid))
                         y (range 0 (count grid))]
                     [x y])]
    (doflash grid all-points #{})
    #_(reduce #(check-flash-surrounding %2 %1) grid all-points)))

(defn p1-step
  [input]
  (let [increased (util/grid-mapv inc input)
        flashed   (flash increased)
        set-zero  (util/grid-mapv #(if (> % 9) 0 %) flashed)]
    set-zero))

(comment
 (get-point d11-test [0 1])
 (def input d11-test)

 (let [all-points (for [x (range 0 (count d11-test))
                        y (range 0 (count d11-test))]
                    [x y])])
 (mapv #(mapv inc %) input)
 (map inc (first input))

 (time (take 1 (drop 10000 (iterate (partial util/grid-mapv inc) d11-test)))))