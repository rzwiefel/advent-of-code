(ns y2021.d13
  (:require
   [clojure.string :as s]
   [util]))

(def d13-input (util/file->vec "2021-d13.txt"))

(def d13-test
  (s/split-lines
   "6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

foldalongy=7
foldalongx=5"))

(defn set-point
  [grid [x y] value]
  (if (not (and (< -1 x (count (first grid)))
                (< -1 y (count grid))))
    grid
    (let [row (nth grid y)]
      (assoc grid y (assoc row x value)))))

(defn parse-input
  [input]
  (let [[coords folds] (split-with #(not= % "") input)
        coords         (map #(s/split % #",") coords)
        coords         (map #(vector (util/parse-long (first %))
                                     (util/parse-long (second %)))
                            coords)
        folds          (rest folds)
        folds          (map #(s/split % #"=") folds)
        folds          (map #(vector (last (first %))
                                     (util/parse-long (second %)))
                            folds)]
    [coords folds]))

(defn apply-fold
  [points [dir dist]]

  (for [[x y] points]
    (if (= dir \y)
      (if (> y dist)
        (let [dist-to-fold (- y dist)]
          [x (- dist dist-to-fold)])
        [x y])
      (if (> x dist)
        (let [dist-to-fold (- x dist)]
          [(- dist dist-to-fold) y])
        [x y]))))


(defn apply-folds
  [coords folds]
  (map
   println
   (let [canvas (mapv (fn [_] (vec (repeat 50 " "))) (range 10))]
     (reduce
      (fn [grid point]
        (set-point grid point "#"))
      canvas
      (set (reduce (fn [acc fold] (apply-fold acc fold)) coords folds))))))

(comment
  (def input d13-test)

  (parse-input d13-test)

  (let [[coords folds] (parse-input d13-input)
        fold (first folds)]
    (set (apply-fold coords fold)))

  (let [[coords folds] (parse-input d13-input)]
    (apply-folds coords folds))

  comment)