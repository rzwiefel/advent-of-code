(ns y2023.d3
  (:require
   [util]
   [clojure.string :as string]))

(def input (util/file->vec "2023-d3.txt"))

(def sample-input (string/split-lines "467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598.."))

(defn parse-schematic
  [input]
  (let [coordloop (for [i (range (count input))
                        j (range (count (first input)))
                        :let [c (get-in input [i j])]]
                    [[i j] c])
        coordloop (concat coordloop [[[-1 -1] \.]])]
    (reduce (fn [[building nums parts] [[i j] c]]
              (let [new-parts (if (not (or (Character/isDigit c) (= c \.)))
                                (conj parts {:coords [i j] :part c})
                                parts)
                    new-nums (if (and (not (Character/isDigit c))
                                      (seq (:num building)))
                               (conj nums building)
                               nums)
                    new-building (if (Character/isDigit c)
                                   (merge-with conj building
                                               {:coords [i j] :num (str c)})
                                   {:coords #{} :num []})]

                [new-building new-nums new-parts]))
            [{:coords #{} :num []} [] []]
            coordloop)))

(defn get-surrounding-coords
  [[i j]]
  (for [x (range (- i 1) (+ i 2))
        y (range (- j 1) (+ j 2))
        :when (not (and (= x i) (= y j)))]
    [x y]))

(defn filter-part-nums
  [nums parts]
  (let [valid-part-coords (set (mapcat (comp get-surrounding-coords :coords) parts))]
    (for [{:keys [coords num]} nums
          :when (some #(contains? valid-part-coords %) coords)]
      num)))

(defn p1
  [input]
  (let [[_building nums parts] (parse-schematic input)]
    (->> (filter-part-nums nums parts)
         (map string/join)
         (map util/parse-long)
         (reduce +))))

(comment
  (parse-schematic sample-input)

  (let [[_building nums parts] (parse-schematic sample-input)]
    (filter-part-nums nums parts))
  (p1 sample-input)

  (parse-schematic input)
  (set (map :part (parse-schematic input)))

  (p1 input)
  ;; => 529618


  (defn filter-gear-nums
    [nums parts]
    (for [{:keys [coords _part] :as ogpart} (filter #(= \* (:part %)) parts)
          :let [valid-part-coords (set (mapcat (comp get-surrounding-coords) [coords]))]]
      (assoc ogpart :adjacent-nums (for [{:keys [coords num]} nums
                                         :when (some #(contains? valid-part-coords %) coords)]
                                     (util/parse-long (string/join num))))))

  (defn p2
    [input]
    (let [[_building nums parts] (parse-schematic input)]
      (->> (filter-gear-nums nums parts)
           (filter #(= 2 (count (:adjacent-nums %))))
           (map :adjacent-nums)
           (map (partial apply *))
           (reduce +))))

  (comment
    (let [[_building nums parts] (parse-schematic sample-input)]
      (filter-gear-nums nums parts)))
  
  (p2 sample-input)

  (p2 input)
  ;; => 77509019

  
  )