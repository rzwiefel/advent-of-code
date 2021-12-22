(ns util
  (:require
   [clojure.java.io :as io]
   [clojure.string :as s]))

(defn read-resource
  [name]
  (slurp (io/resource name)))

(defn file->vec
  [name]
  (s/split-lines (read-resource name)))

(defn file->numvec
  [name]
  (map #(Long/parseLong %) (file->vec name)))

(defn parse-int
  ([input radix]
   (Integer/parseInt input radix))
  ([input]
   (Integer/parseInt input)))

(defn parse-long
  ([input radix]
   (Long/parseLong input radix))
  ([input]
   (Long/parseLong input)))

(defn grid-mapv
  [fn input]
  (let [ysize     (count input)
        xsize     (count (first input))
        all-cords (for [y (range ysize)
                        x (range xsize)]
                    [x y])]
    (mapv persistent!
          (persistent!
           (loop [[[x y] & remaining] all-cords
                  grid                (transient (mapv transient input))]
             (let [row   (nth grid y)
                   value (nth row x)]
               (if (nil? remaining)
                 (assoc! grid y (assoc! row x (fn value)))
                 (recur remaining
                        (assoc! grid y (assoc! row x (fn value)))))))))))

(comment
 (grid-mapv inc (mapv (fn [_] (vec (range 10))) (range 10)) )
 (time (doseq [_ (range 10000)]
         (grid-mapv inc (mapv (fn [_] (vec (range 10))) (range 10)) )))
  

)