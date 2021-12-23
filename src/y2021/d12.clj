(ns y2021.d12
  (:require [clojure.string :as s]
            [util]))

(def d12-input (map #(s/split % #"-") (util/file->vec "2021-d12.txt")))

(def test1
  (map #(s/split % #"-")
    (s/split-lines "start-A
start-b
A-c
A-b
b-d
A-end
b-end")))

(def test2
  (map #(s/split % #"-")
    (s/split-lines
      "dc-end
HN-start
start-kj 
dc-start
dc-HN
LN-dc
HN-end
kj-sa
kj-HN
kj-dc")))

(def test3
  (map #(s/split % #"-")
    (s/split-lines
      "fs-end
he-DX
fs-he
start-DX
pj-DX
end-zg
zg-sl
zg-pj
pj-he
RW-he
fs-DX
pj-RW
zg-RW
start-pj
he-WI
zg-he
pj-fs
start-RW")))

(def test4
  (map #(s/split % #"-")
    (s/split-lines #_"start-a\na-end" "start-a\na-c\nc-end\nstart-b\nb-end")))

(defn upper-case? [input] (Character/isUpperCase (first input)))
(defn lower-case? [input] (not (upper-case? input)))

(defn build-graph
  [input]
  (reduce (fn [acc [v1 v2]]
            (-> acc
                (update v1 concat [v2])
                (update v2 concat [v1])))
    {}
    input))

(defn walk-graph
  [graph current ]
  (let [node (last current)]
    (if (= node "end")
      [current]
      (if (lower-case? node)
        (when-not (some #(= node %) (butlast current))
          (apply concat
            (for [child (get graph node)]
              (walk-graph graph (conj current child) ))))
        (apply concat
          (for [child (get graph node)]
            (walk-graph graph (conj current child) )))))))


(comment (def input test1)
         (walk-graph (build-graph test1) ["start"])
         comment)


(defn walk-graph-p2
  [graph current ]
  (let [node (last current)]
    (if (= node "end")
      [current]
      (if (lower-case? node)
        (when (or (>= 2 (count (filter #(= node %) 
                                        current #_(butlast current))))
                   #_(= node "start"))

          (apply concat
            (for [child (get graph node)]
              (walk-graph-p2 graph (conj current child) ))))
        (apply concat
          (for [child (get graph node)]
            (walk-graph-p2 graph (conj current child) )))))))


(comment
  (count (walk-graph-p2 (build-graph test1) ["start"]))
   (meta walk-graph-p2)

  comment)