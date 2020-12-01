(ns aoc-2019
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.test :refer [deftest is are]] 
            ))

(defn read-resource [name]
  (slurp (io/resource name)))
