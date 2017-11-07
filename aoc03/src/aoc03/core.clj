(ns aoc03.core
  (:gen-class)
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.math.combinatorics :as combo]))

(defn parse [input problem]
  (let [rows (->> input
                  (str/split-lines)
                  (map #(str/split % #" "))
                  (map #(map edn/read-string %))
                  (map #(filter some? %)))]
    (condp = problem
      :1 rows
      :2 (partition 3 (apply interleave rows)))))

(defn solve [input problem]
  (let [triangles (parse input problem)]
    (count (filter (fn [sides] (every? (fn [[a b c]] (> (+ a b) c))
                                       (combo/permutations sides)))
                   triangles))))

(defn -main
  [& args]
  (let [problem (keyword (first args))
        input (slurp *in*)]
    (println (if (#{:1 :2} problem)
               (solve input problem)
               "This problem has only two sub-problems. Please supply '1' or '2'."))))
