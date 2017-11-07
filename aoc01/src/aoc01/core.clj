(ns aoc01.core
  (:gen-class)
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.core.match :refer [match]]))

(defn turn [c-dir dir]
  (match c-dir
         :N (if (= dir :L) :W :E)
         :S (if (= dir :L) :E :W)
         :E (if (= dir :L) :N :S)
         :W (if (= dir :L) :S :N)))

(defn step [{x :x y :y c-dir :dir}
            {dir :dir dist :dist}]
  (let [new-c-dir (turn c-dir dir)
        new-x (+ x (match new-c-dir
                          :E    dist
                          :W    (- dist)
                          :else 0))
        new-y (+ y (match new-c-dir
                          :N    dist
                          :S    (- dist)
                          :else 0))]
    {:x   new-x
     :y   new-y
     :dir new-c-dir}))

(defn taxi-distance [x y] (+ (Math/abs x) (Math/abs y)))

(defn between [a b]
  (if (> b a)
    (range (inc a) (inc b))
    (range (dec a) (dec b) -1)))

(defn step-until-dupe
  [{x :x y :y c-dir :dir visited :visited} {dir :dir dist :dist}]
  (let [new-c-dir       (turn c-dir dir)
        new-x           (+ x (match new-c-dir
                                    :E    dist
                                    :W    (- dist)
                                    :else 0))
        new-y           (+ y (match new-c-dir
                                    :N    dist
                                    :S    (- dist)
                                    :else 0))
        new-visited     (if (or (= :N new-c-dir)
                                (= :S new-c-dir))
                          (map #(hash-map :x x :y %)
                               (between y new-y))
                          (map #(hash-map :x % :y y)
                               (between x new-x)))
        {dest-x :x dest-y :y} (first (filter #(contains? visited %)
                                             new-visited))]

    (if (and dest-x dest-y)
      (reduced {:x       dest-x
                :y       dest-y})
      {:x       new-x
       :y       new-y
       :dir     new-c-dir
       :visited (into visited new-visited)})))

(defn parse [input]
  (-> input
      (str/trim)
      (str/split #", ")
      (->> (map (fn [turn]
                  (->> turn
                       (split-at 1)
                       (map #(apply str %)))))
           (map (fn [[dir dist]]
                  {:dir (keyword dir) :dist (Integer/parseInt dist)})))))

(defn solve [input problem]
  (let [directions (parse input)
        init-state {:x 0 :y 0 :dir :N :visited #{{:x 0 :y 0}}}
        {x :x y :y} (reduce (match problem
                                   :1 step
                                   :2 step-until-dupe)
                            init-state
                            directions)]
    (taxi-distance x y)))

(defn -main
[& args]
(let [problem (keyword (first args))
      input (slurp *in*)]
  (println
   (if (or (= :1 problem)
           (= :2 problem))
     (solve input problem)
     "This problem has only two sub-problems. Supply '1' or '2'."))))
