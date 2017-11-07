(ns aoc02.core
  (:gen-class)
  (:require [clojure.string :as str]
            [clojure.spec.alpha :as s]))

(defn rand-char []
  (char (rand-nth (concat (range 48 58) (range 65 69)))))

(defn step-digit [{:keys [x y grid code problem] :as state} dir]
  (let [new-x ((condp = dir
                 :L dec
                 :R inc
                 :U (cond
                      (= problem :1) identity
                      (> y 2)        inc
                      :else          dec)
                 :D (cond
                      (= problem :1) identity
                      (>= y 2)       dec
                      :else          inc))
               x)
        new-y ((condp = dir
                 :U dec
                 :D inc
                 identity)
               y)]
    (into state
          (if (some? (get-in grid [new-y new-x]))
            {:x new-x :y new-y}
            {:x x :y y}))))

(defn step-code [{:keys [code grid] :as state} directions]
  (let [{new-x :x new-y :y} (reduce step-digit
                                    state
                                    directions)]
    (into state {:x new-x
                 :y new-y
                 :code (conj code (get-in grid [new-y new-x]))})))

(defn parse [input]
  (->> input
       (str/split-lines)
       (map #(str/split % #""))
       (map #(map keyword %))))

(defn solve [input problem]
  (:code (reduce step-code
                 (into {:code []
                        :problem problem}
                       (condp = problem
                         :1 {:x 1
                             :y 1
                             :grid [[1 2 3]
                                    [4 5 6]
                                    [7 8 9]]}
                         :2 {:x 0
                             :y 2
                             :grid [[1]
                                    [2 3 4]
                                    [5 6 7 8 9]
                                    [\A \B \C]
                                    [\D]]}))
                 (parse input))))

(defn -main
  [& args]
  (let [problem (keyword (first args))
        input (slurp *in*)]
    (println (if (some? (#{:1 :2} problem))
               (str/join "|" (solve input problem))
               "This problem has only 2 sub-problems. Please supply '1' or '2'."))))
