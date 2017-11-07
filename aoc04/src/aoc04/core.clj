(ns aoc04.core
  (:gen-class)
  (:require [clojure.string :as str]
            [clojure.edn :as edn]))

(defn parse [{:keys [input]}]
  (->> input
       (str/split-lines)
       (map #(drop 1 (re-matches #"((?:[a-z]+-)+)(\d+)\[([a-z]{5}+)\]" %)))
       (map (fn [[enc-name sec-id checksum]]
              [(filter (complement str/blank?)
                       (str/split enc-name #"-"))
               (edn/read-string sec-id)
               checksum]))))

(defn chars-by-frequency [strings]
  (->> strings
       (str/join)
       (frequencies)
       (sort #(let [freq-comp (compare (val %2)
                                       (val %1))]
                (if (= 0 freq-comp)
                  (compare (key %1) (key %2))
                  freq-comp)))
       (keys)))

(defn decrypt-word [{:keys [ciphertext key]}]
  (->> ciphertext
       (map int)
       (map #(+ 97
                (mod (+ (- % 97)
                        key)
                     26)))
       (map char)
       (str/join)))

(defn decrypt-name [{:keys [ciphertexts key]}]
  (->> ciphertexts
       (map #(decrypt-word {:ciphertext % :key key}))
       (str/join " ")))

(defn solve [{:keys [input problem]}]
  (let [rooms (parse {:input input :problem problem})
        real-rooms (filter (fn [[enc-name sec-id checksum]]
                             (= checksum
                                (str/join (take 5 (chars-by-frequency enc-name)))))
                           rooms)]
    (condp = problem
      :1 (reduce (fn [sum [_ sec-id _]]
                   (+ sum sec-id))
                 0
                 real-rooms)
      :2 (->> real-rooms
              (map (fn [[enc-name sec-id _]]
                     {:id sec-id
                      :name (decrypt-name {:ciphertexts enc-name
                                           :key sec-id})}))
              (filter #(re-find #"north" (:name %)))))))

(defn -main
  [& args]
  (let [problem (keyword (first args))
        input (slurp *in*)]
    (println (if (#{:1 :2} problem)
               (solve {:input input :problem problem})
               "This problem has only two sub-problems. Please supply '1' or '2'."))))
