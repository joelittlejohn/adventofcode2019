(ns aoc2019.d8p1
  (:require [clojure.string :as str]))

(def input
  (map #(Integer/valueOf (str %)) (str/trim (slurp "8.txt"))))

(defn- parse-image
  [in w h]
  (->> in
       (partition w)
       (partition h)
       vec))

(defn count-pixels-matching
  [layer q]
  (apply + (map (fn [row] (count (filter #(= q %) row))) layer)))

(def i
  (parse-image input 25 6))

(let [n (->> i
             (map-indexed (fn [i layer] [i (count-pixels-matching layer 0)]))
             (sort-by second)
             first
             first)
      layer (i n)
      ones (count-pixels-matching layer 1)
      twos (count-pixels-matching layer 2)]
  (* ones twos))

2159
