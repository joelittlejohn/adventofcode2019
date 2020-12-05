(ns aoc2019.d8p2
  (:require [clojure.string :as str]))

(def black 0)
(def white 1)
(def transparent 2)

(def input
  (map #(Integer/valueOf (str %)) (str/trim (slurp "8.txt"))))

(defn- parse-image
  [in w h]
  (->> in
       (partition w)
       (partition h)
       vec))

(defn- flatten
  [image]
  (apply map (fn [& rows]
               (apply map (fn [& pixels]
                            (reduce #(if (= transparent %1) %2 %1) pixels)) rows))
         image))

(def image
  (parse-image input 25 6))

(defn- print-image
  [image]
  (doseq [row image]
    (doseq [pixel row]
      (case pixel
        0 (print "▒")
        1 (print "█")
        2 (print " ")))
    (println "")))

(print-image (flatten image))
