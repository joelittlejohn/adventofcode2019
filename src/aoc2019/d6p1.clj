(ns aoc2019.d6p1
  (:require [clojure.string :as str]))

(defn- parse
  [s]
  (->> s
       str/split-lines
       (map #(str/split % #"\)"))))

(defn count-orbits
  [orbits path]
  (if-let [orbiters (get orbits (last path))]
    (+ (* (count path) (count orbiters))
       (apply + (map #(count-orbits orbits (conj path %)) orbiters)))
    0))

(defn total-orbits
  [s]
  (let [orbits (reduce (fn [m [a b]] (update m a #(if % (conj % b) [b]) )) {} (parse s))]
    (count-orbits orbits ["COM"])))

(def input
  (slurp "6.txt"))

(total-orbits
 "COM)B
B)C
C)D
D)E
E)F
B)G
G)H
D)I
E)J
J)K
K)L")
42


(total-orbits input)
333679
