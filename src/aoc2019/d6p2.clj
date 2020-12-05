(ns aoc2019.d6p2
  (:require [clojure.string :as str]))

(defn- parse
  [s]
  (->> s
       str/split-lines
       (map #(str/split % #"\)"))))

(defn- path-to-com
  [orbits x]
  (loop [path [] current-node x]
    (let [parent (orbits current-node)]
      (if (= "COM" parent)
        path
        (recur (conj path current-node) parent)))))

(defn- transfers
  [orbits start finish]
  (let [start-path-to-com (path-to-com orbits start)
        finish-path-to-com (path-to-com orbits finish)
        first-common-node (some (set start-path-to-com) finish-path-to-com)
        path (concat (take-while #(not= first-common-node %) start-path-to-com)
                     [first-common-node]
                     (reverse (take-while #(not= first-common-node %) finish-path-to-com)))]
    (dec (count path))))

(defn you-to-san
  [s]
  (let [orbits (reduce (fn [m [a b]] (assoc m b a)) {} (parse s))]
    (transfers orbits (orbits "YOU") (orbits "SAN"))))

(def input
  (slurp "6.txt"))

(you-to-san
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
K)L
K)YOU
I)SAN")
4

(you-to-san input)
370
