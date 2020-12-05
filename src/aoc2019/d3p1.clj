(ns aoc2019.d3p1
  (:require [clojure.string :as str]))

(defmulti move (fn [_ [direction] _] direction))

(defmethod move \U
  [[x y] [_ distance] positions]
  [[x (+ y distance)] (set (concat positions (for [y2 (range (inc y) (inc (+ y distance)))] [x y2])))])

(defmethod move \D
  [[x y] [_ distance] positions]
  [[x (- y distance)] (set (concat positions (for [y2 (range (- y distance) y)] [x y2])))])

(defmethod move \R
  [[x y] [_ distance] positions]
  [[(+ x distance) y] (set (concat positions (for [x2 (range (inc x) (inc (+ x distance)))] [x2 y])))])

(defmethod move \L
  [[x y] [_ distance] positions]
  [[(- x distance) y] (set (concat positions (for [x2 (range (- x distance) x)] [x2 y])))])

(defn- parse
  [wire]
  (->> (str/split wire #",")
       (map (fn [[direction & distance]] [direction (Integer/valueOf (apply str distance))]))))

(defn abs
  [n]
  (max n (- n)))

(defn positions
  [wire]
  (reduce (fn [[position positions] step] (move position step positions)) [[0 0] #{}] wire))

(defn distance
  [[l1 l2]]
  (let [[_ positions1] (positions (parse l1))
        [_ positions2] (positions (parse l2))
        intersections (set/intersection (set positions1) (set positions2))
        _ (prn intersections)
        distances (map (fn [[x y]] (+ (abs x) (abs y))) intersections)]
    (->> distances sort first)))

(distance ["R8,U5,L5,D3" "U7,R6,D4,L4"])

(distance ["R75,D30,R83,U83,L12,D49,R71,U7,L72" "U62,R66,U55,R34,D71,R55,D58,R83"])
(distance ["R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51" "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"])
(distance (str/split-lines (slurp "3.txt")))

(move [0 0] [\U 10] #{})
