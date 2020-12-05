(ns aoc2019.d3p2
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defmulti move (fn [_ [direction] _] direction))

(defmethod move \U
  [[x y s] [_ distance] positions]
  [[x (+ y distance) (+ s distance)]
   (set (concat positions (for [i (range 1 (inc distance))]
                            [x (+ y i) (+ s i)])))])

(defmethod move \D
  [[x y s] [_ distance] positions]
  [[x (- y distance) (+ s distance)]
   (set (concat positions (for [i (range 1 (inc distance))]
                            [x (- y i) (+ s i)])))])

(defmethod move \R
  [[x y s] [_ distance] positions]
  [[(+ x distance) y (+ s distance)]
   (set (concat positions (for [i (range 1 (inc distance))]
                            [(+ x i) y (+ s i)])))])

(defmethod move \L
  [[x y s] [_ distance] positions]
  [[(- x distance) y (+ s distance)]
   (set (concat positions (for [i (range 1 (inc distance))]
                            [(- x i) y (+ s i)])))])

(defn- parse
  [wire]
  (->> (str/split wire #",")
       (map (fn [[direction & distance]] [direction (Integer/valueOf (apply str distance))]))))

(defn abs
  [n]
  (max n (- n)))

(defn positions
  [wire]
  (reduce (fn [[position positions] step] (move position step positions)) [[0 0 0] #{}] wire))

(defn steps
  [[l1 l2]]
  (let [positions1 (->> (positions (parse l1)) second (sort-by last))
        positions2 (->> (positions (parse l2)) second (sort-by last))
        intersections (set/intersection (set (map (juxt first second) positions1))
                                        (set (map (juxt first second) positions2)))
        steps (for [i intersections]
                (+ (some (fn [[x y s]] (when (= i [x y]) s)) positions1)
                   (some (fn [[x y s]] (when (= i [x y]) s)) positions2)))]
    (->> steps sort first)))

(steps ["R8,U5,L5,D3" "U7,R6,D4,L4"])

(steps ["R75,D30,R83,U83,L12,D49,R71,U7,L72" "U62,R66,U55,R34,D71,R55,D58,R83"])
(steps ["R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51" "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"])
(steps (str/split-lines (slurp "3.txt")))
