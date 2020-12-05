(ns aoc2019.d10p2
  (:require [clojure.string :as str]))

(defn- parse
  [s]
  (->> s
       str/trim
       str/split-lines
       (mapv vec)))

(defn- asteroid?
  [c]
  (= \# c))

(defn- minus
  [[ax ay] [bx by]]
  [(- ax bx) (- ay by)])

(defn- plus
  [[ax ay] [bx by]]
  [(+ ax bx) (+ ay by)])

(defn- multiply
  [[ax ay] [bx by]]
  [(* ax bx) (* ay by)])

(defn- direction
  [[x y]]
  (let [[dx dy] (cond (zero? x) [0 1]
                      (zero? y) [1 0]
                      :else (let [r (/ x y)]
                              (if (ratio? r)
                                [(int (numerator r)) (int (denominator r))]
                                [r 1])))]
    [(* (Integer/signum x) (Math/abs dx)) (* (Integer/signum y) (Math/abs dy))]))

(defn- grid
  [xs]
  (for [x (range (count (first xs)))
        y (range (count xs))]
    [x y]))

(defn- asteroids-visible
  [p asteroids w h]
  (loop [[a & rest] asteroids remaining (set asteroids)]
    (if a
      (let [relative-position (minus a p)
            d (direction relative-position)
            blocked-positions (->> (iterate #(plus % d) a)
                                   (drop 1)
                                   (take-while (fn [[x y]] (and (>= x 0) (>= y 0) (< x w) (< y h)))))]
        (recur rest (apply disj remaining blocked-positions)))
      remaining)))

(defn- gradient
  [[x y]]
  (cond (zero? x) Integer/MAX_VALUE
        (zero? y) 0
        :else (/ y x)))

(defn- sort-clockwise
  [p asteroids]
  (->> (for [a asteroids
             :let [[rx ry :as relative-position] (minus a p)
                   quadrant (cond (and (>= rx 0) (neg? ry)) 3
                                  (and (>= rx 0) (>= ry 0)) 2
                                  (and (neg? rx) (>= ry 0)) 1
                                  (and (neg? rx) (neg? ry)) 0)
                   g (case quadrant
                       3 (gradient (multiply relative-position [1 -1]))
                       2 (gradient (reverse relative-position))
                       1 (gradient (multiply relative-position [-1 1]))
                       0 (gradient (reverse relative-position)))]]
         {:a a :relative-position relative-position :quadrant quadrant :gradient g})
       (sort-by (juxt :quadrant :gradient))
       (map :a)
       reverse))

(defn- destroyed
  [p asteroid-positions w h]
  (let [destroyed-by-sweep (->> (asteroids-visible p asteroid-positions w h)
                                (sort-clockwise p))
        remaining (apply disj (set asteroid-positions) destroyed-by-sweep)]
    (if (seq remaining)
      (concat destroyed-by-sweep (lazy-seq (destroyed p remaining w h)))
      destroyed-by-sweep)))

(def example1 "
.#....#####...#..
##...##.#####..##
##...#...#.#####.
..#.....X...###..
..#.#.....#....##")

(def input
  (slurp "10.txt"))

(let [in (parse input)
      w (count (first in))
      h (count in)
      p [26 29]
      asteroid-positions (-> (filter (fn [[x y]] (asteroid? (get-in in [y x]))) (grid in))
                             set
                             (disj p))
      destroyed (destroyed p asteroid-positions w h)
      [x y] (nth destroyed 199)]
  (+ (* 100 x) y))
