(ns aoc2019.d10p1
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
            [dx dy] (direction relative-position)
            blocked-positions (->> (iterate (fn [[x y]] [(+ x dx) (+ y dy)]) a)
                                   (drop 1)
                                   (take-while (fn [[x y]] (and (>= x 0) (>= y 0) (< x w) (< y h)))))]
        (recur rest (apply disj remaining blocked-positions)))
      remaining)))

(defn- best-monitoring-station
  [s]
  (let [in (parse s)
        w (count (first in))
        h (count in)
        asteroid-positions (filter (fn [[x y]] (asteroid? (get-in in [y x]))) (grid in))]
    (->> (for [p asteroid-positions
               :let [other-asteroids (disj (set asteroid-positions) p)]]
           [p (count (asteroids-visible p other-asteroids w h))])
         (sort-by last)
         last)))

(def example1 "
.#..#
.....
#####
....#
...##")

(def example2 "
......#.#.
#..#.#....
..#######.
.#.#.###..
.#..#.....
..#....#.#
#..#....#.
.##.#..###
##...#..#.
.#....####")

(def example3 "
#.#...#.#.
.###....#.
.#....#...
##.#.#.#.#
....#.#.#.
.##..###.#
..#...##..
..##....##
......#...
.####.###.")

(def example4 "
.#..#..###
####.###.#
....###.#.
..###.##.#
##.##.#.#.
....###..#
..#.#..#.#
#..#.#.###
.##...##.#
.....#.#..")

(def example5 "
.#..##.###...#######
##.############..##.
.#.######.########.#
.###.#######.####.#.
#####.##.#.##.###.##
..#####..#.#########
####################
#.####....###.#.#.##
##.#################
#####.##.###..####..
..######..##.#######
####.##.####...##..#
.#####..#.######.###
##...#.##########...
#.##########.#######
.####.#.###.###.#.##
....##.##.###..#####
.#.#.###########.###
#.#.#.#####.####.###
###.##.####.##.#..##")

(def input
  (slurp "10.txt"))

(best-monitoring-station example1)
[[3 4] 8]

(best-monitoring-station example2)
[[5 8] 33]

(best-monitoring-station example3)
[[1 2] 35]

(best-monitoring-station example4)
[[6 3] 41]

(best-monitoring-station example5)
[[11 13] 210]

(best-monitoring-station input)
[[26 29] 303]
