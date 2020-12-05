(ns aoc2019.d12p1
  (:require [clojure.string :as str]))

(defn- parse-planet
  [s]
  (->> s
       (re-find #"<x=(-?\d+), y=(-?\d+), z=(-?\d+)>")
       next
       (mapv #(Integer/valueOf %))))

(defn- plus
  [a b]
  (mapv + a b))

(defn- parse
  [s]
  (->> s str/split-lines (mapv parse-planet)))

(defn- pairs
  [xs]
  (for [a xs
        b xs
        :when (not (= a b))]
    [a b]))

(defn new-velocity-dimension
  [a b v]
  (cond (= a b) v
        (< a b) (inc v)
        (> a b) (dec v)))

(defn new-velocity
  [a b v]
  [(new-velocity-dimension (a 0) (b 0) (v 0))
   (new-velocity-dimension (a 1) (b 1) (v 1))
   (new-velocity-dimension (a 2) (b 2) (v 2))])

(defn steps
  ([positions]
   (steps positions (repeat (count positions) [0 0 0])))
  ([positions velocities]
   (let [pairs (pairs positions)
         new-velocities (vals (reduce (fn [m [a b]] (update m a #(new-velocity a b %))) (zipmap positions velocities) pairs))
         new-positions (mapv plus positions new-velocities)]
     (cons [positions velocities] (lazy-seq (steps new-positions new-velocities))))))

(defn- abs
  [x]
  (int (Math/abs x)))

(defn energy
  [position velocity]
  (let [potential (->> position (map abs) (reduce +))
        kinetic (->> velocity (map abs) (reduce +))]
    (* potential kinetic)))

(defn total-energy
  [positions velocities]
  (->> (map energy positions velocities)
       (reduce +)))

(defn find-step
  [position steps]
  (loop [[x & xs] steps n 0]
    (when (zero? (mod n 10000))
      (prn n))
    (if (= position (first x))
      n
      (recur xs (inc n)))))

(def input
  (parse (slurp "12.txt")))

(def example1
  (parse "<x=-1, y=0, z=2>
<x=2, y=-10, z=-7>
<x=4, y=-8, z=8>
<x=3, y=5, z=-1>"))

(comment

  (apply total-energy (-> example1 steps (nth 10)))
  179

  (apply total-energy (-> input steps (nth 1000)))
  8538)
