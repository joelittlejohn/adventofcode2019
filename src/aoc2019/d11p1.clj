(ns aoc2019.d11p1
  (:require [clojure.string :as str]))

(def clockwise
  (->> [[0 -1] [1 0] [0 1] [-1 0]]
       cycle
       (partition-all 2 1)
       (map vec)
       (take 4)
       (into {})))

(def anticlockwise
  (reduce-kv #(assoc %1 %3 %2) {} clockwise))

(defprotocol IO
  (input [this])
  (output [this value]))

(defprotocol HullPaintingRobot
  (camera [this])
  (paint [this color])
  (turn [this direction]))

(defn- plus
  [[ax ay] [bx by]]
  [(+ ax bx) (+ ay by)])

(defrecord R [hull position orientation waiting-for-turn]
  HullPaintingRobot
  (camera [this]
    (let [c (get-in @hull (vec (reverse @position)))]
      (if (= "" c)
        0
        c)))
  (paint [this color]
    (swap! hull #(assoc-in % (vec (reverse @position)) color)))
  (turn [this direction]
    (swap! orientation (if (zero? direction) anticlockwise clockwise))
    (swap! position plus @orientation))
  IO
  (input [this]
    (camera this))
  (output [this value]
    (if @waiting-for-turn
      (turn this value)
      (paint this value))
    (swap! waiting-for-turn not)))

(defn- robot
  [hull position orientation]
  (let [hull (atom hull)
        position (atom position)
        orientation (atom orientation)
        waiting-for-turn (atom false)]
    (->R hull position orientation waiting-for-turn)))

(defn- parse-op
  [x]
  (let [opcode (->> x str (take-last 2) (apply str) Integer/valueOf)
        [m1 m2 m3] (->> x str (drop-last 2) reverse (map #(Integer/valueOf (str %))))]
    [opcode (or m1 0) (or m2 0) (or m3 0)]))

(defn position
  [mode value relative-base]
  (case mode
    0 value
    2 (+ relative-base value)))

(defn- param
  [state mode value relative-base]
  (case mode
    0 (state value)
    1 value
    2 (state (+ relative-base value))))

(defn compute
  [xs io]
  (loop [n 0 state xs rb 0]
    (let [[o r1 r2 r3] (drop n state)
          [opcode m1 m2 m3] (parse-op o)]
      (case opcode
        1 (recur (+ 4 n) (assoc state (position m3 r3 rb) (+ (param state m1 r1 rb) (param state m2 r2 rb))) rb)
        2 (recur (+ 4 n) (assoc state (position m3 r3 rb) (* (param state m1 r1 rb) (param state m2 r2 rb))) rb)
        3 (recur (+ 2 n) (assoc state (position m1 r1 rb) (input io)) rb)
        4 (do (output io (param state m1 r1 rb))
              (recur (+ 2 n) state rb))
        5 (if-not (zero? (param state m1 r1 rb))
            (recur (param state m2 r2 rb) state rb)
            (recur (+ 3 n) state rb))
        6 (if (zero? (param state m1 r1 rb))
            (recur (param state m2 r2 rb) state rb)
            (recur (+ 3 n) state rb))
        7 (if (< (param state m1 r1 rb) (param state m2 r2 rb))
            (recur (+ 4 n) (assoc state (position m3 r3 rb) 1) rb)
            (recur (+ 4 n) (assoc state (position m3 r3 rb) 0) rb))
        8 (if (= (param state m1 r1 rb) (param state m2 r2 rb))
            (recur (+ 4 n) (assoc state (position m3 r3 rb) 1) rb)
            (recur (+ 4 n) (assoc state (position m3 r3 rb) 0) rb))
        9 (recur (+ 2 n) state (+ rb (param state m1 r1 rb)))
        99 state))))

(defn- add-memory
  [program states]
  (into program (repeat states 0)))

(defn- grid
  [w h]
  (vec (repeat h (vec (repeat w "")))))

(def input
  (mapv #(Long/valueOf (str/trim %)) (str/split (slurp "11.txt") #",")))

(comment
  (let [r (robot (grid 200 200) [100 100] [0 -1])]

   (compute (add-memory input 1024) r)

   (->> (deref (:hull r))
        (mapcat (fn [row] (filter #(not= "" %) row)))
        count)))
