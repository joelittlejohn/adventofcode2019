(ns aoc2019.d7p2
  (:require [clojure.string :as str]
            [clojure.core.async :refer [<!! >!! chan put!]]))

(defn- parse-op
  [x]
  (let [opcode (->> x str (take-last 2) (apply str) Integer/valueOf)
        [m1 m2 m3] (->> x str (drop-last 2) reverse (map #(Integer/valueOf (str %))))]
    [opcode (or m1 0) (or m2 0) (or m3 0)]))

(defn- param
  [state mode value]
  (case mode
    0 (state value)
    1 value))

(defn- compute
  [xs input output]
  (loop [n 0 state xs]
    (let [[o r1 r2 r3] (drop n state)
          [opcode m1 m2] (parse-op o)]
      (case opcode
        1 (recur (+ 4 n) (assoc state r3 (+ (param state m1 r1) (param state m2 r2))))
        2 (recur (+ 4 n) (assoc state r3 (* (param state m1 r1) (param state m2 r2))))
        3 (recur (+ 2 n) (assoc state r1 (<!! input)))
        4 (do (put! output (param state m1 r1))
              (recur (+ 2 n) state))
        5 (if-not (zero? (param state m1 r1))
            (recur (param state m2 r2) state)
            (recur (+ 3 n) state))
        6 (if (zero? (param state m1 r1))
            (recur (param state m2 r2) state)
            (recur (+ 3 n) state))
        7 (if (< (param state m1 r1) (param state m2 r2))
            (recur (+ 4 n) (assoc state r3 1))
            (recur (+ 4 n) (assoc state r3 0)))
        8 (if (= (param state m1 r1) (param state m2 r2))
            (recur (+ 4 n) (assoc state r3 1))
            (recur (+ 4 n) (assoc state r3 0)))
        99 state))))

(defn run-amplifiers
  [program [A B C D E]]
  (let [->A (chan)
        A->B (chan)
        B->C (chan)
        C->D (chan)
        D->E (chan)]

    (put! ->A A)
    (put! ->A 0)
    (put! A->B B)
    (put! B->C C)
    (put! C->D D)
    (put! D->E E)

    (future (compute program ->A A->B))
    (future (compute program A->B B->C))
    (future (compute program B->C C->D))
    (future (compute program C->D D->E))
    (compute program D->E ->A)

    (<!! ->A)))

(defn best-phase-settings
  [program]
  (let [results (for [A (range 5 10) B (range 5 10) C (range 5 10) D (range 5 10) E (range 5 10)
                      :let [settings [A B C D E]]
                      :when (= 5 (count (set settings)))]
                  [(run-amplifiers program settings) settings])]
    (->> results (sort-by first) last)))

(def input
  (mapv #(Integer/valueOf (str/trim %)) (str/split (slurp "7.txt") #",")))

(comment

  (run-amplifiers [3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5] [9,8,7,6,5])
  (best-phase-settings [3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5])

  (best-phase-settings [3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0])
  [54321 [0 1 2 3 4]]

  (best-phase-settings [3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0])
  [65210 [1 0 4 3 2]]

  (best-phase-settings input)
  [11828 [4 0 2 3 1]])
