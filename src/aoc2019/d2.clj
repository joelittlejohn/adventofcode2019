(ns aoc2019.d2
  (:require [clojure.string :as str]))

(def input2
  (mapv #(Integer/valueOf (str/trim %)) (str/split (slurp "2.txt") #",")))

(defn compute
  [xs]
  (loop [n 0 state xs]
    (let [[opcode r1 r2 r3] (drop n state)]
      (case opcode
        1 (recur (+ 4 n) (assoc state r3 (+ (state r1) (state r2))))
        2 (recur (+ 4 n) (assoc state r3 (* (state r1) (state r2))))
        99 state))))

(compute [1,0,0,0,99])
(compute [2,3,0,3,99])
(compute [2,4,4,5,99,0])
(compute [1,1,1,4,99,5,6,0,99])

(-> input2 (assoc 1 12 2 2) compute (get 0))
2894520

(for [noun (range 99)
      verb (range 99)
      :when (= 19690720 (-> input2 (assoc 1 noun 2 verb) compute (get 0)))]
  (+ (* 100 noun) verb))
