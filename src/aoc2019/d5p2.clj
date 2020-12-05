(ns aoc2019.d5p2
  (:require [clojure.string :as str]
            [clojure.test :refer [is]]))

(defn- read-value!
  []
  (or (try
        (print "input> ")
        (flush)
        (Integer/valueOf (read-line))
        (catch NumberFormatException _))
      (recur)))

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

(defn compute
  [xs]
  (loop [n 0 state xs]
    (let [[o r1 r2 r3] (drop n state)
          [opcode m1 m2] (parse-op o)]
      (case opcode
        1 (recur (+ 4 n) (assoc state r3 (+ (param state m1 r1) (param state m2 r2))))
        2 (recur (+ 4 n) (assoc state r3 (* (param state m1 r1) (param state m2 r2))))
        3 (recur (+ 2 n) (assoc state r1 (read-value!)))
        4 (do (println "output>" (param state m1 r1))
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

(def input2
  (mapv #(Integer/valueOf (str/trim %)) (str/split (slurp "2.txt") #",")))

(def input5
  (mapv #(Integer/valueOf (str/trim %)) (str/split (slurp "5.txt") #",")))

(comment
  (is (= [2 0 0 0 99] (compute [1,0,0,0,99])))
  (is (= [2 3 0 6 99] (compute [2,3,0,3,99])))
  (is (= [2 4 4 5 99 9801] (compute [2,4,4,5,99,0])))
  (is (= [30 1 1 4 2 5 6 0 99] (compute [1,1,1,4,99,5,6,0,99])))
  (is (= 2894520 (-> input2 (assoc 1 12 2 2) compute (get 0))))
  (is (= [9342] (for [noun (range 99)
                      verb (range 99)
                      :when (= 19690720 (-> input2 (assoc 1 noun 2 verb) compute (get 0)))]
                  (+ (* 100 noun) verb))))
  (is (= [2 0 1 0] (parse-op 1002)))
  (is (= [2 0 0 0] (parse-op 2)))
  (is (= [1002 4 3 4 99] (compute [1002,4,3,4,33]))))

(defn -main
  [& _]
  (compute input5))
