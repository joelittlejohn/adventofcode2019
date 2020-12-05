(ns aoc2019.d9p1
  (:require [clojure.string :as str]))

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
  [xs]
  (loop [n 0 state xs rb 0]
    (let [[o r1 r2 r3] (drop n state)
          [opcode m1 m2 m3] (parse-op o)]
      (case opcode
        1 (recur (+ 4 n) (assoc state (position m3 r3 rb) (+ (param state m1 r1 rb) (param state m2 r2 rb))) rb)
        2 (recur (+ 4 n) (assoc state (position m3 r3 rb) (* (param state m1 r1 rb) (param state m2 r2 rb))) rb)
        3 (recur (+ 2 n) (assoc state (position m1 r1 rb) (read-value!)) rb)
        4 (do (println "output>" (param state m1 r1 rb))
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

(def input
  (mapv #(Integer/valueOf (str/trim %)) (str/split (slurp "9.txt") #",")))

(defn -main
  [& _]

  ;;(compute (add-memory [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99] 1024))

  ;;(compute [1102,34915192,34915192,7,4,7,99,0])

  ;;(compute [104,1125899906842624,99])

  (compute (add-memory input 1024))
)
