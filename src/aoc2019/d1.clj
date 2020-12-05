(ns aoc2019.d1
  (:require [clojure.java.io :as io]))

(def input1
  (with-open [r (io/reader "1.txt")]
    (doall (->> (line-seq r) (map #(Integer/valueOf %))))))

(defn fuel
  [x]
  (- (int (/ x 3)) 2))

(int (reduce #(+ %1 (fuel %2)) 0 input1))

3388015

(defn fuel2
  [m]
  (let [f (- (int (/ m 3)) 2)]
    (if (pos? f)
      (+ f (fuel2 f))
      0)))

(int (reduce #(+ %1 (fuel2 %2)) 0 input1))
5079140
