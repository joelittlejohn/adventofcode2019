(ns aoc2019.d4p1)

(defn two-adjacent-same?
  [n]
  (some (fn [[a b]] (= a b)) (partition-all 2 1 (str n))))

(defn never-decrease?
  [n]
  (not (some (fn [[a b]] (and a b (> a b))) (partition-all 2 1 (map #(Integer/valueOf (str %)) (str n))))))

(def possible?
  (every-pred two-adjacent-same? never-decrease?))

(defn n?
  [a b]
  (->> (range a b)
       (filter possible?)
       count))

(n? 125730 579381)
