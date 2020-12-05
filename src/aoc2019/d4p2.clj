(ns aoc2019.d4p2)

(defn two-adjacent-same?
  [n]
  (some #(= (count %) 2) (partition-by identity (str n))))

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

(comment
  (n? 125730 579381)
  1411)
