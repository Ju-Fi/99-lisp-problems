(ns lists)

;; Problem 01
(defn my-last
  [coll]
  (if (empty? coll)
    nil
    (if (empty? (rest coll))
      coll
      (recur (rest coll)))))

;; Problem 02
(defn last-two
  [coll]
  (if (empty? coll)
    nil
    (let [[a b c] coll]
      (if c
        (recur (rest coll))
        (list a b)))))

;; Problem 03
(defn at
  [coll k]
  (if (> k 1)
    (recur (rest coll) (dec k))
    (first coll)))

;; Problem 04
(defn size
  [coll]
  (loop [coll coll
         cnt 0]
    (if (empty? coll)
      cnt
      (recur (rest coll) (inc cnt)))))

(defn cool-size
  [coll]
  (reduce (fn [cnt _] (inc cnt)) 0 coll))

;; Problem 05
(defn reverso
  [coll]
  (seq
   (loop [source coll reversed []]
     (if (empty? (rest source))
       (conj reversed (first source))
       (recur (butlast source) (conj reversed (last source)))))))

;; Problem 06
(defn is-palindrome
  [coll]
  (= coll (reverso coll)))

;; Problem 07
(defn my-flatten
  [coll]
  (filter #(not (seq? %))
          (tree-seq seq? seq coll)))

;; Problem 08
(defn compress
  [coll]
  (->> (partition-all 2 1 coll)
       (filter (fn [[a b]] (not= a b)))
       (map first)))

;; Problem 09
(defn pack
  [coll]
  (partition-by identity coll))

;; Problem 10
(defn encode
  [coll]
  (map #(list (count %) (first %))
       (pack coll)))

(comment
  (my-last '(1 2 3 4))
  (last-two '(1 2 3 4))
  (at '(1 2 3 4) 3)
  (size '(1 2 3 4))
  (cool-size '(1 2 3 4))
  (reverso '(1 2 3 4))
  (is-palindrome '(1 2 2 1))
  (my-flatten '(1 2 (3 (4))))
  (compress '(1 1 1 1 2 3 3 1 1 4 5 5 5 5))
  (pack '(1 1 1 1 2 3 3 1 1 4 5 5 5 5))
  (encode '(1 1 1 1 2 3 3 1 1 4 5 5 5 5))
)
