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

;; Problem 11
(defn modified-encode
  [coll]
  (map #(if (> (count %) 1)
          (list (count %) (first %))
          (first %))
       (pack coll)))

;; Problem 12
(defn decode
  [encoded]
  (my-flatten
   (map #(repeat (first %) (last %)) encoded)))

(defn modified-decode
  [encoded]
  (flatten
   (map #(if (seq? %)
            (repeat (first %) (last %))
            %)
        encoded)))

;; Problem 13
(defn encode-direct
  [coll]
  (seq
   (loop [n 0 prev (first coll) cnt 0 encoded []]
     (if (< n (count coll))
       (recur (inc n) (nth coll n)
              (if (= (nth coll n) prev)
                (inc cnt)
                1)
              (if (not= (nth coll n) prev)
                (conj encoded
                      (if (= cnt 1)
                        prev
                        (list cnt prev)))
                encoded))
       (conj encoded (list cnt prev))))))

;; Problem 14
(defn duplicate
  [coll]
  (my-flatten
   (map #(list % %) coll)))

;; Problem 15
(defn replicate
  [coll]
  (my-flatten
   (map #(list % % %) coll)))

;; Problem 16
(defn my-drop
  [coll n]
  (->> (partition n coll)
       (map butlast)
       (apply concat [])))

;; Problem 17
(defn my-split
  [coll len]
  (list (take len coll) (take-last (- (count coll) len) coll)))

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
  (modified-encode '(1 1 1 1 2 3 3 1 1 4 5 5 5 5))
  (decode (encode '(1 1 1 1 2 3 3 1 1 4 5 5 5 5)))
  (modified-decode (modified-encode '(1 1 1 1 2 3 3 1 1 4 5 5 5 5)))
  (encode-direct '(1 1 1 1 2 3 3 1 1 4 5 5 5 5))
  (duplicate '(1 2 3 3 4))
  (replicate '(1 2 3 3 4))
  (my-drop '(1 2 3 4 5 6 7 8 9 10) 3)
  (my-split '(1 2 3 4 5 6 7 8 9 10) 3)
)
