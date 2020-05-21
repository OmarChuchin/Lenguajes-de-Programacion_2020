;(ns parallelPrimes)

(defn prime?
  "Returns true if n is a prime number, otherwise returns false."
  [n]
  (if (< n 2)
    false
    (loop [i 2]
      (if (<= (* i i) n)
        (if (zero? (rem n i))
          false
          (recur (inc i)))
        true))))

(defn chunks
  [how-many]
  (->>
    (range 1 (inc 5000000))
    (partition (/ 5000000 how-many))))

(defn prime-sum
  [num-threads]
    (->>
      (chunks num-threads)
      (pmap #(reduce + (filter prime? %)))
      (reduce +)))

(time (println (prime-sum 1)))
(time (println (prime-sum 2)))
(time (println (prime-sum 4)))
(time (println (prime-sum 8)))