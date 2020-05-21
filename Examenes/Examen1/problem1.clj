(ns problem1)

;==========================================================
; Jesús Omar Cuenca Espino A01378844
;==========================================================

(require '[clojure.test :refer [deftest is run-tests]])

(defn check-if-contains-7
  "
  Checks if the collection contains the number 7.
  "
  [col]
  (if (empty? col)
    false
    (if (= 7 (first col))
      true
      (check-if-contains-7 (rest col)))))

(defn to-Coll
  "
  Transforms a number into a collection of numbers.
  "
  [number]
  (loop [lst ()
         num number]
    (if (< num 10)
      (concat (list num) lst)
      (recur (concat (list (rem num 10)) lst) (quot num 10)))))

(defn divisible-by7
  "
  Check if a number is mathematically divisible by 7.
  "
  [num]
  (= 0 (rem num 7)))

;==========================================================
(defn lucky-number?
  "Returns true if n is a positive integer number that is
  exactly divisible by 7 or contains at least one digit 7.
  Otherwise returns false."
  [n]
  (or (divisible-by7 n) (check-if-contains-7 (to-Coll n))))

;==========================================================
(deftest test-lucky-number?
  (is (lucky-number? 7))
  (is (lucky-number? 2828))
  (is (lucky-number? 773704))
  (is (lucky-number? 0))
  (is (lucky-number? 14890123808))
  (is (lucky-number? 100000700000))
  (is (not (lucky-number? 1)))
  (is (not (lucky-number? 26)))
  (is (not (lucky-number? 123456890)))
  (is (not (lucky-number? 14890123806))))

;==========================================================
(run-tests)
