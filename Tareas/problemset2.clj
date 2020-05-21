(ns problemset2)

;----------------------------------------------------------
; Problem Set #2
; Date: March 12, 2020.
; Authors:
;          A01378844 Jesus Omar Cuenca Espino
;----------------------------------------------------------


(require '[clojure.test :refer [deftest is run-tests]])

(defn replic
  "
  Returns a list with each element within lst multiplied by times.
  "
  ([times lst]
   (if (or (<= times 0) (empty? lst))
     ()
     (replic times (rest lst) (repeat times (first lst)))))

  ([times lst result]
   (if (empty? lst)
     result
     (replic times
             (rest lst)
             (concat result (repeat times (first lst)))))))

(deftest test-replic
  (is (= () (replic 7 ())))
  (is (= () (replic 0 '(a b c))))
  (is (= '(a a a) (replic 3 '(a))))
  (is (= '(1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4)
         (replic 4 '(1 2 3 4)))))

(defn expand
  "
  Returns a list where the elements are multiplied by the position the are in.
  "
  ([lst]
   (expand lst 1 ()))
  ([lst times result]
   (if (empty? lst)
     result
     (expand (rest lst)
             (inc times)
             (concat result (repeat times (first lst)))))))

(deftest test-expand
  (is (= () (expand ())))
  (is (= '(a) (expand '(a))))
  (is (= '(1 2 2 3 3 3 4 4 4 4) (expand '(1 2 3 4))))
  (is (= '(a b b c c c d d d d e e e e e)
         (expand '(a b c d e)))))

(defn insert
  "Inserts the value provided into the list in the right order"
  ([n lst]
   (insert n lst 0))
  ([n lst position]
     (if (not= nil (first (drop position lst)))
       (if (< n (first (drop position lst)))
         (concat (take position lst) (list n) (drop position lst))
         (insert n lst (inc position)))
       (concat lst (list n) ))))

(deftest test-insert
  (is (= '(14) (insert 14 ())))
  (is (= '(4 5 6 7 8) (insert 4 '(5 6 7 8))))
  (is (= '(1 3 5 6 7 9 16) (insert 5 '(1 3 6 7 9 16))))
  (is (= '(1 5 6 10) (insert 10 '(1 5 6)))))

(defn my-sort
  "Sorts the array given to the function from smallest value to the biggest"
  ([lst]
   (my-sort lst ()))
  ([lst result]
   (if (empty? lst)
     result
     (my-sort (rest lst) (insert (first lst) result)))))

(deftest test-my-sort
  (is (= () (my-sort ())))
  (is (= '(0 1 3 3 4 6 7 8 9) (my-sort '(4 3 6 8 3 0 9 1 7))))
  (is (= '(1 2 3 4 5 6) (my-sort '(1 2 3 4 5 6))))
  (is (= '(1 5 5 5 5 5 5) (my-sort '(5 5 5 1 5 5 5)))))

(defn rotate-left
  "Rotates to the left the list the number that is provided"
  [times lst]
  (if (empty? lst)
    ()
    (cond
      (< times 0) (rotate-left (inc times) (concat (list (last lst)) (butlast lst)))
      (> times 0) (rotate-left (dec times) (concat (rest lst) (list (first lst))))
      :else lst)))

(deftest test-rotate-left
  (is (= () (rotate-left 5 ())))
  (is (= '(a b c d e f g) (rotate-left 0 '(a b c d e f g))))
  (is (= '(b c d e f g a) (rotate-left 1 '(a b c d e f g))))
  (is (= '(g a b c d e f) (rotate-left -1 '(a b c d e f g))))
  (is (= '(d e f g a b c) (rotate-left 3 '(a b c d e f g))))
  (is (= '(e f g a b c d) (rotate-left -3 '(a b c d e f g))))
  (is (= '(a b c d e f g) (rotate-left 7 '(a b c d e f g))))
  (is (= '(a b c d e f g) (rotate-left -7 '(a b c d e f g))))
  (is (= '(b c d e f g a) (rotate-left 8 '(a b c d e f g))))
  (is (= '(g a b c d e f) (rotate-left -8 '(a b c d e f g))))
  (is (= '(d e f g a b c) (rotate-left 45 '(a b c d e f g))))
  (is (= '(e f g a b c d) (rotate-left -45 '(a b c d e f g)))))

(defn binary
  "Returns the binary representation of a number."
  [n]
  (->> [() n]
       (iterate (fn [[r n]]
                  [(cons (rem n 2) r) (quot n 2)]))
       (drop-while (fn [[r n]]
                     (not= n 0)))
       first
       first))

(deftest test-binary
  (is (= () (binary 0)))
  (is (= '(1 1 1 1 0) (binary 30)))
  (is (= '(1 0 1 1 0 0 0 0 0 1 0 0 0 0 1 1) (binary 45123))))

(defn prime-factors
  "Returns in an array the prime factors of the number provided."
  ([number]
   (prime-factors number () 2))
  ([number primes divider]
    (if (= number 1)
        primes
        (if (= (rem number divider) 0)
          (prime-factors (/ number divider) (concat primes (list divider)) divider)
          (prime-factors number primes (inc divider))))))

(deftest test-prime-factors
  (is (= () (prime-factors 1)))
  (is (= '(2 3) (prime-factors 6)))
  (is (= '(2 2 2 2 2 3) (prime-factors 96)))
  (is (= '(97) (prime-factors 97)))
  (is (= '(2 3 3 37) (prime-factors 666))))

(defn gcd
  "Returns the greatest common divisor of a and b"
  [a b]
  (if (> b a)
    (gcd b a)
    (if (= b 0)
      a
      (gcd b (rem a b))
      )))

(deftest test-gcd
  (is (= 1 (gcd 13 7919)))
  (is (= 4 (gcd 20 16)))
  (is (= 6 (gcd 54 24)))
  (is (= 7 (gcd 6307 1995)))
  (is (= 12 (gcd 48 180)))
  (is (= 14 (gcd 42 56))))

(defn insert-everywhere
  "Returns a set of lists that have the value of x in every position in lst"
  [x lst]
  (->> (range (inc (count lst)))
       (map (fn [n]
              (concat (take n lst)
                      (list x)
                      (drop n lst))))))

(deftest test-insert-everywhere
  (is (= '((1)) (insert-everywhere 1 ())))
  (is (= '((1 a) (a 1)) (insert-everywhere 1 '(a))))
  (is (= '((1 a b c) (a 1 b c) (a b 1 c) (a b c 1))
         (insert-everywhere 1 '(a b c))))
  (is (= '((1 a b c d e)
           (a 1 b c d e)
           (a b 1 c d e)
           (a b c 1 d e)
           (a b c d 1 e)
           (a b c d e 1))
         (insert-everywhere 1 '(a b c d e))))
  (is (= '((x 1 2 3 4 5 6 7 8 9 10)
           (1 x 2 3 4 5 6 7 8 9 10)
           (1 2 x 3 4 5 6 7 8 9 10)
           (1 2 3 x 4 5 6 7 8 9 10)
           (1 2 3 4 x 5 6 7 8 9 10)
           (1 2 3 4 5 x 6 7 8 9 10)
           (1 2 3 4 5 6 x 7 8 9 10)
           (1 2 3 4 5 6 7 x 8 9 10)
           (1 2 3 4 5 6 7 8 x 9 10)
           (1 2 3 4 5 6 7 8 9 x 10)
           (1 2 3 4 5 6 7 8 9 10 x))
         (insert-everywhere 'x '(1 2 3 4 5 6 7 8 9 10)))))

(defn deep-reverse
  "Returns every element within the list completely flipped."
  [lst]
  (if (empty? lst) ()
  (->>
    (reverse lst)
    (map #(if (list? %)  (deep-reverse %) %)))))

(deftest test-deep-reverse
  (is (= () (deep-reverse ())))
  (is (= '(3 (d c b) a) (deep-reverse '(a (b c d) 3))))
  (is (= '(((6 5) 4) 3 (2 1))
         (deep-reverse '((1 2) 3 (4 (5 6)))))))

(defn pack
  [lst]
  (partition-by identity lst))

(deftest test-pack
  (is (= () (pack ())))
  (is (= '((a a a a) (b) (c c) (a a) (d) (e e e e))
         (pack '(a a a a b c c a a d e e e e))))
  (is (= '((1) (2) (3) (4) (5)) (pack '(1 2 3 4 5))))
  (is (= '((9 9 9 9 9 9 9 9 9)) (pack '(9 9 9 9 9 9 9 9 9)))))

(defn compress
  "Compresses the elements within a list so that there are no repetitions"
  [lst]
  (->> lst
    (pack)
      (mapcat #(distinct %))))

(deftest test-compress
  (is (= () (compress ())))
  (is (= '(a b c d) (compress '(a b c d))))
  (is (= '(a b c a d e)
         (compress '(a a a a b c c a a d e e e e))))
  (is (= '(a) (compress '(a a a a a a a a a a)))))

(defn encode
  "Encodes the repetitions of the list into vectors where the first element
  Is the number of times it repeats itself and the second is the element to be repeated"
  [lst]
  (->> lst
       (pack)
       (map #(vector (count %) (first %)))))

(deftest test-encode
  (is (= () (encode ())))
  (is (= '([4 a] [1 b] [2 c] [2 a] [1 d] [4 e])
         (encode '(a a a a b c c a a d e e e e))))
  (is (= '([1 1] [1 2] [1 3] [1 4] [1 5])
         (encode '(1 2 3 4 5))))
  (is (= '([9 9]) (encode '(9 9 9 9 9 9 9 9 9)))))

(defn encode-modified
  "Encodes the repetitions of the list into vectors where the first element
  Is the number of times it repeats itself and the second is the element to be repeated
  Unless this element appears only once, then it is only appended without the number of times it repeats."
  [lst]
  (->> lst
       (pack)
       (mapcat #(if (< 1 (count %))
                  (vector (vector (count %) (first %)))
                    %))))

(deftest test-encode-modified
  (is (= () (encode-modified ())))
  (is (= '([4 a] b [2 c] [2 a] d [4 e])
         (encode-modified '(a a a a b c c a a d e e e e))))
  (is (= '(1 2 3 4 5) (encode-modified '(1 2 3 4 5))))
  (is (= '([9 9]) (encode-modified '(9 9 9 9 9 9 9 9 9)))))

(defn decode
  "Decodes the value that the function encode-modified gives as an output"
  [lst]
  (->> lst
  (mapcat #(if (vector? %)
             (repeat (first %) (last %))
             (list %)))))

(deftest test-decode
  (is (= () (decode ())))
  (is (= '(a a a a b c c a a d e e e e)
         (decode '([4 a] b [2 c] [2 a] d [4 e]))))
  (is (= '(1 2 3 4 5) (decode '(1 2 3 4 5))))
  (is (= '(9 9 9 9 9 9 9 9 9) (decode '([9 9])))))

(run-tests)