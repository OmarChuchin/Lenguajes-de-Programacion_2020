;----------------------------------------------------------
; Problem Set #1
; Date: March 03, 2020.
; Authors:
;          A01378844    Jesus Omar Cuenca Espino
;----------------------------------------------------------

(require '[clojure.test :refer [deftest is run-tests]])
(require '[clojure.math.numeric-tower :refer [sqrt expt]])
(require '[clojure.math.numeric-tower :refer [abs]])
(use 'clojure.set)

;;A partir de aqui es la tarea
(defn !
    "The factorial operation"
    [x]
    (if (< x 2)
      1
      (*' x (! (-' x 1)))))

(deftest test-!
    (is (= 1
            (! 0)))
    (is (= 120
            (! 5)))
    (is (= '(1 1 2 6 24 120 720 5040 40320 362880 3628800)
            (map ! (range 11))))
    (is (= 15511210043330985984000000N
            (! 25)))
    (is (= 815915283247897734345611269596115894272000000000N
            (! 40))))

(defn duplicate
    [lst]
    (mapcat #(list % %) lst))

(deftest test-duplicate
    (is (= '(1 1 2 2 3 3 4 4 5 5)
            (duplicate '(1 2 3 4 5))))
    (is (= ()
            (duplicate ())))
    (is (= '(a a)
            (duplicate '(a))))
    (is (= '(a a b b c c d d e e f f g g h h)
            (duplicate '(a b c d e f g h)))))
  
(defn pow
    "The power operation"
    [x y]
    (if (< y 1)
        1
        (* x (pow x (- y 1)))))

(deftest test-pow
    (is (= 1 (pow 0 0)))
    (is (= 0 (pow 0 1)))
    (is (= 1 (pow 5 0)))
    (is (= 5 (pow 5 1)))
    (is (= 125 (pow 5 3)))
    (is (= 25 (pow -5 2)))
    (is (= -125 (pow -5 3)))
    (is (= 1024 (pow 2 10)))
    (is (= 525.21875 (pow 3.5 5)))
    (is (= 129746337890625 (pow 15 12)))
    (is (= 3909821048582988049 (pow 7 22))))
            
(defn fib
    "Formula to calculate the n value of the fibonacci sequence"
    [n]
    (if (< n 2)
        n
        (+ (fib (- n 1)) (fib (- n 2)))))
; Usar para acelerar el proceso de calculo de valores.
(def fib (memoize fib))

(deftest test-fib
    (is (= 0
            (fib 0)))
    (is (= 1
            (fib 1)))
    (is (= 1
            (fib 2)))
    (is (= 5
            (fib 5)))
    (is (= '(0 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610
                987 1597 2584 4181 6765)
            (map fib (range 21))))
    (is (= 267914296
            (fib 42))))

(defn enlist
    "Returns a list of lists that contains all the elements of the list"
    [lst]
    (if (empty? lst)
        ()
        (cons (list (first lst)) (enlist (rest lst)))))

(deftest test-enlist
    (is (= () (enlist ())))
    (is (= '((a) (b) (c)) (enlist '(a b c))))
    (is (= '(((1 2 3)) (4) ((5)) (7) (8))
            (enlist '((1 2 3) 4 (5) 7 8)))))

(defn positives
    "Returns a list of all the positive elements of this array"
    [lst]
    (if (empty? lst)
        '()
        (if (>= (first lst) 0)
        (cons (first lst) (positives (rest lst)))
        (positives (rest lst)))))

(deftest test-positives
    (is (= () (positives '())))
    (is (= () (positives '(-4 -1 -10 -13 -5))))
    (is (= '(3 6) (positives '(-4 3 -1 -10 -13 6 -5))))
    (is (= '(4 3 1 10 13 6 5) (positives '(4 3 1 10 13 6 5)))))

(defn add-list
    "Adds all the elements in the list"
    [lst]
    (if (empty? lst)
        0
        (+ (first lst) (add-list (rest lst)))))

(deftest test-add-list
    (is (= 0 (add-list ())))
    (is (= 10 (add-list '(2 4 1 3))))
    (is (= 55 (add-list '(1 2 3 4 5 6 7 8 9 10)))))

(defn invert-pairs
    "Inverts all the pairs of vectors in the list"
    [lst]
    (if (empty? lst)
        ()
        (cons (vec (list (first (rest (first lst))) (first (first lst)))) (invert-pairs (rest lst)))
        ))

(deftest test-invert-pairs
    (is (= () (invert-pairs ())))
    (is (= '([1 a][2 a][1 b][2 b]))
            (invert-pairs '([a 1][a 2][b 1][b 2])))
    (is (= '([1 January][2 February][3 March])
            (invert-pairs '([January 1][February 2][March 3])))))

(defn list-of-symbols?
    "Checks if every element of the list is a symbol"
    [lst]
    (if (empty? lst)
        true
        (if (symbol? (first lst))
            (list-of-symbols? (rest lst))
            false)))

(deftest test-list-of-symbols?
    (is (list-of-symbols? ()))
    (is (list-of-symbols? '(a)))
    (is (list-of-symbols? '(a b c d e)))
    (is (not (list-of-symbols? '(a b c d 42 e))))
    (is (not (list-of-symbols? '(42 a b c)))))

(defn swapper
    "Exchanges every occurrence of the first parameter with the second and viceversa in the array given to me in the third parameter"
    [a b lst]
    (if (empty? lst)
        ()
        (cond 

        (= a (first lst))
            (concat (list b) (swapper a b (rest lst)))

        (= b (first lst))
            (concat (list a) (swapper a b (rest lst)))

        :else
            (concat (list (first lst)) (swapper a b (rest lst)))
        ))
)

(deftest test-swapper
    (is (= ()
            (swapper 1 2 ())))
    (is (= '(4 3 4 9 9 3 3 3 9 9 7 9
                3 7 8 7 8 4 5 6)
            (swapper 1 2 [4 3 4 9 9 3 3 3 9 9 7
                            9 3 7 8 7 8 4 5 6])))
    (is (= '(4 4 5 1 4 8 1 5 6 4 5 2 9 5 9 9 2 1 1 4)
            (swapper 1 2 [4 4 5 2 4 8 2 5 6 4 5
                            1 9 5 9 9 1 2 2 4])))
    (is (= '(soft purr warm purr little ball of fur
                happy purr sleepy purr kitty kitty kitty)
            (swapper 'purr
                    'kitty
                    '(soft kitty warm kitty little ball
                        of fur happy kitty sleepy kitty
                        purr purr purr)))))

(defn dot-product
    "Returns the dot product of two matrix 1xn"
    [a b]
    (if (empty? a)
        0
        (+ (* (first a) (first b)) (dot-product (rest a) (rest b)))))

(deftest test-dot-product
(is (= 0 (dot-product () ())))
(is (= 32 (dot-product '(1 2 3) '(4 5 6))))
(is (= 21.45 (dot-product '(1.3 3.4 5.7 9.5 10.4)
                            '(-4.5 3.0 1.5 0.9 0.0)))))

(defn average
    "Calculates the average of a list of numbers"
    [x]
    (if (empty? x)
        nil
        (/ (add-list x) (count x))))
            
    (deftest test-average
    (is (nil? (average ())))
    (is (= 4
            (average '(4))))
    (is (= 3
            (average '(5 6 1 6 0 1 2))))
    (is (= 2.5
            (average '(1.7 4.5 0 2.0 3.4 5 2.5 2.2 1.2)))))

(defn standard-deviation
    "Calculates the standard deviation of a list of numbers"
    ([lst]
        (if (empty? lst)
            nil
            (standard-deviation lst (average lst) 0 (count lst))))
    ([lst avg sum size]
        (if (empty? lst)
            (sqrt (/ sum size))
            (standard-deviation (rest lst) 
                                avg 
                                (+' sum (pow (-' (first lst) avg) 2))
                                size))))

(defn aprox=
    "Checks if x is approximately equal to y. Returns true
    if |x - y| < epsilon, or false otherwise."
    [epsilon x y]
    (< (abs (- x y)) epsilon))

(deftest test-standard-deviation
    (is (nil? (standard-deviation ())))
    (is (approx= 0.01
                1.87
                (standard-deviation
                    '(6 2 3 1))))
    (is (approx= 0.0001
                12.3153
                (standard-deviation
                    '(4 8 15 16 23 42))))
    (is (approx= 0.00001
                7.07106
                (standard-deviation
                    '(110 105 90 100 95))))
    (is (approx= 0.001
                2.983
                (standard-deviation
                    '(9 2 5 4 12 7 8 11
                        9 3 7 4 12 5 4 10
                        9 6 9 4)))))

(run-tests)