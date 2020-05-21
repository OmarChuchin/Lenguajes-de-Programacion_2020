;----------------------------------------------------------
; Problem Set #3
; Date: Abril 1, 2020.
; Authors:
;          A01378844 Jesus Omar Cuenca Espino
; I am really sorry for the delay.
;----------------------------------------------------------

(require '[clojure.test :refer [deftest is run-tests]])
(require '[clojure.math.numeric-tower :refer [abs]])

(defn aprox=
  "Checks if x is approximately equal to y. Returns true
  if |x - y| < epsilon, or false otherwise."
  [epsilon x y]
  (< (abs (- x y)) epsilon))

(defn argswap
  "
  This function returns the exact same function passed to it as a parameter.
  But changes the order that the parameters where given to it.
  "
  [f]
  (fn [x y]
    (f y x)))

(deftest test-argswap
  (is (= '(2 1)
         ((argswap list) 1 2)))
  (is (= -7
         ((argswap -) 10 3)))
  (is (= 1/4
         ((argswap /) 8 2)))
  (is (= '((4 5 6) 1 2 3)
         ((argswap cons) '(1 2 3) '(4 5 6))))
  (is (= '(1 0 4 25 100)
         ((argswap map) '(-1 0 2 5 10) #(* % %)))))

(defn there-exists-one
  "
  Return true if within the list the function f given to exists a value that returned true
  Otherwise it returns false.
  "
  [f lst]
  (if (empty? lst)
      false
      (if (f (first lst))
        true
        (there-exists-one f (rest lst)))))

(deftest test-there-exists-one
  (is (not (there-exists-one pos?
                             ())))
  (is (there-exists-one pos?
                        '(-1 -10 4 -5 -2 -1)))
  (is (there-exists-one neg?
                        '(-1)))
  (is (not (there-exists-one symbol?
                             '(4 8 15 16 23 42))))
  (is (there-exists-one symbol?
                        '(4 8 15 sixteen 23 42))))

;(defn bisection
;  [a b f]
;  (loop [a a
;         b b
;         c (/ (+ a b) 2)]
;    (if (aprox= 0.00001 0 (f c))
;      c
;      (if (< 0 (/ (f a) (f c)))
;        (recur a c (/ (+ a c) 2))
;        (recur c b (/ (+ c b) 2))))))
;
;(deftest test-bisection
;  (is (aprox= 0.0001
;              3.0
;              (bisection 1 4 (fn [x] (* (- x 3) (+ x 4))))))
;  (is (aprox= 0.0001
;              -4.0
;              (bisection -5 0 (fn [x] (* (- x 3) (+ x 4))))))
;  (is (aprox= 0.0001
;              Math/PI
;              (bisection 1 4 (fn [x] (Math/sin x)))))
;  (is (aprox= 0.0001
;              (* 2 Math/PI)
;              (bisection 5 10 (fn [x] (Math/sin x)))))
;  (is (aprox= 0.0001
;              1.618033988749895
;              (bisection 1 2 (fn [x] (- (* x x) x 1)))))
;  (is (aprox= 0.0001
;              -0.6180339887498948
;              (bisection -10 1 (fn [x] (- (* x x) x 1))))))

(defn linear-search
  "
  Iterates through the list evaluating the function f over each of it's element looking for
  the value 'value'.
  If it finds it, it returns the position where the value is.
  Otherwise returns nil.
  "
  [lst value f]
  (loop [position 0
         lst lst]
    (if (empty? lst)
      nil
      (if (f (first lst) value)
        position
        (recur (inc position) (rest lst))))))

(deftest test-linear-search
  (is (nil? (linear-search [] 5 =)))
  (is (= 0 (linear-search [5] 5 =)))
  (is (= 4 (linear-search
             [48 77 30 31 5 20 91 92
              69 97 28 32 17 18 96]
             5
             =)))
  (is (= 3 (linear-search
             ["red" "blue" "green" "black" "white"]
             "black"
             identical?)))
  (is (nil? (linear-search
              [48 77 30 31 5 20 91 92
               69 97 28 32 17 18 96]
              96.0
              =)))
  (is (= 14 (linear-search
              [48 77 30 31 5 20 91 92
               69 97 28 32 17 18 96]
              96.0
              ==)))
  (is (= 8 (linear-search
             [48 77 30 31 5 20 91 92
              69 97 28 32 17 18 96]
             70
             #(<= (abs (- %1 %2)) 1)))))

(defn deriv
  "
  Returns a function that would serve as the derivate of the function f,
  given to it as a parameter, with the level of precision of h.
  "
  [f h]
  (fn [a]
    (/ (- (f (+ a h)) (f a)) h)))

(defn f [x] (* x x x))
(def df (deriv f 0.001))
(def ddf (deriv df 0.001))
(def dddf (deriv ddf 0.001))

(deftest test-deriv
  (is (aprox= 0.05 75 (df 5)))
  (is (aprox= 0.05 30 (ddf 5)))
  (is (aprox= 0.05 6 (dddf 5))))

(defn newton
  "
  Newtons way of finding the roots of a function.
  "
  [f times]
  (let [div (deriv f 0.0001)]
    (loop [iter 0
           value 0]
      (if (< iter times)
        (recur (inc iter) (- value (/ (f value) (div value))))
        value))
    ))

(deftest test-newton
  (is (aprox= 0.00001
              10.0
              (newton (fn [x] (- x 10))
                      1)))
  (is (aprox= 0.00001
              -0.5
              (newton (fn [x] (+ (* 4 x) 2))
                      1)))
  (is (aprox= 0.00001
              -1.0
              (newton (fn [x] (+ (* x x x) 1))
                      50)))
  (is (aprox= 0.00001
              -1.02987
              (newton (fn [x] (+ (Math/cos x)
                                 (* 0.5 x)))
                      5))))

(defn integral
  "Using Simpson's Rule of 3 you can accurately calculate the value of the integral of f"
  [a b n f]
  (let [h (/ (- b a) n)]
    (loop [iter 0
           sum 0]
      (if (<= iter n)
        (cond
          (or (= iter 0) (= iter n)) (recur (inc iter) (+ sum (f (+ a (* iter h)))))
          (odd? iter) (recur (inc iter) (+ sum (* 4 (f (+ a (* iter h))))))
          (even? iter) (recur (inc iter) (+ sum (* 2 (f (+ a (* iter h)))))))
        (* (/ h 3) sum)))))

(deftest test-integral
  (is (= 1/4 (integral 0 1 10 (fn [x] (* x x x)))))
  (is (= 21/4
         (integral 1 2 10
                   (fn [x]
                     (integral 3 4 10
                               (fn [y]
                                 (* x y))))))))

;(defn binary-search
;  ""
;  [lst data f]
;  (loop [position (quot (count lst) 2)
;         vct lst]
;    (let [half (quot (count vct) 2)
;          value (first (drop half vct))]
;      (cond
;        (empty? vct) nil
;        (= data value) position
;        (f data value) (recur (- position (dec half)) (take half vct))
;        :else (recur (+ position half) (drop half vct))))))
;
;(binary-search '(1 2 3 4) 5 <)
;
;(def small-list [4 8 15 16 23 42])
;
;(def big-list [0 2 5 10 11 13 16 20 24 26
;               29 30 31 32 34 37 40 43 44
;               46 50 53 58 59 62 63 66 67
;               70 72 77 79 80 83 85 86 94
;               95 96 99])
;
;(def animals ["dog" "dragon" "horse" "monkey" "ox"
;              "pig" "rabbit" "rat" "rooster" "sheep"
;              "snake" "tiger"])
;(defn str<
;  "Returns true if a is less than b, otherwise
;   returns false. Designed to work with strings."
;  [a b]
;  (< (compare a b) 0))
;
;(nil? (binary-search [] 5 <))
;(binary-search small-list 16 <)
;(= 0 (binary-search small-list 4 <))
;
;(deftest test-binary-search
;  (is (nil? (binary-search [] 5 <)))
;  (is (= 3 (binary-search small-list 16 <)))
;  (is (= 0 (binary-search small-list 4 <)))
;  (is (= 5 (binary-search small-list 42 <)))
;  (is (nil? (binary-search small-list 7 <)))
;  (is (nil? (binary-search small-list 2 <)))
;  (is (nil? (binary-search small-list 99 <)))
;  (is (= 17 (binary-search big-list 43 <)))
;  (is (= 0 (binary-search big-list 0 <)))
;  (is (= 39 (binary-search big-list 99 <)))
;  (is (nil? (binary-search big-list 12 <)))
;  (is (nil? (binary-search big-list -1 <)))
;  (is (nil? (binary-search big-list 100 <)))
;  (is (= 5 (binary-search animals "pig" str<)))
;  (is (= 0 (binary-search animals "dog" str<)))
;  (is (= 11 (binary-search animals "tiger" str<)))
;  (is (nil? (binary-search animals "elephant" str<)))
;  (is (nil? (binary-search animals "alligator" str<)))
;  (is (nil? (binary-search animals "unicorn" str<))))

(run-tests)
