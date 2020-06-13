;----------------------------------------------------------
; Problem Set #5
; Date: June 5, 2020.
; Authors:
;          A01378844 Omar Cuenca
;----------------------------------------------------------
(require '[clojure.test :refer [deftest is run-tests]])
(require '[clojure.core.logic :as logic])
(require '[clojure.core.logic.fd :as fd])

;DONE
(logic/defne removeo
             "Removes the first instance of the element x"
             [x lst result]
             ([h [h . t] t])
             ([x [h . t] result]
              (logic/fresh [temp]
                           (removeo x t temp)
                           (logic/conso h temp result))))

(deftest test-removeo
  (is (= [[:b :c :d :e]]
         (logic/run 1 [q]
                    (removeo :a [:a :b :c :d :e] q))))
  (is (= [[:a :b :d :e]]
         (logic/run 1 [q]
                    (removeo :c [:a :b :c :d :e] q))))
  (is (= [:d]
         (logic/run 1 [q]
                    (removeo q [:a :b :c :d :e] [:a :b :c :e]))))
  (is (= []
         (logic/run 1 [q]
                    (removeo :x [:a :b :c :d :e] q))))
  (is (= [[:x :a :b :c :d :e]
          [:a :x :b :c :d :e]
          [:a :b :x :c :d :e]
          [:a :b :c :x :d :e]
          [:a :b :c :d :x :e]
          [:a :b :c :d :e :x]]
         (logic/run 6 [q]
                    (removeo :x q [:a :b :c :d :e]))))
  (is (= [[:a [:b :c :d :e]]
          [:b [:a :c :d :e]]
          [:c [:a :b :d :e]]
          [:d [:a :b :c :e]]
          [:e [:a :b :c :d]]]
         (logic/run* [q1 q2]
                     (removeo q1 [:a :b :c :d :e] q2)))))

;----------------------------------------------------------------

;DONE
(logic/defne reverseo
             "Logical function that succeeds if the
             reverse of lst is result."
             [lst result]
             ([[] []])
             ([[h . t] result]
              (logic/fresh [temp]
                           (reverseo t temp)
                           (logic/appendo temp [h] result))))

(defn palindromeo
  "Logic Function that succeeds if and only if the value sent to it is a palindrome"
  [v]
  (reverseo v v))

(deftest test-palindromeo
  (is (= [:yes]
         (logic/run 1 [q]
                    (palindromeo [])
                    (logic/== q :yes))))
  (is (= [:yes]
         (logic/run 1 [q]
                    (palindromeo [:a])
                    (logic/== q :yes))))
  (is (= [:yes]
         (logic/run 1 [q]
                    (palindromeo [:a :b :c :b :a])
                    (logic/== q :yes))))
  (is (= []
         (logic/run 1 [q]
                    (palindromeo [:a :b :c :d])
                    (logic/== q :yes))))
  (is (= '[[]
           [_0]
           [_0 _0]
           [_0 _1 _0]
           [_0 _1 _1 _0]
           [_0 _1 _2 _1 _0]
           [_0 _1 _2 _2 _1 _0]]
         (logic/run 7 [q]
                    (palindromeo q)))))

;----------------------------------------------------------------

;DONE
(logic/defne rotateo
             "Logic expression that succeeds if result is the same as lst
             but with the first element of lst as the last element of result"
             [lst result]
             ([[h . t] temp]
              (logic/appendo t [h] temp)))

(deftest test-rotateo
  (is (= [:yes]
         (logic/run 1 [q]
                    (rotateo [:a :b :c :d :e]
                             [:b :c :d :e :a])
                    (logic/== q :yes))))
  (is (= []
         (logic/run 1 [q]
                    (rotateo [:a :b :c :d :e]
                             [:a :b :c :d :e])
                    (logic/== q :yes))))
  (is (= []
         (logic/run 1 [q]
                    (rotateo [] q))))
  (is (= [[:a]]
         (logic/run 1 [q]
                    (rotateo [:a] q))))
  (is (= [[:b :c :d :e :a]]
         (logic/run 1 [q]
                    (rotateo [:a :b :c :d :e] q))))
  (is (= [[:e :a :b :c :d]]
         (logic/run 1 [q]
                    (rotateo q [:a :b :c :d :e]))))
  (is (= '[[[_0] [_0]]
           [[_0 _1] [_1 _0]]
           [[_0 _1 _2] [_1 _2 _0]]
           [[_0 _1 _2 _3] [_1 _2 _3 _0]]
           [[_0 _1 _2 _3 _4] [_1 _2 _3 _4 _0]]
           [[_0 _1 _2 _3 _4 _5] [_1 _2 _3 _4 _5 _0]]
           [[_0 _1 _2 _3 _4 _5 _6] [_1 _2 _3 _4 _5 _6 _0]]]
         (logic/run 7 [q1 q2]
                    (rotateo q1 q2)))))

;----------------------------------------------------------------

;DONE
(declare evensizeo)
(logic/defne oddsizeo
             "Logic expression that succeeds if the lst has an odd length of elements"
             [lst]
             ([[x . t]]
              (evensizeo t)))

(logic/defne evensizeo
             "Logic expression that succeeds if the lst has an even number of elements"
             [lst]
             ([[]])
             ([[h . t]]
              (oddsizeo t)))

(deftest test-evensizeo-oddsizeo
  (is (= [:yes]
         (logic/run 1 [q]
                    (evensizeo [])
                    (logic/== q :yes))))
  (is (= [:yes]
         (logic/run 1 [q]
                    (oddsizeo [:x])
                    (logic/== q :yes))))
  (is (= []
         (logic/run 1 [q]
                    (evensizeo [:x])
                    (logic/== q :yes))))
  (is (= []
         (logic/run 1 [q]
                    (oddsizeo [])
                    (logic/== q :yes))))
  (is (= [:yes]
         (logic/run 1 [q]
                    (evensizeo [:a :b :c :d :e :f])
                    (logic/== q :yes))))
  (is (= [:yes]
         (logic/run 1 [q]
                    (oddsizeo [:a :b :c :d :e])
                    (logic/== q :yes))))
  (is (= '[[]
           [_0 _1]
           [_0 _1 _2 _3]
           [_0 _1 _2 _3 _4 _5]
           [_0 _1 _2 _3 _4 _5 _6 _7]]
         (logic/run 5 [q]
                    (evensizeo q))))
  (is (= '[[_0]
           [_0 _1 _2]
           [_0 _1 _2 _3 _4]
           [_0 _1 _2 _3 _4 _5 _6]
           [_0 _1 _2 _3 _4 _5 _6 _7 _8]]
         (logic/run 5 [q]
                    (oddsizeo q)))))

;----------------------------------------------------------------

;DONE
(logic/defne splito
  "Function that succeeds if the result of adding the left & the right elements one by one results in the result list
  Starting by the left list"
  [result left right]
  ([[] [] []])
  ([[x] [x] []])
  ([[x y . t] [x . temp1] [y . temp2]]
   (splito t temp1 temp2)))

(deftest test-splito
  (is (= [:yes]
         (logic/run 1 [q]
                    (splito [] [] [])
                    (logic/== q :yes))))
  (is (= [:yes]
         (logic/run 1 [q]
                    (splito [:a] [:a] [])
                    (logic/== q :yes))))
  (is (= [:yes]
         (logic/run 1 [q]
                    (splito [:a :b] [:a] [:b])
                    (logic/== q :yes))))
  (is (= [:yes]
         (logic/run 1 [q]
                    (splito [:a :b :c :d :e :f]
                            [:a :c :e]
                            [:b :d :f])
                    (logic/== q :yes))))
  (is (= [:yes]
         (logic/run 1 [q]
                    (splito [:a :b :c :d :e :f :g]
                            [:a :c :e :g]
                            [:b :d :f])
                    (logic/== q :yes))))
  (is (= [[[:a :c :e] [:b :d :f]]]
         (logic/run 1 [q1 q2]
                    (splito [:a :b :c :d :e :f] q1 q2))))
  (is (= [[:a :b :c :d :e :f :g]]
         (logic/run 1 [q]
                    (splito q [:a :c :e :g] [:b :d :f]))))
  (is (= '[[[] [] []]
           [[_0] [_0] []]
           [[_0 _1] [_0] [_1]]
           [[_0 _1 _2] [_0 _2] [_1]]
           [[_0 _1 _2 _3] [_0 _2] [_1 _3]]
           [[_0 _1 _2 _3 _4] [_0 _2 _4] [_1 _3]]
           [[_0 _1 _2 _3 _4 _5] [_0 _2 _4] [_1 _3 _5]]]
         (logic/run 7 [q1 q2 q3]
                    (splito q1 q2 q3)))))

;----------------------------------------------------------------

;DONE
(declare equalo)
(logic/defne equalo-helper
             "Helps the logical expression equalo to check if all the elements in a list are the same"
             [v lst]
             ([x [x]]);casos base
             ([v [v . t]]
              (equalo-helper v t)))

(logic/defne equalo
             "Logic expression that succeeds if every single element in the list is the same"
             [lst]
             ([[]])
             ([[x]])
             ([[h . t]]
              (equalo-helper h t)))

(deftest test-equalo
  (is (= [:yes]
         (logic/run 1 [q]
                    (equalo [])
                    (logic/== q :yes))))
  (is (= [:yes]
         (logic/run 1 [q]
                    (equalo [:x])
                    (logic/== q :yes))))
  (is (= [:yes]
         (logic/run 1 [q]
                    (equalo [:x :x])
                    (logic/== q :yes))))
  (is (= [:yes]
         (logic/run 1 [q]
                    (equalo [:x :x :x :x :x])
                    (logic/== q :yes))))
  (is (= [:x]
         (logic/run 1 [q]
                    (equalo [:x :x q :x]))))
  (is (= '[_0]
         (logic/run 1 [q]
                    (equalo [q q q q q q]))))
  (is (= '([_0 _0 _0 _0 _0])
         (logic/run 1 [q1 q2 q3 q4 q5]
                    (equalo [q1 q2 q3 q4 q5]))))
  (is (= []
         (logic/run 1 [q]
                    (equalo [:x :y])
                    (logic/== q :yes))))
  (is (= []
         (logic/run 1 [q1 q2]
                    (equalo [q1 q1 q2 q1 q1])
                    (logic/!= q1 q2))))
  (is (= '([]
           [_0]
           [_0 _0]
           [_0 _0 _0]
           [_0 _0 _0 _0]
           [_0 _0 _0 _0 _0]
           [_0 _0 _0 _0 _0 _0])
         (logic/run 7 [q]
                    (equalo q)))))

;----------------------------------------------------------------

;DONE
(logic/defne counto
             "Logic expression that succeeds if counter is the length of the lst"
             [lst counter]
             ([[] 0])
             ([[h . t] result]
              (logic/fresh [temp]
                           (counto t temp)
                           (fd/+ 1 temp result))))

(deftest test-counto
  (is (= [0]
         (logic/run 1 [q]
                    (fd/in q (fd/interval 0 10))
                    (counto [] q))))
  (is (= [1]
         (logic/run 1 [q]
                    (fd/in q (fd/interval 0 10))
                    (counto [:a] q))))
  (is (= [2]
         (logic/run 1 [q]
                    (fd/in q (fd/interval 0 10))
                    (counto [:a :b] q))))
  (is (= [3]
         (logic/run 1 [q]
                    (fd/in q (fd/interval 0 10))
                    (counto [:a :b :c] q))))
  (is (= [10]
         (logic/run 1 [q]
                    (fd/in q (fd/interval 0 10))
                    (counto (repeat 10 :x) q))))
  (is (= '([_0])
         (logic/run 1 [q]
                    (fd/in q (fd/interval 0 10))
                    (counto q 1))))
  (is (= '([_0 _1 _2 _3 _4])
         (logic/run 1 [q]
                    (fd/in q (fd/interval 0 10))
                    (counto q 5))))
  (is (= '([[] 0]
           [(_0) 1]
           [(_0 _1) 2]
           [(_0 _1 _2) 3]
           [(_0 _1 _2 _3) 4]
           [(_0 _1 _2 _3 _4) 5]
           [(_0 _1 _2 _3 _4 _5) 6])
         (logic/run 7 [q1 q2]
                    (fd/in q1 q2 (fd/interval 0 10))
                    (counto q1 q2)))))

;----------------------------------------------------------------

;DONE
(logic/defne facto
             "Logic expression that succeeds if the value of result is the factorial of a"
             [a result]
             ([0 1])
             ([x r]
              (logic/fresh [y temp]
                           (fd/- x 1 y)
                           (facto y temp)
                           (fd/* x temp r))))

(deftest test-facto
  (is (= [1]
         (logic/run 1 [q]
                    (facto 0 q))))
  (is (= [1]
         (logic/run 1 [q]
                    (facto 1 q))))
  (is (= [720]
         (logic/run 1 [q]
                    (facto 6 q))))
  (is (= [2432902008176640000]
         (logic/run 1 [q]
                    (facto 20 q))))
  (is (= [0 1]
         (logic/run 2 [q]
                    (facto q 1))))
  (is (= [5]
         (logic/run 1 [q]
                    (facto q 120))))
  (is (= [10]
         (logic/run 1 [q]
                    (facto q 3628800))))
  (is (= [:yes]
         (logic/run 1 [q]
                    (facto 4 24)
                    (logic/== q :yes))))
  (is (= [:yes]
         (logic/run 1 [q]
                    (facto 15 1307674368000)
                    (logic/== q :yes))))
  (is (= [[0 1]
          [1 1]
          [2 2]
          [3 6]
          [4 24]
          [5 120]
          [6 720]
          [7 5040]
          [8 40320]
          [9 362880]]
         (logic/run 10 [n r]
                    (facto n r)))))

;----------------------------------------------------------------

;DONE
(logic/defne powo
             "Logic expression that succeeds if the r is the result of b to the power p"
             [b p r]
             ([x 0 1])
             ([x y oper]
              (logic/fresh [alfa beta]
                           (fd/in alfa beta (fd/interval 0 100))
                           (fd/- y 1 alfa);exponente -1
                           (powo x alfa beta)
                           (fd/* x beta oper))))

(deftest test-powo
  (is (= [:yes]
         (logic/run 1 [q]
                    (powo 3 2 9)
                    (logic/== q :yes))))
  (is (= [32]
         (logic/run 1 [q]
                    (powo 2 5 q))));este falla
  (is (= [5]
         (logic/run 1 [q]
                    (powo q 2 25))))
  (is (= [3]
         (logic/run 1 [q]
                    (powo 2 q 8))))
  (is (= [1]
         (logic/run 1 [q]
                    (powo q q q))))
  (is (= #{[64 1] [8 2] [4 3] [2 6]}
         (set
           (logic/run* [a b]
                       (powo a b 64)))));este falla
  (is (= '[_0]
         (logic/run 1 [q]
                    (powo q 0 1))))
  (is (= (set (range 101))
         (set
           (logic/run* [q]
                       (fd/in q (fd/interval 0 100))
                       (powo q 1 q))))))

;----------------------------------------------------------------

;DONE
(logic/defne rangeo
             "logic expression that succeeds if the result is the array of numbers from begin to end inclusive"
             [begin end result]
             ([a b []]
              (fd/> a b))
             ([a b result]
              (fd/<= a b)
              (logic/fresh [c temp]
                           (fd/+ a 1 c)
                           (rangeo c b temp)
                           (logic/conso a temp result))))

(deftest test-rangeo
  (is (= [[3 4 5 6 7 8 9 10]]
         (logic/run 1 [q]
                    (rangeo 3 10 q))))
  (is (= [[7]]
         (logic/run 1 [q]
                    (rangeo 7 7 q))))
  (is (= [[]]
         (logic/run 1 [q]
                    (rangeo 10 1 q))))
  (is (= [6]
         (logic/run 1 [q]
                    (fd/in q (fd/interval 1 10))
                    (rangeo 2 q [2 3 4 5 6]))))
  (is (= [[2 6]]
         (logic/run 1 [q1 q2]
                    (fd/in q1 q2 (fd/interval 1 10))
                    (rangeo q1 q2 [2 3 4 5 6]))))
  (is (= #{[]
           [1] [1 2] [1 2 3] [1 2 3 4]
           [2] [2 3] [2 3 4]
           [3] [3 4]
           [4]}
         (set
           (logic/run* [q]
                       (logic/fresh [start end]
                                    (fd/in start end (fd/interval 1 4))
                                    (rangeo start end q)))))))

(run-tests)