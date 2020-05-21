(defprotocol Quiz6 (do-it [this x]))
(extend-type String Quiz6
  (do-it [this x] (* x (count this))))
(def q (reify Quiz6
         (do-it [this x] (* x x))))
(+ (do-it "hi" 5) (do-it q 2))

(defprotocol MyProtocol (m [x y]))
(deftype T1 [a b]
  MyProtocol
  (m [x y] (* (+ a b) y)))
(deftype T2 [c]
  MyProtocol
  (m [x y] (- c y)))
(def o1 (->T1 1 2))
(def o2 (->T2 3))

(- (m o1 (.b o1)) (m o2 (.c o2)))