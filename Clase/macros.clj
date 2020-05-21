;(->> (range 5)
;     (mapcat (fn [n] '(list n (nth )))))

(defn cond-helper
  [exprr]
  (->> (range (count exprr))
       (mapcat (fn [n] (list n (nth exprr n))))))

(defmacro nth-expr
  [n & exprr]
  `(cond ~n
         ~@(cond-helper exprr)
         (throw (RuntimeException "Bad nth value!"))))

(macroexpand-1 `(nth-expr (- 5 4) (* 2 3) (- 5 2) (+ 7 2) (/ 20 2)))



(defn helper
  "def many helper many"
  [lst](->> (partition 2 lst) (map (fn [lst2]
                                     `(def ~(symbol (str (first lst2) )) ~(second lst2))) ) ))

(defmacro def-many
  [& expressions]
  `(do ~@(helper expressions)))





(defn def-vars-helper
  [expressions]
  (->> (range (count expressions)) (mapcat (fn [n] `(~n ~(nth expressions n))))))

(defmacro nth-expr
  [n & expressions]

  `(case
     ~n
     ~@(def-vars-helper expressions)
     (throw (RuntimeException. "Bad nth value!"))))