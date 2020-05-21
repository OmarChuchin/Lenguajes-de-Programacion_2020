(defn getNil
  [k m]
  (nil? (get m k false)))

(defn f [s] (str "Hello, " s "!"))

(defn fa [lst] (first (drop (dec (count lst)) lst)))

(defn penlast [lst]
  (if (> 3 (count lst))
          (first lst)
          (penlast (rest lst))))

(defn nElement
  [lst pos]
  (if (< pos 1)
    (first lst)
    (nElement (rest lst) (dec pos))))

(defn altCount
  [data]
  (loop [size 0
         lst data]
    (if (nil? (first lst))
      size
      (recur (inc size) (rest lst)))))

(defn altReverse
  [lst]
  (loop [nlst []
         vct lst]
    (if (empty? vct)
      nlst
      (recur (conj nlst (last vct)) (butlast vct)))))

(defn fib
  [stop]
  (loop [arr [1 1]
         iter 2]
    (if (< iter stop)
      (recur (conj arr (reduce + (drop (- iter 2) arr))) (inc iter))
      arr)))

(defn palindrome
  [arr]
  (if (> 2 (count arr))
    true
    (if (= (first arr) (last arr))
      (palindrome (rest (butlast arr)))
      false)))

(defn flat
  [arr]
  (loop [result []
         lst arr]
    (let [val (first lst)]
      (cond
        (nil? val)  result
        (coll? val) (recur (concat result (flat val)) (rest lst))
        :else (recur (concat result (list val)) (rest lst))))))

(= (flat '((1 2) 3 [4 [5 6]])) '(1 2 3 4 5 6))

(defn maxValue
  [& arr]
  (loop [maxi nil
         lst arr]
    (let [value (first lst)]
      (cond
        (empty? lst) maxi
        (nil? maxi) (recur value (rest lst))
        (< maxi value) (recur value (rest lst))
        :else (recur maxi (rest lst))))))

(defn altInterleave
  [la lb]
  (loop [a la
         b lb
         result []
         bool true]
    (if bool
      (if (empty? a)
        result
        (recur (rest a) b (conj result (first a)) false))
      (if (< (count b) 2)
        (conj result (first b))
        (recur a (rest b) (conj result (first b)) true)))))

(defn interposeAlt
  [value arr]
  (loop [bool false
         lst arr
         result []]
    (if (empty? lst)
      result
      (if bool
        (recur false lst (conj result value))
        (recur true (rest lst) (conj result (first lst)))))))

(defn removeN
  [arr value]
  (let [n (dec value)]
    (loop [result []
           lst arr
           iter 0]
      (cond
        (empty? lst) result
        (< iter n) (recur (conj result (first lst))
                          (rest lst)
                          (inc iter))
        :else (recur result (rest lst) 0)))))

(defn factorial
  [value]
  (if (< value 2)
    1
    (* value (factorial (dec value)))))

(mapcat #(cons % (list %)) '(1 2 3 4)) ; Duplicate a sequence

(defn multiplySeq
  [vct times]
  (loop [result []
         lst vct
         iter 0]
    (cond
      (empty? lst) result
      (< iter times) (recur (conj result (first lst)) lst (inc iter))
      :else (recur result (rest lst) 0))))

(defn altRange
  [start end]
  (loop [result []
         n start]
    (if (< n end)
      (recur (conj result n) (inc n))
      result)))

(defn splitAt
  [lst at]
  (vector (take at lst) (drop at lst)))

(defn compress
  [lst]
  (loop [result [(first lst)]
         vct lst]
    (cond
      (empty? vct) result
      (= (last result) (first vct)) (recur result (rest vct))
      :else (recur (conj result (first vct)) (rest vct)))))

(defn dumb
  [top]
  (loop [result []
         n 1]
    (if (< n top)
      (recur (conj result n) (+ n 3))
      result)))

(partition-by identity [1 1 2 1 1 1])

(defn halfTruth
  [& bools]
  (< 1 (count (partition-by identity bools))))

(defn gcd
  [a b]
  (cond
    (< a b) (gcd b a)
    (= 0 (rem a b)) b
    :else (gcd b (rem a b))))

(defn createHashMap
  [vl vct]
  (loop [hsh {}
         lst vct]
    (if (empty? lst)
      hsh
      (recur (assoc hsh (first lst) vl) (rest lst)))))

(defn indexSeq
  [vct]
  (loop [lst vct
         iter 0
         result []]
    (if (empty? lst)
      result
      (recur (rest lst) (inc iter) (conj result [(first lst) iter])))))

(defn isCapital
  [c]
  (= (clojure.string/upper-case c) (str c)))

(defn getCaps
  [s]
  (->> s
       (seq)
       (map #(if (and (= (clojure.string/upper-case %) (str %)) (java.lang.Character/isLetter %)) %))
       (clojure.string/join)))

(defn triangle
  [l]
  (loop [stars 1
         space (dec l)]
    (if (> space -1)
      (do (print (clojure.string/join(repeat space " ")))
          (print (clojure.string/join(repeat stars "*")))
          (println)
          (recur (+ stars 2) (dec space))))))

(defn createMap
  [k v]
  (loop [keys k
         values v
         result {}]
    (if (or (nil? (first keys)) (nil? (first values)))
      result
      (recur (rest keys) (rest values) (assoc result (first keys) (first values))))))

(defn ittwo
  [f init] (lazy-seq (cons init (ittwo f (f init)))))

(defn commonValues
  [a b]
  (->> (map #(if (contains? b %) %) a)
       (remove nil?)
       (set)))

(defn getCrocs
  [f a b]
  (cond
    (f a b) :lt
    (f b a) :gt
    :else :eq))

(defn matMul
  [a b]
  (set (mapcat #(for [i a] (list i %)) b)))

(defn resultDigits
  [a b]
  (->> (* a b)
       (set)))

(defn dotProduct
  [a b]
  (loop [result 0
         a a
         b b]
    (if (nil? (first a))
      result
      (recur (+ result (* (first a) (first b))) (rest a) (rest b)))))

(defn symetricDiff
  [a b]
  (let [result #{}]
    (->> (map #(if (not (contains? b %)) (conj result %)) a)
         b
        (map #(if (not (contains? a %)) (conj result %)))
         result)))