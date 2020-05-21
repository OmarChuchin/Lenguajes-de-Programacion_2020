; Problem 1
; Jesús Omar Cuenca Espino A01378844
; Daniel Marcelo Zavala Hernández A01169414

(defn printBlock
  [length r]
  (do (println (clojure.string/join(repeat length "*")))
      (if (> r 1)
        (printBlock length (dec r)))))

(defn printBar
  [square thick times repetitions]
  (if (> repetitions 0)
    (do (loop [repetitionsAlt (inc times)]
          (if (> repetitionsAlt 1)
            (do (print (clojure.string/join(repeat thick "*")))
                (print (clojure.string/join(repeat square ".")))
                (recur (dec repetitionsAlt)))
            (do (println (clojure.string/join(repeat thick "*"))))))
        (printBar square thick times (dec repetitions)))))

(defn printFullBlock
  [t s n length r]
  (if (> r 0)
    (do (printBlock length t)
        (printBar s t n s)
        (printFullBlock t s n length (dec r)))))

(defn grid
  [s t n]
  (let [length (+ (* (+ s t) n) t)]
    (if (and (> s 0) (> t 0) (> n 0))
      (do (printFullBlock t s n length n)
          (printBlock length t)))))