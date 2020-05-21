; Problem 2
; Jesús Omar Cuenca Espino A01378844
; Daniel Marcelo Zavala Hernández A01169414

(defn associate
  [lst]
  (list (count lst) (first lst)))

(defn describe
  [lst]
  (->> lst
       (partition-by identity)
       (mapcat #(list (count %) (first %)))))

(defn conway
  ([]
   (conway '(1)))
  ([lst]
   (lazy-seq (cons lst (conway (describe lst))))))