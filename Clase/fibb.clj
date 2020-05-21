(use clojure.core.logic)
(defn fibs
  ([]
   (fibs 0 1))
  ([a b]
   (lazy-seq (cons a (fibs b (+' a b))))))

(take 6 (fibs))

(run* [q]
      (fresh [a b]
             (fd/in a b (fd/interval 0 5))
             (fd/< a b)
             (fd/+ a b 5)
             (== q [a b])))