;----------------------------------------------------------
; Activity: Problem Set #4
; Date: May 22, 2020.
; Authors:
;          A01378844 Omar Cuenca
;----------------------------------------------------------

(defmacro my-or
  "My own version of the or macro."
  ([] nil)
  ([x] x)
  ([x & next]
   `(let [temp# ~x]
      (if temp#
        temp#
        (my-or ~@next)))))

;TEST CASES
(macroexpand-1 '(my-or nil nil true false nil :metal))
(my-or nil nil true false nil :metal)

;-------------------------------------------------------------------------------------------

(defmacro do-loop
  "Looping macro that behaves different depending on the type of loop desired"
  [& args]
  (let [type      (first    (last args))
        condition (last     (last args))
        body      (butlast  args)]
    (if (= :while type)
      `(while ~condition (do ~@body));basic while loop
      `(loop [];UNTIL loop which iterates until the condition is satisfied.
         ~@body
         (if (not ~condition)
           (recur))))))

;TEST CASES
(def i (atom 0))
(do-loop
  (println @i)
  (swap! i inc)
  (:until (= @i 5)))
(def j (atom 1))
(macroexpand-1 `(do-loop
                  (println @j)
                  (swap! j inc)
                  (:until (> @j 15))))
(do-loop
  (println @j)
  (swap! j inc)
  (:until (> @j 15)))
(def j (atom 1))
(do-loop
  (println @j)
  (swap! j inc)
  (:while (<= @j 5)))

;--------------------------------------------------------------------------------------------

(defmacro def-pred
  "Macro that defines a function and its anti-function that evaluates a same variables & does the same stuff
  But returns it's negative form."
  [fun variable & expression]
  `(do  (defn ~fun [~@variable] (do ~@expression))
        (defn ~(symbol (str 'not- fun)) [~@variable] (not (do ~@expression)))))

;TEST CASES
(macroexpand-1 '(def-pred less-than-one? [x] (< x 1)))

(def-pred less-than-one? [x] (< x 1))

(less-than-one? 0)
;⇒ true
(less-than-one? 2)
;⇒ false
(not-less-than-one? 0)
;⇒ false
(not-less-than-one? 2)
;⇒ true

(macroexpand-1 '(def-pred plural? [s]
                          (println "check s in" s)
                          (= \s (last s))))

(def-pred plural? [s] (println "check s in" s) (= \s (last s)))

(plural? "boys")
;;; prints:
;;; check s in boys
;⇒ true
(plural? "girl")
;;; prints:
;;; check s in girl
;⇒ false
(not-plural? "boys")
;;; prints:
;;; check s in boys
;⇒ false
(not-plural? "girl")
;;; prints:
;;; check s in girl
;⇒ true
;--------------------------------------------------------------------------------------------

(defn curry-helper
  "Function that recursively creates functions passing down params of list one by one until it runs out of
  params to give, and then just executes the instructions contained in body."
  [lst body]
  (if (empty? lst)
    `(do ~@body)
    `(fn [~(first lst)] ~(curry-helper (rest lst) body))))

(defmacro defn-curry
  "
  Macro that takes a name of a function to be created and some args, then recursively creates functions that receives
  all the args given to the original function one by one using its respective helper.
  Finally when there are no more args to create other functions, the last function created executes the code within
  body.
  "
  [name args & body]
  (if (empty? args)
    `(defn ~name []
       (do ~@body))
    `(defn ~name [~(first args)]
       ~(curry-helper (rest args) body))))


(macroexpand-1 '(defn-curry sum [a b c d]
                            (prn 'args a b c d)
                            (+ a b c d)))

(defn-curry sum
            [a b c d]
            (prn 'args a b c d)
            (+ a b c d))

((((sum 1) 2) 3) 4)
;;; prints:
;;; args 1 2 3 4
;⇒ 10

((((sum 15) 8) 16) 42)
;;; prints:
;;; args 15 8 16 42
;⇒ 81

(macroexpand-1 '(defn-curry add1 [x] (+ x 1)))
(defn-curry add1 [x] (+ x 1))

(add1 0); ⇒ 1

(add1 41); ⇒ 42

(macroexpand-1 '(defn-curry hello [] "hello"))
;⇒ (clojure.core/defn hello [] (do "hello"))

(defn-curry hello [] "hello")

(hello) ;⇒ "hello"

;--------------------------------------------------------------------------------------------

(defn clean-clauses
  "Function that drops elements found before the key & takes everything else until it finds the antiKey or
  the list is emptied."
  [key antiKey clauses]
  (->>
    (drop-while #(not= % key) clauses)
    (rest)
    (take-while #(not= % antiKey))))

;Function test cases
;(clean-clauses :ELSE :THEN '(:ELSE (println "Else section.") 'oops :THEN (println "Then section.") 'ok))
;(clean-clauses :THEN :ELSE '(:ELSE (println "Else section.") 'oops :THEN (println "Then section.") 'ok))
;(clean-clauses :THEN :ELSE '(:THEN :ELSE (println "Else section.") 'oops))

(defmacro IF
  "
  Similar to Clojure's if macro, that takes a condition and clauses declared as :THEN and/or :ELSE
  which are separated within an if clause in the correct place.
  "
  [condition & clauses]
  `(if ~condition
     (do ~@(clean-clauses :THEN :ELSE clauses));If the condition is satisfied
     (do ~@(clean-clauses :ELSE :THEN clauses))));If the condition is not satisfied

;Macro test cases
(macroexpand-1 '(IF (> 3 1)
                    :ELSE (println "Else section.") 'oops
                    :THEN (println "Then section.") 'ok))
(IF (> 3 1)
    :ELSE (println "Else section.") 'oops
    :THEN (println "Then section.") 'ok)

(macroexpand-1 '(IF (< 3 1) :ELSE 'ok))
(IF (< 3 1) :ELSE 'ok)