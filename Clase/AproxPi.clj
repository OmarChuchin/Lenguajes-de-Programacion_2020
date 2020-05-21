(ns AproxPi)

(defn aprox-pi
  [num-rects]
  (let [width (/ 1 num-rects)]
  (loop [i 0
         sum 0]
    (let [mid (* width (+ i 0.5))
          height (/ 4 (+ 1 (* mid mid)))]
    (if (< i num-rects)
      (recur (+ i 1) (+ sum height))
      (* width sum))))))

(aprox-pi 10000)

