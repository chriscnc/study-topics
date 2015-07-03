(ns sicp.ordered-sets)

(defn element-of-set? [x s]
  (cond (empty? s) false
        (= x (first s)) true
        (< x (first s)) false
        :else (recur x (rest s))))

(defn intersection-set [s1 s2]
  (if (or (empty? s1) (empty? s2))
    '()
    (let [x1 (first s1)
          x2 (first s2)]
      (cond (= x1 x2) (cons x1 (intersection-set (rest s1)
                                                 (rest s2)))
            (< x1 x2) (recur (rest s1) s2)
            (> x1 x2) (recur s1 (rest s2))))))
                                        
; Exercise 2.61
(defn adjoin-set [x s]
  (if (empty? s)
    (cons x s)
    (let [s1 (first s)]
      (cond (= x s1) (rest s)
            (< x s1) (cons x s)
            (> x s1) (cons s1 (adjoin-set x (rest s)))))))


; Exercise 2.62
(defn union-set [s1 s2]
  (cond (empty? s1) s2
        (empty? s2) s1
        :else (let [x1 (first s1)
                    x2 (first s2)]
                (cond (= x1 x2) (cons x1 (union-set (rest s1) (rest s2)))
                      (< x1 x2) (cons x1 (union-set (rest s1) s2))
                      (> x1 x2) (cons x2 (union-set s1 (rest s2)))))))


