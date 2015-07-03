(ns sicp.sets)

(defn element-of-set? [x s]
  (cond (empty? s) false
        (= x (first s)) true
        :else (element-of-set? x (rest s))))

(defn adjoin-set [x s]
  (if (element-of-set? x s)
    s
    (cons x s)))

(defn intersection-set [s1 s2]
  (cond (or (empty? s1) (empty? s2)) '()
        (element-of-set? (first s1) s2) (cons (first s1)
                                              (intersection-set (rest s1) s2))
        :else (intersection-set (rest s1) s2)))

; Exercise 2.59
(defn union-set [s1 s2]
  (if (empty? s1)
    s2
    (union-set (rest s1) (adjoin-set (first s1) s2))))

; Exercise 2.60
; element-of-set? doesn't change

(defn adjoin-set [x s] (cons x s))
; adjoin-set is now constant time

(defn intersection-set [s1 s2]
  (if (or (empty? s1) (empty? s2)) 
    '()
    (cons (first s1) (intersection-set (rest s1) s2))))
; intersection-set becomes linear in the size of s1

; union-set doesn't need to change, however its efficent becomes
; linear in the size of s1
