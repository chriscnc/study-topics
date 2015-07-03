(ns sicp.ch02-1)

(defn gcd [a b]
  (if (= b 0)
    a
    (gcd b (rem a b))))

(defn make-rat [n d] 
  (let [g (gcd n d)]
    [(/ n g) (/ d g)]))

(defn numer [x] (first x))
; could also be defined as: (def numer first)
(defn denom [x] (second x))
; could also be defined as: (def denom second)

(defn add-rat [x y]
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(defn sub-rat [x y]
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(defn mul-rat [x y]
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(defn div-rat [x y]
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(defn equal-rat? [x y]
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(defn print-rat [x]
  (println (str (numer x) "/" (denom x))))

;(def one-half (make-rat 1 2))
;(print-rat one-half)
;(def one-third (make-rat 1 3))
;(print-rat (add-rat one-half one-third))
;(print-rat (mul-rat one-half one-third))
;(print-rat (add-rat one-third one-third))

; Exercise 2.1
(defn make-rat [n d] 
  (let [g (gcd n d)
        numer (if (neg? d) (- n) n)
        denom (if (neg? d) (- d) d)]
    [(/ n g) (/ d g)]))


; Exercise 2.2
(defn make-point [x y] [x y])
(defn x-point [p] (first p))
(defn y-point [p] (second p))

(defn make-segment [p1 p2] [p1 p2])
(defn start-segment [s] (first s))
(defn end-segment [s] (second s))

(defn average [x y] (/ (+ x y) 2.0))
(defn midpoint-segment [s]
  (make-point (average (x-point (start-segment s))
                       (x-point (end-segment s)))
              (average (y-point (start-segment s))
                       (y-point (end-segment s)))))

(defn print-point [p]
  (println (str "(" (x-point p) "," (y-point p) ")")))

(def seg (make-segment (make-point 0 0)
                       (make-point 2 2)))

;(print-point (midpoint-segment seg))

; Exercise 2.3
(defn make-rect [top-left bottom-right] [top-left bottom-right])
(defn abs [x] (if (neg? x) (- x) x))
(defn rect-top-left [r] (first r))
(defn rect-bottom-right [r] (second r))

(defn rect-length [r]
  (abs (- (x-point (rect-top-left r)) 
          (x-point (rect-bottom-right r)))))
(defn rect-height [r]
  (abs (- (y-point (rect-top-left r))
          (y-point (rect-bottom-right r)))))

(defn rect-perimeter [r]
  (+ (* 2 (rect-length r))
     (* 2 (rect-height r))))

(defn rect-area [r]
  (* (rect-length r) (rect-height r))) 

(def r (make-rect (make-point 0 2)
                  (make-point 3 0)))

;(assert (= (rect-perimeter r) 10))
;(assert (= (rect-area r) 6))

; change the representation of rect to a map
(defn make-rect [top-left bottom-right] {:top-left top-left :bottom-right bottom-right})
(defn rect-top-left [r] (:top-left r))
(defn rect-bottom-right [r] (:bottom-right r))


(defn sicp-cons [x y]
  (letfn [(dispatch [m] (cond (= m 0) x
                              (= m 1) y
                              :else (throw (Exception. "Argument not 0 or 1 -- CONS" m))))]
    dispatch))

(defn sicp-car [z] (z 0))
(defn sicp-cdr [z] (z 1))

;(sicp-car (sicp-cons 1 2))
;(sicp-cdr (sicp-cons 1 2))
           
; Exercise 2.4
;(car (cons 1 2))
;(car (lambda (m) (m 1 2)))
;((lambda (m) (m 1 2)) (lambda (p q) p))
;(lambda (1 2) 1)
;1
;(define (cdr z)
;  (z (lambda (p q) q)))


; Exercise 2.5
(defn exp [n x]
  (reduce * (repeat n x)))

(defn pair-cons [a b] 
  (fn [f] (f (* (exp 2 a) (exp 3 b)))))
(defn pair-car [p]
  ; not sure what math goes here)
(defn pair-cdr [p]
  ; not sure what math goes here)

; Exercise 2.6 - skipping

; Section 2.1.4 (Exercises 2.7-2.16) Extended exercise skipping

