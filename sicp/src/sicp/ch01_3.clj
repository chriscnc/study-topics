(ns sicp.ch01-3
  (:import (java.lang.Math)))

(defn cube [x] (* x x x))

(defn sum-integers [a b]
  (if (> a b)
    0
    (+ a (sum-integers (inc a) b))))

(defn sum-cubes [a b]
  (if (> a b)
    0 
    (+ (cube a) (sum-cubes (inc a) b))))

(defn pi-sum [a b]
  (if (> a b)
    0
    (+ (/ 1.0 (* a (+ a 2))) (pi-sum (+ a 4) b))))

(defn sum [term a next b]
  (if (> a b)
    0
    (+ (term a)
       (sum term (next a) next b))))

(defn sum-cubes [a b]
  (sum cube a inc b))

(defn sum-integers [a b]
  (sum identity a inc b))

(defn pi-sum [a b]
  (letfn [(pi-term [x] (/ 1.0 (* x (+ x 2))))
          (pi-next [x] (+ x 4))]
    (sum pi-term a pi-next b)))


(defn integral [f a b dx]
  (letfn [(add-dx [x] (+ x dx))]
    (* (sum f (+ a (/ dx 2.0)) add-dx b))))

; Exercise 1.29 - Simpsons rull - skipping

; Exercise 1.30
(defn sum [term a next b]
  (letfn [(iter [a result]
            (if (> a b)
              result
              (recur (next a) (+ result (term a)))))]
    (iter a 0)))

(sum identity 1 inc 5)

; Exercise 1.31
(defn product [term a next b]
  (if (> a b)
    1
    (* (term a)
       (product term (next a) next b))))

(defn product [term a next b]
  (letfn [(iter [a result]
            (if (> a b)
              result
              (recur (next a) (* result (term a)))))]
    (iter a 1)))

(defn factorial [n]
  (product identity 1 inc n))

;(factorial 5); => 120

(defn pi-approx [n]
  (letfn [(term [x]
           (let [d (double x)]
             (if (odd? x)
               (/ (+ d 1) (+ d 2))
               (/ (+ d 2) (+ d 1)))))]
    (* 4 (product term 1 inc n))))

;(pi-approx 10000); => 3.13174


; Exercise 1.32
(defn accumulate [combiner null-value term a next b]
  (if (> a b)
    null-value
    (combiner (term a)
       (accumulate combiner null-value term (next a) next b))))

(defn accumulate [combiner null-value term a next b]
  (letfn [(iter [a result]
            (if (> a b)
              result
              (recur (next a) (combiner result (term a)))))]
    (iter a null-value)))

(defn sum [a b]
  (accumulate + 0 identity a inc b))

(defn product [a b]
  (accumulate * 1 identity a inc b))

;(sum 1 5)
;(product 1 5)

; Exercise 1.33
(defn accumulate-filter [combiner null-value pred term a next b]
  (letfn [(iter [a result]
            (if (> a b)
              result
              (if (pred a)
                (recur (next a) (combiner result (term a)))
                (recur (next a) result))))]
    (iter a null-value)))


(defn sum-even [a b]
  (accumulate-filter + 0 even? identity a inc b))

;(sum-even 1 10)

; in schema this doesn't compile, the x in the second let binding must come from the scope
; outside the let. In Clojure x in the second let binding takes the value 3.
;(let [x 3
;      y (+ x 2)]
;  (* x y))

; Exercise 1.34
; An evaluatation of (2 2) will be attempted and fail because 2 is not function.

(defn average [x y] (/ (+ (double x) y) 2))
(defn abs [x] (if (< x 0) (- x) x))

(defn close-enough? [x y]
  (< (abs (- x y)) 0.001))

(defn search [f neg-point pos-point]
  (let [midpoint (average neg-point pos-point)]
    (if (close-enough? neg-point pos-point)
      midpoint
      (let [test-value (f midpoint)]
        (cond (pos? test-value) (search f neg-point midpoint)
              (neg? test-value) (search f midpoint pos-point)
              :else midpoint)))))

(defn half-interval-method [f a b]
  (let [a-value (f a)
        b-value (f b)]
    (cond (and (neg? a-value) (pos? b-value)) (search f a b)
          (and (neg? b-value) (pos? a-value)) (search f b a)
          :else (throw (Exception. (format "Values are not of opposite sign (%d,%d)" a b))))))

;(half-interval-method #(Math/sin %) 2.0 4.0)
;(half-interval-method #(- (* % % %) (* 2 %) 3) 1.0 2.0)

(def tolerance 0.0001)
(defn fixed-point [f first-guess]
  (letfn [(close-enough? [v1 v2] (< (abs (- v1 v2)) tolerance))
          (attempt [guess]
            (let [next-guess (f guess)]
              (if (close-enough? guess next-guess)
                next-guess
                (recur next-guess))))]
    (attempt first-guess)))

;(fixed-point #(Math/cos %) 1.0)
;(fixed-point #(+ (Math/sin %) (Math/cos %)) 1.0)

(defn sqrt [x]
  (fixed-point (fn [y] (average y (/ x y))) 1.0))

; Exercises 1.35 - 1.39 - skipped (need to know more math)

(defn average-damp [f]
  (fn [x] (average x (f x))))

(defn square [x] (* x x))
((average-damp square) 10)

(defn sqrt [x]
  (fixed-point (average-damp (fn [y] (/ x y))) 1.0))

(defn cube-root [x]
  (fixed-point (average-damp (fn [y] (/ x (square y)))) 1.0))

(def dx 0.00001)

(defn deriv [g]
  (fn [x]
    (/ (- (g (+ x dx)) (g x))
       dx)))

((deriv cube) 5)

(defn newton-transform [g]
  (fn [x]
    (- x (/ (g x) ((deriv g) x)))))

(defn newtons-method [g guess]
  (fixed-point (newton-transform g) guess))

(defn sqrt [x]
  (newtons-method (fn [y] (- (square y) x)) 1.0))


(defn fixed-point-of-transform [g transform guess]
  (fixed-point (transform g) guess))

(defn sqrt [x]
  (fixed-point-of-transform (fn [y] (/ x y)) 
                            average-damp
                            1.0))

(defn sqrt [x]
  (fixed-point-of-transform (fn [y] (- (square y) x))
                            newton-transform
                            1.0))

; Exercise 1.40 - skipped

; Exercise 1.41
(defn do-twice [f]
  (fn [x] (f (f x))))

;((do-twice inc) 5); => 7

; Exercise 1.42
(defn compose [f g]
  (fn [x] (f (g x))))

; Exercise 1.43 - this is not correct
(defn repeated [f n]
  (fn [x]
    (letfn [(iter [result i]
              (if (> i n)
                result
                (iter (f result) (inc i))))]
      (iter (f x) 2))))

((repeated square 2) 2)

; Exercise 1.44 - 1.46 - skipping

