(ns sicp.ch01-1)

; The <formal parameters> are the names withing the body of the procedure to 
; refer to the corresponding arguments of the procedure.

(defn square [x] (* x x))

(defn sum-of-squares [x y]
  (+ (square x) (square y)))

(defn f [a] 
  (sum-of-squares (+ a 1) (* a 2)))

(defn abs [x]
  (cond (> x 0) x
        (= x 0) 0
        (< x 0) (- x)))

(defn abs2 [x]
  (cond (< x 0) (do (+ 2 2) (- x))
        :else x))

(defn abs3 [x]
  (if (< x 0)
    (do (+ 2 2) (- x))
    x))


; Exercise 1.3
(defn sum-larger-of [x y z]
  (cond (and (> x z) (> y z)) (sum-of-squares x y)
        (and (> x y) (> z y)) (sum-of-squares x z)
        :else (sum-of-squares y z)))

; Exercise 1.4
; a-plus-abs-b always adds a to the absolute value of b
; when b is positive (+ a b) is evaluated
; when b is zero (- a b) is evaluated
; when b is negative (- a b) is evaluated 

; Exercise 1.5
(defn p [] (p))
(defn test-order [x y]
  (if (= x 0) 0 y))
;(test-order 0 (p))

; Since (p) will call itself recursively forever or in Clojure's case, cause a stack overflow.
; If the interpreter is using applicative-order evaluation, the test will never terminate
; because both arguments will be evaluated before the body of test-order is evaluated. If the
; interpreter is using normal-order evaluation the result will be 0, because the arguments to 
; test-only will not be evaluated before the body of test-order, and else branch of the if 
; expression will never be evaluated

(defn average [x y]
  (/ (+ x y) 2))

(defn improve [guess x]
  (average guess (/ x guess)))

(defn good-enough? [guess x]
  (< (abs (- (square guess) x)) 0.001))

(defn sqrt-iter 
  [guess x]
  (if (good-enough? guess x)
    guess
    (recur (improve guess x) x)))
               
(defn sqrt [x]
  (sqrt-iter 1.0 x))

;(sqrt 9)
;(sqrt (+ 100 37))
;(sqrt (+ (sqrt 2) (sqrt 3)))
;(square (sqrt 1000))

; Exercise 1.6
(defn new-if
  [predicate then-clause else-clause]
  (cond predicate then-clause
        :else else-clause))

;(new-if (= 2 3) 0 5) ; => 5
;(new-if (= 1 1) 0 5) ; => 0

; if new-if is used to define sqrt-iter
(defn bad-sqrt-iter 
  [guess x]
  (new-if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x) x)))
    ;(recur (improve guess x) x)))
; both branches of 'new-if' will be evaluated regardless of whether the 
; predicate is true or not
;(bad-sqrt-iter 1.0 2)
; will cause a stack overflow in the case of the named recursion 'sqrt-iter'
; will cause a compiler error when using 'recur' because the recursion will 
; no longer be in the tail position.


; Exercise 1.7
;(let [x 0.00001]
;  (* (alt-sqrt x) (alt-sqrt x)))
; in the case of really small numbers the guess can always be smaller than
; then an arbitrary tolerance???
; in the case of really large numbers, I think the distance between x and guess
; will never come within the tolerance due to loss of precision???

; hmmmm, this is not it!
(defn alt-good-enough? [last-guess current-guess]
  (let [guess-change (abs (- (last-guess current-guess)))]
    (< (/ guess-change current-guess) 0.001)))

(defn alt-sqrt-iter 
  [guess last-guess x]
  (if (alt-good-enough? 0 guess x)
    guess
    (recur (improve guess x) guess x)))
               
(defn alt-sqrt [x]
  (sqrt-iter 1.0 x))

; Exercise 1.8
(defn cube [x]
  (* x x x))

(defn improve-cube [guess x]
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))

(defn good-enough-cube? [guess x]
  (< (abs (- (cube guess) x)) 0.001))

(defn cubert-iter 
  [guess x]
  (if (good-enough-cube? guess x)
    guess
    (recur (improve-cube guess x) x)))
               
(defn cubert [x]
  (cubert-iter 1.0 x))

;(* (cubert 8.0) (cubert 8.0) (cubert 8.0))


(defn better-sqrt [x]
  (let [average (fn [x y] (/ (+ x y) 2))
        improve (fn [guess]
                  (average guess (/ x guess)))
        good-enough? (fn [guess]
                       (< (abs (- (square guess) x)) 0.001))
        sqrt-iter (fn [guess]
                    (if (good-enough? guess)
                      guess
                      (recur (improve guess))))]
    (sqrt-iter 1.0)))

(defn better-yet-sqrt [x]
  (letfn [(average [x y] (/ (+ x y) 2))
          (improve [guess]
            (average guess (/ x guess)))
          (good-enough? [guess]
            (< (abs (- (square guess) x)) 0.001))
          (sqrt-iter [guess]
            (if (good-enough? guess)
              guess
              (recur (improve guess))))]
    (sqrt-iter 1.0)))

