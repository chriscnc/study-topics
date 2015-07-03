(ns sicp.ch01-2)

(defn factorial [n]
  (if (= n 1)
    1
    (* n (factorial (- n 1))))) ; can't use 'recur' here. not in tail-call position

(defn factorial [n]
  (fact-iter 1 1 n))

(defn fact-iter 
  [product counter max-count]
  (if (> counter max-count)
    product
    (recur (* counter product)
               (inc counter)
               max-count)))

; There is a distinction between a recursive process and a recusive procedure.
; The first factorial is both a recursive process and procedure. The second one
; using fact-iter is an iterative process even though it is a recursive procedure.

; Exercise 1.9
;recursive
;
;(defn + [a b]
;  (if (= a 0) 
;    b
;    (inc (+ (dec a) b))))
;
;(+ 4 5)
;(inc (+ 3 5))
;(inc (inc (+ 2 5)))
;(inc (inc (inc (+ 1 5))))
;(inc (inc (inc (inc (+ 0 5)))))
;(inc (inc (inc (inc 5))))
;(inc (inc (inc 6)))
;(inc (inc 7))
;(inc 8)
;9


;iterative
;(defn + [a b]
;  (if (= a 0)
;    b
;    (+ (dec a) (inc b))))
;
;(+ 4 5)
;(+ 3 6)
;(+ 2 7)
;(+ 1 8)
;(+ 0 9)
;9

; Exercise 1.10
(defn ackerman-fn [x y]
  (cond (= y 0) 0
        (= x 0) (* 2 y)
        (= y 1) 2
        :else (recur (dec x)
                           (ackerman-fn x (dec y)))))

;(ackerman-fn 1 10) ; => 1024
;(ackerman-fn 2 4) ; => 65536
;(ackerman-fn 3 3) ; => 65536

(defn f [n] (ackerman-fn 0 n)) ; the set of even integers
(defn g [n] (ackerman-fn 1 n)) ; {n | 2^n}
(defn h [n] (ackerman-fn 2 n)) ; {n | 2^{n | 2^n}2^n} 
(defn k [n] (* 5 n n)); 5n^2

(defn fib [n]
  (cond (= n 0) 0
        (= n 1) 1
        :else (+ (fib (- n 1))
                 (fib (- n 2)))))
; won't finish long before stack overflow because the time-complexity is exponential in n

(defn fib [n]
  (letfn [(fib-iter [a b cnt]
            (if (= cnt 0)
              b
              (recur (+ a b) a (dec cnt))))]
    (fib-iter 1 0 n)))
; linear process
;(fib 5)
;(fib-iter 1 0 5)
;(fib-iter 1 1 4)
;(fib-iter 2 1 3)
;(fib-iter 3 2 2)
;(fib-iter 5 3 1)
;(fib-iter 8 5 0)
;5

(defn count-change [amount]
  (letfn [(first-denomination [kinds-of-coins]
            (cond (= kinds-of-coins 1) 1
                  (= kinds-of-coins 2) 5
                  (= kinds-of-coins 3) 10
                  (= kinds-of-coins 4) 25
                  (= kinds-of-coins 5) 50))
          (cc [amount kinds-of-coins]
            (cond (= amount 0) 1
                  (or (< amount 0) (= kinds-of-coins 0)) 0
                  :else (+ (cc amount
                               (- kinds-of-coins 1))
                           (cc (- amount (first-denomination kinds-of-coins))
                               kinds-of-coins))))]
    (cc amount 5)))

; Exercise 1.11
; f(n) = n if n < 3
; f(n) = f(n-1) + 2f(n-2) + 3f(n-3) if n >= 3

(defn f [n]
  (cond (= n 0) 0
        (= n 1) 1
        (= n 2) 2
        :else (+ (f (- n 1))
                 (* 2 (f (- n 2)))
                 (* 3 (f (- n 3))))))

; doesn't work
(defn f [n]
  (letfn [(f-iter [a b c cnt]
            (cond (= cnt 0) c
                  (= cnt 1) b
                  (= cnt 2) a
                  :else (+ (f-iter a b c (dec cnt))
                           (* 2 (f-iter b c (- c 1) (dec cnt)))
                           (* 3 (f-iter c (- c 1) (- c 2) (dec cnt))))))]
    (f-iter 2 1 0 n)))


; Exercise 1.12
(defn pascal [n]
  (letfn [(p-iter [acc row]
            (if (> row n) 
              acc
              (p-iter)))]
    (cond (= n 1) [1]
      (= n 2) [1 [1 1]]
      :else (p-iter [1 [1 1]] 3))))


; Exercise 1.13 proof

; Exercise 1.14
; I believe the depth of the tree will be bounded by the amount/smallest-denomination.
; Since the smallest-denomination is 1, the tree depth will be <= amount.
; The far left branch will only be as deep as the number of denominations

; Exercise 1.15
(defn abs [x] (if (< x 0) (- x) x))
(defn cube [x] (* x x x))
(def p-calls (atom 0))
(defn p [x] 
  (do
    (swap! p-calls inc)
    (- (* 3 x) (* 4 (cube x)))))
(defn sine [angle]
  (if (not (> (abs angle) 0.1))
    angle
    (p (sine (/ angle 3.0)))))

;(sine 12.15)

; a) p is called 5 times
; b) ???

(defn expt [b n]
  (if (= n 0)
    1
    (* b (expt b (- n 1)))))

(defn expt [b n]
  (letfn [(expt-iter [b counter product]
            (if  (= counter 0)
              product
              (recur b (dec counter) (* b product))))]
    (expt-iter b n 1)))

(defn square [x] (* x x))

(defn fast-expt [b n]
  (cond (= n 0) 1
        (even? n) (square (fast-expt b (/ n 2)))
        :else (* b (fast-expt b (- n 1)))))

; Exercise 1.16 - skipped

; Exercise 1.17
(defn dbl [x] (+ x x))
(defn halve [x] (/ x 2))
(defn fast-mult [a b]
  (cond (or (= a 0) (= b 0)) 0 
        (= a 1) b
        (= b 1) a
        (even? b) (dbl (fast-mult a (halve b)))
        :else (+ a (fast-mult a (dec b)))))


; Exercise 1.18 - skipped
; Exercise 1.19 - skipped

(defn gcd [a b]
  (if (= b 0)
    a
    (gcd b (rem a b))))

; Exercise 1.20
; applicative order
(gcd 206 40)
(gcd 40 (rem 206 40))
(gcd 40 6)
(gcd 6 (rem 40 6))
(gcd 6 4)
(gcd 4 (rem 6 4))
(gcd 4 2)
(gcd 2 (rem 4 2))
(gcd 2 0)
2
; normal order
(gcd 206 40)
(gcd 40 (rem 206 40))
(gcd (rem 206 40) 
     (rem 40 (rem 206 40)))
; this gets way to busy to continue
; looks like rem will get call a lot in the normal order execution and
; only 4 times in the applicative order execution

(defn divides? [a b] (= (rem b a) 0))
(defn find-divisor [n test-divisor]
  (cond (> (square test-divisor) n) n
        (divides? test-divisor n) test-divisor
        :else (find-divisor n (inc test-divisor))))
(defn smallest-divisor [n]
  (find-divisor n 2))

(defn prime? [n]
  (= n (smallest-divisor n)))

(defn expmod [base exp m]
  (cond (= exp 0) 1
        (even? exp) (rem (square (expmod base (/ exp 2) m))
                         m)
        :else (rem (* base (expmod base (- exp 1) m))
                   m)))

(defn fermat-test [n]
  (letfn [(try-it [a]
            (= (expmod a n n) a))]
    (try-it (+ 1 (rand-int (dec n))))))


(defn fast-prime? [n times]
  (cond (= times 0) true
        (fermat-test n) (recur n (dec times))
        :else false))

;(fast-prime? 1239848837 10000000)
;(fast-prime? 1239848838 10000000)

; Exercise 1.21
(smallest-divisor 199)
(smallest-divisor 1999)
(smallest-divisor 19999)

; Exercises 1.22 - 1.28 - skipping
