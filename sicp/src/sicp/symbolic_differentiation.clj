(ns sicp.symbolic-differentiation)

(defn variable? [x] (symbol? x))

(defn same-variable? [v1 v2]
  (and (variable? v1) (variable? v2) (= v1 v2)))

(defn =number? [exp num]
  (and (number? exp) (= exp num)))

(defn make-sum [a1 a2] 
  (cond (=number? a1 0) a2
        (=number? a2 0) a1
        (and (number? a1) (number? a2)) (+ a1 a2)
        :else (list '+ a1 a2)))

(defn make-product [m1 m2] 
  (cond (or (=number? m1 0) (=number? m2 0)) 0
        (=number? m1 1) m2
        (=number? m2 1) m1
        (and (number? m1) (number? m2)) (* m1 m2)
        :else (list '* m1 m2)))

(defn make-exponentiation [base exponent]
  (cond (= exponent 0) 1
        (= exponent 1) base
        :else (list '** base exponent)))

(defn sum? [x]
  (and (list? x) (= (first x) '+)))

(defn addend [s] (first (rest s)))

(defn augend [s] (first (rest (rest s))))

(defn product? [x]
  (and (list? x) (= (first x) '*)))

(defn multiplier [p] (first (rest p)))

(defn multiplicand [p] (first (rest (rest p))))

(defn exponentiation? [x]
  (and (list? x) (= (first x) '**)))

(defn base [e] (second e))

(defn exponent [e] (nth e 2))

(defn deriv [exp variable]
  (cond (number? exp) 0
        (variable? exp) (if (same-variable? exp variable) 1 0)
        (sum? exp) (make-sum (deriv (addend exp) variable)
                             (deriv (augend exp) variable))
        (product? exp) (make-sum
                         (make-product (multiplier exp)
                                       (deriv (multiplicand exp) variable))
                         (make-product (deriv (multiplier exp) variable)
                                       (multiplicand exp)))
        (exponentiation? exp) (cond (= (exponent exp) 0) 1
                                    (= (exponent exp) 1) (base exp)
                                    :else (make-product 
                                            (exponent exp)
                                            (make-exponentiation (base exp)
                                                                 (dec (exponent exp)))))
        :else (throw (Exception. "unknown expressiong type -- DERIV" exp))))

; Exercises 2.55-2.56
(deriv '(+ x 3) 'x)
(deriv '(* x y) 'x)
(deriv '(* (* x y) (+ x 3)) 'x)
(deriv '(** x 2) 'x)
(deriv '(** (+ x 2) 3) 'x)

; Exercises 2.57-2.58 skipped

