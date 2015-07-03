(ns sicp.gen-arith
  (:import (java.lang Math)))

; op/type dispatch table
(def dispatch (atom {}))

(defn put [op tipe item]
  (let [k {op tipe}] 
    (swap! dispatch assoc k item)))

(defn git [op tipe]
  (let [k {op tipe}]
    (@dispatch k)))

; type tagging
(defn attach-tag [type-tag contents]
  (conj contents type-tag))

(defn type-tag [datum]
  (if (list? datum)
    (first datum)
    (throw (Exception. "Bad tagged datum -- TYPE-TAG" datum))))

(defn contents [datum]
  (if (list? datum)
    (rest datum)
    (throw (Exception. "Bad tagged datum == CONTENTS" datum))))

(defn apply-generic [op & args]
  (let [type-tags (map type-tag args)
        proc (git op type-tags)]
    (if proc
      (apply proc (map contents args))
      (throw (Exception. (str "No method for these types -- APPLY-GENERIC" 
                              (list op type-tags)))))))

; generic arithmetic procedures
(defn add [x y] (apply-generic 'add x y))
(defn sub [x y] (apply-generic 'sub x y))
(defn mul [x y] (apply-generic 'mul x y))
(defn div [x y] (apply-generic 'div x y))

; primitive operations
(defn install-clojure-number-package []
  (letfn [(tag [x] (attach-tag 'clojure-number x))]
    (put 'add '(clojure-number clojure-number)
      (fn [x y] (tag (+ x y))))
    (put 'sub '(clojure-number clojure-number)
      (fn [x y] (tag (- x y))))
    (put 'mul '(clojure-number clojure-number)
      (fn [x y] (tag (* x y))))
    (put 'div '(clojure-number clojure-number)
      (fn [x y] (tag (/ (double x) y))))
    (put 'make 'clojure-number
      (fn [x] (tag x)))))

(defn make-clojure-number [n]
  ((get 'make 'scheme-number) n))

; rational operations
(defn install-rational-package []
  (letfn [;; internal procedures
          (gcd [a b]
            (if (= b 0)
              a
              (gcd b (rem a b))))
          (numer [x] (first x))
          (denom [x] (second x))
          (make-rat [n d] 
            (let [g (gcd n d)]
              (list (/ n g) (/ d g))))
          (add-rat [x y] 
            (make-rat (+ (* (numer x) (denom y))
                         (* (numer y) (denom x)))
                      (* (denom x) (denom y))))
          (sub-rat [x y] 
            (make-rat (- (* (numer x) (denom y))
                         (* (numer y) (denom x)))
                      (* (denom x) (denom y))))
          (mul-rat [x y]
            (make-rat (* (numer x) (numer y))
                      (* (denom x) (denom y))))
          (div-rat [x y]
            (make-rat (* (numer x) (denom y))
                      (* (denom x) (numer y))))
          ;; interface to rest of the system
          (tag [x] (attach-tag 'rational x))]
    (put 'add '(rational rational)
      (fn [x y] (tag (add-rat x y))))
    (put 'sub '(rational rational)
      (fn [x y] (tag (sub-rat x y))))
    (put 'mul '(rational rational)
      (fn [x y] (tag (mul-rat x y))))
    (put 'div '(rational rational)
      (fn [x y] (tag (div-rat x y))))
    (put 'make 'rational
      (fn [n d] (tag (make-rat n d))))))

; complex operations
(defn install-complex-package []
  (letfn [;; imported procedures from rectangular
          (make-from-real-imag [x y]
            ((get 'make-from-real-imag 'rectangular) x y))
          (make-from-mag-ang [r a]
            ((get 'make-from-mag-ang 'polar) r a))
          ;; internal procedures
          (add-complex [z1 z2]
            (make-from-real-imag (+ (real-part z1) (real-part z2))
                                 (+ (imag-part z1) (imag-part z2))))
          (sub-complex [z1 z2]
            (make-from-real-imag (- (real-part z1) (real-part z2))
                                 (- (imag-part z1) (imag-part z2))))
          (mul-complex [z1 z2]
            (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                               (+ (angle z1) (angle z2))))
          (div-complex [z1 z2]
            (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                               (- (angle z1) (angle z2))))
          ;; interface to rest of the system
          (tag [z] (attach-tag 'complex z))]
    (put 'real-part '(complex) real-part)
    (put 'imag-part '(complex) imag-part)
    (put 'magnitude '(complex) magnitude)
    (put 'angle '(complex angle))
    (put 'add '(complex complex)
      (fn [z1 z2] (tag (add-complex z1 z2))))
    (put 'sub '(complex complex)
      (fn [z1 z2] (tag (sub-complex z1 z2))))
    (put 'mul '(complex complex)
      (fn [z1 z2] (tag (mul-complex z1 z2))))
    (put 'div '(complex complex)
      (fn [z1 z2] (tag (div-complex z1 z2))))
    (put 'make-from-real-imag 'complex
      (fn [x y] (tag (make-from-real-imag x y))))
    (put 'make-from-mag-ang 'complex
      (fn [r a] (tag (make-from-mag-ang r a))))))
            
(defn make-complex-from-real-imag [x y]
  ((get 'make-from-real-imag 'complex) x y))
(defn make-complex-from-mag-ang [r a]
  ((get 'make-from-mag-ang 'complex) r a))

(install-complex-package)
(def z (make-complex-from-real-imag 3 4))
;; might return to work on this further.
