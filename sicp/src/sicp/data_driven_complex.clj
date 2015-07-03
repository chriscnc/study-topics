(ns sicp.data-driven-complex
  (:import (java.lang Math)))

; helpers
(defn square [x] (* x x))
(defn sqrt [x] (Math/sqrt x))
(defn atan [r i] (Math/atan2 r i))
(defn cos [x] (Math/cos x))
(defn sin [x] (Math/sin x))

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

; op/type dispatch table
(def dispatch (atom {}))

(defn put [op tipe item]
  (let [k {op tipe}] 
    (swap! dispatch assoc k item)))

(defn git [op tipe]
  (let [k {op tipe}]
    (@dispatch k)))

; rectangular-package
(defn install-rectangular-package []
  (letfn [;; internal procedures 
          (real-part [z] (first z))
          (imag-part [z] (second z))
          (make-from-real-imag [x y] (list x y))
          (magnitude [z] (sqrt (+ (square (real-part z))
                                  (square (imag-part z)))))
          (angle [z] (atan (imag-part z) (real-part z)))
          (make-from-mag-ang [r a] (list (* r (cos a)) 
                                         (* r (sin a))))
          ;; interface to the rest of the system
          (tag [x] (attach-tag 'rectangular x))]
    (do 
      (put 'real-part '(rectangular) real-part)
      (put 'imag-part '(rectangular) imag-part)
      (put 'magnitude '(rectangular) magnitude)
      (put 'angle '(rectangular) angle)
      (put 'make-from-real-imag 'rectangular
           (fn [x y] (tag (make-from-real-imag x y))))
      (put 'make-from-mag-ang 'rectangular
           (fn [r a] (tag (make-from-mag-ang r a)))))))

; polar-package
(defn install-polar-package []
  (letfn [;; internal procedures 
          (magnitude [z] (first z))
          (angle [z] (second z))
          (make-from-mag-ang [r a] (list r a))
          (real-part [z] (* (magnitude z) (cos (angle z))))
          (imag-part [z] (* (magnitude z) (sin (angle z))))
          (make-from-real-imag [x y] (list (sqrt (+ (square x) (square y)))
                                           (atan y x)))
          ;; interface to the rest of the system
          (tag [x] (attach-tag 'polar x))]
    (do 
      (put 'real-part '(polar) real-part)
      (put 'imag-part '(polar) imag-part)
      (put 'magnitude '(polar) magnitude)
      (put 'angle '(polar) angle)
      (put 'make-from-real-imag 'polar
           (fn [x y] (tag (make-from-real-imag x y))))
      (put 'make-from-mag-ang 'polar
           (fn [r a] (tag (make-from-mag-ang r a)))))))

(defn apply-generic [op & args]
  (let [type-tags (map type-tag args)
        proc (git op type-tags)]
    (if proc
      (apply proc (map contents args))
      (throw (Exception. (str "No method for these types -- APPLY-GENERIC" 
                              (list op type-tags)))))))

(defn real-part [z] (apply-generic 'real-part z))
(defn imag-part [z] (apply-generic 'imag-part z))
(defn magnitude [z] (apply-generic 'magnitude z))
(defn angle [z] (apply-generic 'angle z))
(defn make-from-real-imag [x y]
  ((git 'make-from-real-imag 'rectangular) x y))
(defn make-from-mag-ang [r a]
  ((git 'make-from-mag-ang 'polar) r a))

(install-polar-package)
(install-rectangular-package)
(def c (make-from-real-imag 4 5))
(def d (make-from-mag-ang 6 90))
;(real-part d)
;(angle c)


