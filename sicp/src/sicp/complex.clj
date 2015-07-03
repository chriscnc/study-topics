(ns sicp.complex
  (:import (java.lang Math)))

; helpers
(defn square [x] (* x x))
(defn sqrt [x] (Math/sqrt x))
(defn atan [r i] (Math/atan2 r i))
(defn cos [x] (Math/cos x))
(defn sin [x] (Math/sin x))


;; Explicit dispatch ;;;;;;;;;;;;;

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

(defn rectangular? [z] (= (type-tag z) 'rectangular))
(defn polar? [z] (= (type-tag z) 'polar))

; rectangular implementation
(defn real-part-rectangular [z] (first z))
(defn imag-part-rectangular [z] (second z))
(defn magnitude-rectangular [z]
  (sqrt (+ (square (real-part-rectangular z))
           (square (imag-part-rectangular z)))))
(defn angle-rectangular [z]
  (atan (imag-part-rectangular z) (real-part-rectangular z)))

(defn make-from-real-imag-rectangular [x y]
  (attach-tag 'rectangular (list x y)))
(defn make-from-mag-ang-rectangular [r a]
  (attach-tag 'rectangular (list (* r (cos a)) (* r (sin a)))))

; polar implementation
(defn magnitude-polar [z] (first z))
(defn angle-polar [z] (first z))
(defn make-from-real-imag-polar [x y]
  (attach-tag 'polar
              (list (sqrt (+ (square x) (square y)))
                    (atan y x))))
(defn make-from-mag-ang-polar [r a]
  (attach-tag 'polar (list r a)))
(defn real-part-polar [z]
  (* (magnitude-polar z) (cos (angle-polar z))))
(defn imag-part-polar [z]
  (* (magnitude-polar z) (sin (angle-polar z))))

; Generic selectors
(defn real-part [z]
  (cond (rectangular? z) (real-part-rectangular (contents z))
        (polar? z) (real-part-polar (contents z))
        :else (throw (Exception. "Unknown type -- REAL-PART" z))))

(defn imag-part [z]
  (cond (rectangular? z) (imag-part-rectangular (contents z))
        (polar? z) (imag-part-polar (contents z))
        :else (throw (Exception. "Unknown type -- IMAG-PART" z))))

(defn magnitude [z]
  (cond (rectangular? z) (magnitude-rectangular (contents z))
        (polar? z) (magnitude-polar (contents z))
        :else (throw (Exception. "Unknown type -- MAGNITUDE" z))))

(defn angle [z]
  (cond (rectangular? z) (angle-rectangular (contents z))
        (polar? z) (angle-polar (contents z))
        :else (throw (Exception. "Unknown type -- ANGLE" z))))

; Constructors
(defn make-from-real-imag [x y]
  (make-from-real-imag-rectangular x y))

(defn make-from-mag-ang [r a]
  (make-from-mag-ang-polar r a))

; Operations
(defn add-complex [z1 z2]
  (make-from-real-imag (+ (real-part z1) (real-part z2))
                       (+ (imag-part z1) (imag-part z2))))

(defn sub-complex [z1 z2]
  (make-from-real-imag (- (real-part z1) (real-part z2))
                       (- (imag-part z1) (imag-part z2))))

(defn mul-complex [z1 z2]
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                     (+ (angle z1) (angle z2))))

(defn div-complex [z1 z2]
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                     (- (angle z1) (angle z2))))

;; Data-driven dispatch

