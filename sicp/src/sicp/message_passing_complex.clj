(ns sicp.message-passing-complex
  (:import (java.lang Math)))

; helpers
(defn square [x] (* x x))
(defn sqrt [x] (Math/sqrt x))
(defn atan [r i] (Math/atan2 r i))
(defn cos [x] (Math/cos x))
(defn sin [x] (Math/sin x))

(defn make-from-real-imag [x y]
  (letfn [(dispatch [op]
            (cond (= op 'real-part) x
                  (= op 'imag-part) y
                  (= op 'magnitude) (sqrt (+ (square x) (square y)))
                  (= op 'angle) (atan y x)
                  :else (throw (Exception. "Unknown op -- MAKE-FROM-REAL-IMAG" op))))]
    dispatch))

;((make-from-real-imag 4 5) 'real-part)

