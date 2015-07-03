(ns sicp.poly)

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

(defn install-polynomial-package []
  (letfn [;; internal procedures
          ;; representation of poly
          (make-poly [variable term-list]
            (conj variable term-list))
          (variable [p] (first p))
          (term-list [p] (rest p))
          (variable? [x] (symbol? x))
          (same-variable? [v1 v2]
            (and (variable? v1) (variable? v2) (= v1 v2)))
          ;; representation of terms and term lists
          ;<procedures adjoin-term ..coeff from text below>
          (add-poly [p1 p2] ...)
          ;; procedures used by add-poly
          (mul-poly [p1 p2] ...)
          ;; procedures used by mul-poly
          ;; interface to rest of the system

          (defn tag [p] (attach-tag 'polynomial p))]
    (put 'add '(polynomial polynomial)
      (fn [p1 p2] (tag (add-poly p1 p2))))
    (put 'mul '(polynomial polynomial)
      (fn [p1 p2] (tag (add-poly p1 p2))))
    (put 'make 'polynomial
      (fn [variable terms] (tag (make-poly variable terms)))))

(defn add-terms [L1 L2]
  (cond (empty-termlist? L1) L2
        (empty-termlist? L2) L1
        :else (let [t1 (first-term L1)
                    t2 (first-term L2)]
                (cond (> (order t1) (order t2)) 
                      (adjoin-term t1 (add-terms (rest-terms L1) L2))
                      (< (order t1) (order t2))
                      (adjoin-term L1 (rest-terms L2))
                      :else (adjoin-term
                              (make-term (order t1)
                                         (add (coeff t1) (coeff t2)))
                              (add-terms (rest-terms L1)
                                         (rest-terms L2)))))))

(defn mul-term-by-all-terms [t1 L]
  (if (empty-termlist? L)
    (the-empty-termlist? L)
    (let [t2 (first-term L)]
      (adjoin-term
        (make-term (+ (order t1) (order t2))
                   (mul (coeff t1) (coeff t2)))
        (mul-term-by-all-terms t1 (rest-terms L))))))

(defn mul-terms [L1 L2]
  (if (empty-termlist? L1)
    (the-empty-termlist)
    (add-terms (mul-term-by-all-terms (first-term L1) L2)
               (mul-terms (rest-terms L1) L2))))


                         




(defn add-poly [p1 p2]
  (if (same-variable? (variable p1) (variable p2))
    (make-poly (variable p1)
               (add-terms (term-list p1)
                          (term-list p2)))
    (throw (Exception. (str "Polys not in same var -- ADD-POLY"
                            (list p1 p2))))))

(defn mul-poly [p1 p2]
  (if (same-variable? (variable p1) (variable p2))
    (make-poly (variable p1)
               (mul-terms (term-list p1)
                          (term-list p2)))
    (throw (Exception. (str "Polys not in same var -- MUL-POLY"
                            (list p1 p2))))))

