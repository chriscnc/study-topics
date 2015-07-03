(ns sicp.ch02-2
  (:import (java.lang.Math)))

(defn list-ref [items n]
  (if (= n 0)
    (first items)
    (recur (rest items) (dec n))))

(def squares (list 1 4 9 16 25))

;(list-ref squares 3)

(defn length [items]
  (if (empty? items)
    0 
    (+ 1 (length (rest items)))))

; can use 'next' with 'nil?' or 'empty?' but you need to use 'empty?' with rest.

(def odds (list 1 3 5 7))
;(length odds)

(defn append [list1 list2]
  (if (empty? list1)
    list2
    (cons (first list1) 
          (append (rest list1) list2))))

;(append odds squares)

; Exercise 2.17
(defn last-pair [coll]
  (if (empty? (rest coll))
    (first coll)
    (recur (rest coll))))

;(last-pair (list 23 72 149 34))

; Exercise 2.18
(defn rev [coll]
  (letfn [(rev-iter [acc coll]
            (if (empty? coll)
              acc
              (recur (cons (first coll) acc) (next coll))))]
    (rev-iter '() coll)))

(rev (list 1 4 9 16 25))

; version of append that won't blow the stack and uses rev
(defn append-acc [list1 list2]
  (letfn [(append-iter [acc coll]
            (if (empty? coll)
              acc
              (recur (cons (first coll) acc) (rest coll))))]
    (append-iter list2 (rev list1))))

(append-acc odds squares)

; Exercise 2.19
(def us-coins (list 50 25 10 5 1))
(def uk-coins (list 100 50 20 10 5 2 1 0.5))

(def no-more? empty?)
(def except-first-denomination rest)
(def first-denomination first)

(defn cc [amount coin-values]
  (cond (= amount 0) 1
        (or (< amount 0) (no-more? coin-values)) 0
        :else (+ (cc amount
                     (except-first-denomination coin-values))
                 (cc (- amount (first-denomination coin-values))
                     coin-values))))

;(cc 100 us-coins)
;(cc 100 (rev us-coins))
;(cc 100 uk-coins)

; Exercise 2.20
(defn same-parity [x & more]
  (letfn [(parity-filter [pred acc coll] 
            (if (empty? coll)
              (rev acc)
              (if (pred (first coll))
                (recur pred (cons (first coll) acc) (rest coll))
                (recur pred acc (rest coll)))))]
  (if (even? x)
    (parity-filter even? (list x) more)
    (parity-filter odd? (list x) more))))

;(same-parity 1 2 3 4 5 6 7)
;(same-parity 2 3 4 5 6 7)


(defn scale-list [items factor]
  (if (empty? items)
    items
    (cons (* (first items) factor)
          (scale-list (rest items) factor))))

(scale-list (list 1 2 3 4 5) 10)

(defn mapcar [proc items]
  (if (empty? items)
    items
    (cons (proc (first items))
          (mapcar proc (rest items)))))

(def abs #(Math/abs %))
;(mapcar abs (list -10 2.5 -11.6 17))
;(mapcar (fn [x] (* x x)) (list 1 2 3 4))

(defn scale-list [items factor]
  (mapcar #(* % factor) items))

; Exercise 2.21
(defn square-list [items]
  (letfn [(square [x] (* x x))]
  (if (empty? items)
    items
    (cons (square (first items)) (square-list (rest items))))))

(defn square-list [items]
  (map #(* % %) items))

;(square-list (list 1 2 3 4))

; Exercise 2.22
; Q1: because cons puts its argument onto the front of the list.
; Q2: because cons takes as its arguments an element and a list not the otherway around

; Exercise 2.23
(defn for-each [f coll]
  (if (not (empty? coll))
    (do
      (f (first coll))
      (recur f (rest coll)))))

;(for-each println (list 57 321 88))
;(doseq [e (list 57 321 88)] (println e))

(defn count-leaves [x]
  (cond (empty? x) 0
        (list? (first x)) (+ (count-leaves (first x))
                             (count-leaves (rest x)))
        :else (+ 1 (count-leaves (rest x)))))

; Exercise 2.24
;(1 (2 (3 4)))

; Exercise 2.25
(def a [1 3 [5 7] 9])
;(first (rest (first (rest (rest a)))))
(def b [[7]])
;(first (first b))
(def c [1 [2 [3 [4 [5 [6 7]]]]]])
;(first (rest (first (rest (first (rest (first (rest (first (rest (first (rest c))))))))))))

; Exercise 2.26
(def x [1 2 3])
(def y [4 5 6])
;(into x y); => [1 2 3 4 5 6]
;(cons x y); => ([1 2 3] 4 5 6)
;(list x y); => ([1 2 3] [4 5 6])

; Exercise 2.27
(def x (list (list 1 2) (list 3 4)))

(defn deep-reverse [coll]
  (letfn [(rev-iter [acc coll]
            (cond (empty? coll) acc
                  (list? (first coll)) (recur (cons (deep-reverse (first coll)) acc)
                                              (next coll))
                  :else (recur (cons (first coll) acc) (next coll))))]
    (rev-iter '() coll)))

;(deep-reverse x)

; Exercise 2.28
(def x (list (list 1 2) (list 3 4)))
(defn fringe [coll]
  (letfn [(iter [acc coll]
            (cond (empty? coll) acc
            (list? (first coll)) (recur (iter acc (first coll))
                                        (rest coll))
            :else (recur (cons (first coll) acc) (rest coll))))]
    (rev (iter '() coll))))

;(fringe x)
;(fringe (list x x))

; Exercise 2.29
(defn make-mobile [left right]
  (list left right))

(defn make-branch [length structure]
  (list length structure))

; a)
(def left-branch first)
(def right-branch second)
(def branch-length first)
(def branch-structure second)
(def branch-weight second)
(defn mobile? [branch] (list? (branch-structure branch)))

; b)
(defn total-weight [mobile]
  (let [left (left-branch mobile)
        right (right-branch mobile)]
    (+
     (if (mobile? left)
       (total-weight (branch-structure left))
       (branch-weight left))
     (if (mobile? right)
       (total-weight (branch-structure right))
       (branch-weight right)))))

(def m (make-mobile (make-branch 1 3)
                    (make-branch 1 (make-mobile (make-branch 1 5)
                                                (make-branch 1 7)))))
(def m (make-mobile (make-branch 2 6)
                    (make-branch 1 (make-mobile (make-branch 1 9)
                                                (make-branch 3 3)))))
;(total-weight m)

; c) 
(defn branch-torque [b]
  (let [weight (if (mobile? b)
                 (total-weight (branch-structure b))
                 (branch-weight b))
        length (branch-length b)]
    (* length weight)))

(defn balanced? [m]
  (let [left (left-branch m)
        right (right-branch m)]
    (and 
      (= (branch-torque left) (branch-torque right))
      (if (mobile? left)
        (balanced? (branch-structure left))
        true)
      (if (mobile? right)
        (balanced? (branch-structure right))
        true))))

; d) skipping


(defn scale-tree [tree factor]
  (cond (not (seq? tree)) (* tree factor)
        (empty? tree) '()
        :else (cons (scale-tree (first tree) factor) 
                    (scale-tree (rest tree) factor))))

(def t (list 1 (list 2 (list 3 4) 5) (list 6 7)))
(scale-tree t 10)

(defn scale-tree [tree factor]
  (map (fn [sub-tree]
         (if (seq? sub-tree)
           (scale-tree sub-tree factor)
           (* sub-tree factor)))
       tree))

; Exercise 2.30
(defn square [x] (* x x))

(defn square-tree [tree]
  (cond (not (seq? tree)) (square tree)
        (empty? tree) '()
        :else (cons (square-tree (first tree)) 
                    (square-tree (rest tree)))))

(defn square-tree [tree]
  (map (fn [sub-tree]
         (if (seq? sub-tree)
           (square-tree sub-tree)
           (square sub-tree)))
       tree))

(square-tree (list 1 
                   (list 2 (list 3 4) 5)
                   (list 6 7)))

; Exercise 2.31
(defn tree-map [f tree]
  (cond (not (seq? tree)) (f tree)
        (empty? tree) '()
        :else (cons (tree-map f (first tree))
                    (tree-map f (rest tree)))))
  
(defn square-tree [tree]
  (tree-map square tree))

; Exercise 2.32
(defn subsets [s]
  (if (empty? s)
    (list '())
    (let [cdr (subsets (rest s))]
      (concat cdr (map #(cons (first s) %) cdr)))))
; could use 'into' in place of 'concat' and the order of the
; subsets would be reversed because 'into' uses 'conj' instead of
; 'cons'
;(subsets (list 1 2 3))
; at each level of the recursion the first element is added
; to all of the subsets from the last recursion, this is then
; appended to the subsets. That's what it does, as for why it
; works, not sure of another answer other than the operational 
; definition just given

(defn filtrate [pred coll]
  (cond (empty? coll) '()
        (pred (first coll)) (cons (first coll)
                                  (filtrate pred (rest coll)))
        :else (filtrate pred (rest coll))))

;(filtrate even? (range 20))

(defn accumulate [op initial coll]
  (if (empty? coll)
    initial
    (op (first coll) 
        (accumulate op initial (rest coll)))))

;(accumulate + 0 (range 1 6))
;(accumulate * 1 (range 1 6))
;(accumulate cons nil (range 1 6))
;(accumulate str "" [\c \h \r \i \s])

(defn enumerate-interval [low high]
  (if (> low high)
    nil
    (cons low (enumerate-interval (inc low) high))))

;(enumerate-interval 2 7)

(defn enumerate-tree [tree]
  (cond (not (seq? tree)) (list tree)
        (empty? tree) nil
        :else (concat (enumerate-tree (first tree))
                      (enumerate-tree (rest tree)))))

;(enumerate-tree (list 1 (list 2 (list 3 4)) 5))

(defn sum-odd-squares [tree]
  (accumulate +
              0
              (map square
                   (filter odd?
                           (enumerate-tree tree)))))

;(sum-odd-squares (list 1 (list 2 (list 3 4)) 5))

; Exercise 2.33
(defn acc-map [p coll]
  (accumulate (fn [x y] (cons (p x) y)) nil coll))
(defn acc-append [coll1 coll2]
  (accumulate cons coll2 coll1))
(defn acc-length [coll]
  (accumulate (fn [x y] (inc y)) 0 coll))

; Exercise 2.34
(defn horner-eval [x coefficient-sequence]
  (accumulate (fn [this-coeff higher-terms] (do
                                              (println (str "this-coeff: " this-coeff 
                                                            ", higher-terms: " higher-terms))
                                              (* (+ (* higher-terms x)
                                                    this-coeff))))

              0
              coefficient-sequence))

;(horner-eval 2 (list 1 3 0 5 0 1))


; Exercise 2.35
; ????
;(defn count-leaves [t]
;  (accumulate + 0 (map <f> t)))
;
;(defn count-leaves [x]
;  (cond (empty? x) 0
;        (list? (first x)) (+ (count-leaves (first x))
;                             (count-leaves (rest x)))
;        :else (+ 1 (count-leaves (rest x)))))


; Exercise 2.36
(defn accumulate-n [op init seqs]
  (if (empty? (first seqs))
    nil
    (cons (accumulate op init (map first seqs))
          (accumulate-n op init (map rest seqs)))))

(def s [[1 2 3] [4 5 6] [7 8 9] [10 11 12]])
;(accumulate-n + 0 s)


; Exercise 2.37
(defn dot-product [v w]
  (accumulate + 0 (map * v w)))

(defn matrix-*-vector [m v]
  (map #(dot-product v %) m))

(def m [[1 2]
        [3 4]])
(def v [2 2])
;t0 = [2*1 + 2*2] => 6
;t1 = [2*3 + 2*4] => 14
;t = [6 14]
;(matrix-*-vector m v)

(defn transpose [m]
  (accumulate-n cons '() m))
;(transpose m)

(defn matrix-*-matrix [m n]
  (let [cols (transpose n)]
    (map #(matrix-*-vector cols %) m)))

[1 2   [1 2
 3 4]   3 4]

[ 7 10
 15 22]
;(matrix-*-matrix m m)


(defn fold-left [op init coll]
  (letfn [(iter [result cdr]
            (if (empty? cdr)
              result
              (recur (op result (first cdr))
                     (rest cdr))))]
    (iter init coll)))

(def fold-right accumulate)

(fold-right / 1 (list 1 2 3)) ; => 3/2
(fold-left / 1 (list 1 2 3)) ; => 1/6
(fold-right list nil (list 1 2 3)); => (1 (2 (3 nil)))
(fold-left list nil (list 1 2 3)); => 

(fold-right + 0 (list 1 2 3))
(fold-left + 0 (list 1 2 3))

; op should be commutative to guarantee that fold-right and fold-left will
; produce the same values for any sequence

; Exercise 2.39 skipped




