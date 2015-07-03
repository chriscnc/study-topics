(ns sicp.bin-tree-sets)

(defn entry [t] (first t))
(defn left-branch [t] (second t))
(defn right-branch [t] (nth t 2))
(defn make-tree [entry left right] (list entry left right))

(defn element-of-set? [x s]
  (cond (empty? s) false
        (= x (entry s)) true
        (< x (entry s)) (element-of-set? x (left-branch s))
        (> x (entry s)) (element-of-set? x (right-branch s))))

(element-of-set? 3 [5 [3 [] []] [6 [] []]])


(defn adjoin-set [s x]
  (cond (empty? s) (make-tree x '() '())
        (= x (entry s)) s
        (< x (entry s)) (make-tree (entry s) 
                                   (adjoin-set (left-branch s) x)
                                   (right-branch s))
        (> x (entry s)) (make-tree (entry s)
                                   (left-branch s)
                                   (adjoin-set (right-branch s) x))))
(-> (adjoin-set '() 5)
     (adjoin-set 3)
     (adjoin-set 7)
     (adjoin-set 1)
     (adjoin-set 6))

(reduce adjoin-set '() (range 1 8))

; Exercise 2.63
(defn tree2list-1 [t]
  (if (empty? t) 
    '()
    (concat (tree2list-1 (left-branch t))
            (cons (entry t)
                  (tree2list-1 (right-branch t))))))


(defn tree2list-2 [t]
  (letfn [(copy-to-list [t result]
            (if (empty? t)
              result
              (copy-to-list (left-branch t)
                            (cons (entry t)
                                  (copy-to-list (right-branch t)
                                                result)))))]
    (copy-to-list t '())))

(def t1 '(7 (3 (1 () ()) (5 () ())) (9 () (11 () ()))))
(def t2 '(3 (1 () ()) (7 (5 () ()) (9 () (11 () ())))))
(def t3 '(5 (3 (1 () ()) ()) (9 (7 () ()) (11 () ()))))

(tree2list-1 t1)
(tree2list-2 t1)
(tree2list-1 t2)
(tree2list-2 t2)
(tree2list-1 t1)
(tree2list-2 t2)

; Exercise 2.63a
; The procedures appear to product exact same list for all trees in figure 2.16
; The traversal appears to be the same for both procedures (left, entry, right) so
; I would expect the results to be the same for all trees. It also appears that
; Scheme's append function behaves the same as Clojure's concat, so maybe the 
; 'append' used in this exercise it is one that doesn't preserve the order of the
; lists ????
; Exercise 2.63b
; not sure.

; Exercise 2.64 - skipped
; Exercise 2.65 - skipped
