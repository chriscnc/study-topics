(ns sicp.huffman)

(defn third [x] (nth x 2))
(defn fourth [x] (nth x 3))
(defn make-leaf [sym weight]
  (list 'leaf sym weight))

(defn leaf? [obj]
  (= (first obj) 'leaf))

(defn symbol-leaf [x] (second x))
(defn weight-leaf [x] (third x))

(defn left-branch [tree] (first tree))
(defn right-branch [tree] (second tree))

(defn symbols [tree]
  (if (leaf? tree)
    (list (symbol-leaf tree))
    (third tree)))

(defn weight [tree]
  (if (leaf? tree)
    (weight-leaf tree)
    (fourth tree)))


(defn make-code-tree [left right]
  (list left
        right
        (concat (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(defn choose-branch [bit branch]
  (cond (= bit 0) (left-branch branch)
        (= bit 1) (right-branch branch)
        :else (Exception. (str "bad bit == CHOOSE-BRANCH" bit))))


(defn decode [bits tree]
  (letfn [(decode-1 [bits current-branch]
            (if (empty? bits)
              '()
              (let [next-branch (choose-branch (first bits) current-branch)]
                (if (leaf? next-branch)
                  (cons (symbol-leaf next-branch)
                        (decode-1 (rest bits) tree))
                  (decode-1 (rest bits) next-branch)))))]
    (decode-1 bits tree)))

(defn adjoin-set [x s]
  (cond (empty? s) (list x)
        (< (weight x) (weight (first s))) (cons x set)
        :else (cons (first s)
                    (adjoin-set x (rest s)))))


(defn make-leaf-set [pairs]
  (if (empty? pairs)
    '()
    (let [pair (first pairs)]
      (adjoin-set (make-leaf (first pair) ; symbol
                             (second pair)) ; frequency
                  (make-leaf-set (rest pairs))))))

; Exercise 2.67
(def sample-tree (make-code-tree (make-leaf 'A 4)
                                 (make-code-tree
                                   (make-leaf 'B 2)
                                   (make-code-tree (make-leaf 'D 1)
                                                   (make-leaf 'C 1)))))

(def sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
(def msg (decode sample-message sample-tree))

; Exercise 2.68
(defn exists? [x coll]
  (cond (empty? coll) false
        (= x (first coll)) true
        :else (recur x (rest coll))))


(defn encode-symbol [sym tree]
  (cond (leaf? tree) '()
        (exists? sym (symbols (left-branch tree))) (cons 0 (encode-symbol sym (left-branch tree)))
        (exists? sym (symbols (right-branch tree))) (cons 1 (encode-symbol sym (right-branch tree)))))


(defn encode [msg tree]
  (if (empty? msg)
    '()
    (concat (encode-symbol (first msg) tree)
            (encode (rest msg) tree))))

;(encode msg sample-tree)

; Exercise 2.69-2.72 - skipped
