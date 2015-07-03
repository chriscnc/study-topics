(ns sicp.ch02-3)

(defn memq [item x]
  (cond (empty? x) false
        (= item (first x)) x
        :else (memq item (rest x))))

;(memq 'apple '(pear banana prune))
;(memq 'apple '(x (apple sauce) y apple pear))

; Exercise 2.53
;(list 'a 'b 'c)
;(list (list 'george))
;(rest '((x1 x2) (y1 y2)))
;(first (rest '((x1 x2) (y1 y2))))
;(memq 'red '((red shoes) (blue socks)))
;(memq 'red '(red shoes blue socks))


; Exercise 2.54
;(= '(this is a list) '(this is a list))
;(= '(this is a list) '(this (is a) list))
; There is no need to implement 'equal? in Clojure. = uses value-based equality.
(def equal? =)

;Exercise 2.55
;(car ''abracadabra) expands to....
;(car (quote (quote abracadabra))) => quote



