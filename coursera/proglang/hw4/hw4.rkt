
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file



;; put your code below

(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))

(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (car (list-tail xs (remainder n (length xs))))]))
      
(define (stream-for-n-steps s n)
  (if (> n 0)
      (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1)))
      null))

(define funny-number-stream 
  (letrec ([f (lambda (x) 
                (if (= (remainder x 5) 0)
                    (cons (* x -1) (lambda () (f (+ x 1))))
                    (cons x (lambda () (f (+ x 1))))))])
    (lambda () (f 1))))

(define dan-then-dog
  (letrec ([dan (lambda () (cons "dan.jpg" dog))]
           [dog (lambda () (cons "dog.jpg" dan))])
    dan))

(define (stream-add-zero s)
  (letrec ([f (lambda (s)
                (cons (cons 0 (car (s)))
                      (lambda () (f (cdr (s))))))])
        (lambda () (f s))))

(define (cycle-lists xs ys)
  (letrec ([f (lambda (n)
                (cons (cons (list-nth-mod xs n) (list-nth-mod ys n))
                      (lambda () (f (+ n 1)))))])
    (lambda () (f 0))))

(define (vector-assoc v vec)
  (letrec ([vlen (vector-length vec)]
           [f (lambda (n) 
                (if (> n (- vlen 1))
                    #f
                    (let ([velem (vector-ref vec n)])
                      (cond [(not (pair? velem)) (f (+ n 1))]
                            [(equal? (car velem) v) velem]
                            [#t (f (+ n 1))]))))])
    (f 0)))

(define (cached-assoc xs n)
  (letrec ([cache (make-vector n)]
           [next-slot 0]
           [f (lambda (v)
              (let ([cached-elem (vector-assoc v cache)])
                (if cached-elem
                    cached-elem
                    (let ([elem (assoc v xs)])
                      (if elem
                          (begin (vector-set! cache next-slot elem)
                                 (set! next-slot (modulo (+ next-slot 1) n))
                                 elem)
                          #f)))))])
    f))
                