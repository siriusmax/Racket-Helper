#lang racket

(define (even? n)
  (= (remainder n 2) 0))
(define (square x)
  (* x x))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (loop)
  (define a (read))
  (define n (read))
  (if(equal? a eof)
     (void)
     (begin (display (fast-expt a n)) (newline) (loop))))
(loop)