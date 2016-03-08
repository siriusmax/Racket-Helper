#lang racket

(define (even? n)
  (= (remainder n 2) 0))
(define (square x)
  (* x x))

(define (revertbit n revert)
  (if(= n 0)
     revert
     (if(even? n)
        (revertbit (quotient n 2) (* revert 2))
        (revertbit (quotient n 2) (+ 1 (* revert 2))))))
  

(define (fast-expt b n)
  (define revertn (revertbit n 0))
  (define (fe0 b n ren nowb)
    (cond ((= n 0) nowb)
          ((even? ren) (fe0 b (quotient n 2) (quotient ren 2) (square nowb)))
          (else (fe0 b (quotient n 2) (quotient ren 2) (* b (square nowb))))))
  (fe0 b n revertn 1))

(define (loop)
  (define a (read))
  (define n (read))
  (if(equal? a eof)
     (void)
     (begin (display (fast-expt a n)) (newline) (loop))))
(loop)