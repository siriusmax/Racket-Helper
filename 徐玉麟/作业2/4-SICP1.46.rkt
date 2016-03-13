#lang racket
(define (average x y) (/ (+ x y) 2))

(define (iterative-improve good-enough improve)
  (define (iter x)
    (if(good-enough x)
       x
       (iter (improve x))))
  (lambda (x) (iter x)))
  
(define (sqrt a)
  (define (gd x)
    (< (abs (- x (average x (/ a x)))) 0.0001))
  (define (im x)
    (average x (/ a x)))
  ((iterative-improve gd im) 1))

(define (myloop)
  (let ((n (read)))
    (if (eq? n eof)
        (void)
        (begin (display (sqrt n)) 
               (newline) (myloop)))))

(myloop)