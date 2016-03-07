#lang racket
(define (cont-frac-iter N D k)
  (define (doCal N D i)
    (if (= i k)
        (/ (N k) (D k))
        (/ (N k) (+ (D k) (doCal N D (+ i 1)))))
    )
  (doCal N D 1))

(cont-frac-iter (lambda (x) x) 
           (lambda (x) 1.0)
           30)
 
(cont-frac-iter (lambda (x) (* 2 x))
           (lambda (x) (* 1.0 x))
           30)

(display "********") (newline)
(define (myloop)
  (let ((k (read)))
    (if (eq? k eof)
        (void)
        (begin (display (cont-frac-iter (lambda (x) 1.0) (lambda (x) 1.0) k)) 
(newline) (myloop)))))

(myloop)