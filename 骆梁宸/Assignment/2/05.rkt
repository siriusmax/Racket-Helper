#lang racket

(define (car num)
  (define (car-iter num ret)
    (if (= (remainder num 2) 0)
        (car-iter (/ num 2) (+ 1 ret))
        ret))
  (car-iter num 0))

(define (cdr num)
  (define (cdr-iter num ret)
    (if (= (remainder num 3) 0)
        (cdr-iter (/ num 3) (+ 1 ret))
        ret))
  (cdr-iter num 0))

(define (fast-exp a n)
  (define (square x) (* x x))
  (define (iter a n result)
    (if (= n 0)
        result
        (if (even? n) 
            (iter (square a) (/ n 2) result)
            (iter (square a) (/ (- n 1) 2) (* a result)))))
  (iter a n 1))
  
(define (cons a b)
  (* (fast-exp 2 a) (fast-exp 3 b)))

(define (myloop)
  (let ((a (read))
        (b (read)))
    (if (eq? a eof)
        (void)
        (begin (display (car (cons a b)))
               (display " ")
               (display (cdr (cons a b)))
               (newline) 
               (myloop)))))

(myloop)