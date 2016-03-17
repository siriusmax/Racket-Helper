#lang racket
(define (get-times0 x factor result)
  (if(= 0 (remainder x factor))
     (get-times0 (quotient x factor) factor (+ result 1))
     result))
(define (get-times x factor)
  (get-times0 x factor 0))

(define (car pair) (get-times pair 2))
(define (cdr pair) (get-times pair 3))

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