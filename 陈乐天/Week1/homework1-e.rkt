#lang racket

(define (qpower a n)
  (define (even? number)
    (if (= (remainder number 2) 0)
        #t
        #f))
  (define (square x) (* x x))
  (cond ((= n 0) 1)
        ((even? n) (square (qpower a (/ n 2))))
        (else (* a (square (qpower a (/ (- n 1) 2)))))))


(define (loop)
  (let ((a (read))
        (n (read)))
    (if (eq? a eof)
        (void)
        (begin (display (qpower a n)) (display #\newline) (loop)))))

(loop)