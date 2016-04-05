#lang racket

(define (isEven? n)
  (if (= (remainder n 2) 0)
      #t
      #f))

(define (square x) (* x x))

(define (qpower a n now result)
  (if (= n 0)
      (begin (display result) (display #\newline))
      (begin
        (if (isEven? n)
            (qpower a (quotient n 2) (square now) result)
            (qpower a (quotient n 2) (square now) (* result now))))))

(define (myloop)
  (let ((a (read))
        (n (read)))
    (if (eq? a eof)
        (void)
        (begin
          (qpower a n a 1)
          (myloop)))))

(myloop)