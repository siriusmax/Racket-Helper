#lang racket
(define (square a) (* a a))
(define (fastExp p currentBase result)
  (define half (floor (/ p 2)))
  (if (= 0 p)
      (displayln result)
      (if (even? p)
          (fastExp half (square currentBase) result)
          (fastExp half (square currentBase) (* result currentBase))))
  )

(define (work)
  (let ((base (read)) (p (read)))
    (if (eq? base eof)
        (void)
        (begin (fastExp p base 1)
               (work)))))

(work)