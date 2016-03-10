#lang racket
(define (square a) (* a a))
(define (fastExp base p)
  (define half (floor (/ p 2)))
  (if (= 1 p)
      base
      (if (even? p)
          (square (fastExp base half))
          (* base (square (fastExp base half)))))
  )

(define (work)
  (let ((base (read)) (p (read)))
    (if (eq? base eof)
        (void)
        (begin (displayln (fastExp base p))
               (work)))))

(work)