#lang racket
(define (cont-frac-iter N D k)
  (define (solve-iter N D k idx result)
    (if (= idx 0)
        result
        (solve-iter N D k (- idx 1) (/ (N idx) (+ (D idx) result))))
    )
  (solve-iter N D k k 0))

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