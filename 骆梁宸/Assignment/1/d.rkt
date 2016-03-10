#lang racket
(define (for idx end fun)
  (if (= idx end)
      (void)
      (begin (fun idx)
             (for (+ 1 idx) end fun))))

(define fib (make-vector 51))

(for 0 5 (lambda (idx)
           (vector-set! fib idx 1)))

(for 5 51 (lambda (idx)
            (vector-set! fib idx
                         (+ (vector-ref fib (- idx 1))
                            (* 4 (vector-ref fib (- idx 2)))
                            (* 5 (vector-ref fib (- idx 3)))
                            (- (* 2 (vector-ref fib (- idx 4)) (vector-ref fib (- idx 4))))
                            (* (vector-ref fib (- idx 5)) (vector-ref fib (- idx 5)) (vector-ref fib (- idx 5))))
                         )))

(define (work)
  (let ((n (read)))
    (if (eq? n eof)
        (void)
        (begin (displayln (vector-ref fib n))
               (work))))
  )

(work)