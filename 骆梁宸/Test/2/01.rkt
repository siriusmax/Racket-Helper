#lang racket
(define (for idx end fun)
  (if (= idx end)
      (void)
      (begin (fun idx)
             (for (+ 1 idx) end fun))))

(define (reverse vec)
  (define ret (vector))
  (begin (for 0 (vector-length vec)
           (lambda (idx)
             (set! ret (vector-append (vector (vector-ref vec idx)) ret)))
           )
         ret
         )
  )

(define (work)
  (let ((l (read)))
    (if (eq? l eof)
        (void)
        (begin (displayln (vector->list (reverse (list->vector l))))
               (work)))
    )
  )

(work)