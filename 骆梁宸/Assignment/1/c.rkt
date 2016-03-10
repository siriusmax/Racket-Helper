#lang racket

(define for
  (lambda (idx end func)
    (if (= idx end)
        (void)
        (begin (func idx)
               (for (+ 1 idx) end func)
               )
        )
    )
  )

(define loop
  (lambda (times func)
    (for 0 times
      (lambda (redu) (func))
      )
    )
  )

;main
(define pascalTriangle (vector))
(define (init)
  (begin (set! pascalTriangle (vector-append pascalTriangle (vector (vector))))
         (for 1 101
           (lambda (line)
             (define tmpVec (vector))
             (begin (for 0 line
                      (lambda (idx)
                        (cond ((= idx 0)
                               (set! tmpVec (vector-append tmpVec (vector 1))))
                              ((= idx (- line 1))
                               (set! tmpVec (vector-append tmpVec (vector 1))))
                              (else
                               (set! tmpVec (vector-append tmpVec
                                                           (vector (+ (vector-ref (vector-ref pascalTriangle (- line 1)) idx)
                                                                      (vector-ref (vector-ref pascalTriangle (- line 1)) (- idx 1)))
                                                                   ))))
                              )
                        )
                      )
                    (set! pascalTriangle (vector-append pascalTriangle (vector tmpVec)))
                    )
             )
           )
         )
  )

(define (displayTriangle x)
  (for 0 x
    (lambda (line)
      (define vec (vector-ref pascalTriangle (+ 1 line)))
      (for 0 (+ 1 line)
        (lambda (idx)
          (begin (display (vector-ref vec idx))
                 (if (= idx line)
                     (newline)
                     (display " ")
                     )
                 )
          )
       )
      )
    )
  )

(init)
(define (work)
  (let ((a (read)))
    (if (eq? a eof)
        (void)
        (begin (displayTriangle a)
               (work))
        )
    )
  )

(work)