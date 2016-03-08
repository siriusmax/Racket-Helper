#lang racket
(define inf 2147483647)

(define for
  (lambda (idx end func)
    (if (= idx end)
        (void)
        (begin (func idx)
               (for (+ idx 1) end func)
               )
        )
    )
  )

(define readSingleNumber
  (lambda (current)
    (let ((newNumber (read)))
      (cond ((> newNumber current) newNumber)
            (else current)
            )
      )
    )
  )

(define readLine
  (lambda (redu)
    (let ((M (read)))
      (begin (define ans (- inf))
             (for 0 M
               (lambda (redu)
                 (set! ans (readSingleNumber ans))
                 )
               )
             (displayln ans)
             )
      )
    )
  )

(define exec
  (lambda ()
    (let ((N (read)))
      (for 0 N readLine)
      )
    )
  )

(exec)