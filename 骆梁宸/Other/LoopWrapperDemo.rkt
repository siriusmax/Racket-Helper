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

(define loop
  (lambda (times func)
    (for 0 times
      (lambda (redu) (func))
      )
    )
  )

(define while
  (lambda (judgeFunc func)
    (if (judgeFunc)
        (begin (func)
               (while judgeFunc func))
        (void))
    )
  )

(for 0 5
  (lambda (idx) (display idx)))
(newline)

(loop 5
      (lambda () (display 1)))
(newline)

(define a 0)
(while (lambda () (< a 5))
       (lambda () (begin (display 1)
                         (set! a (+ 1 a)))))
(newline)