#lang racket
(define inf 2147483647)

(define for
  (lambda (idx end func)
    (if (= idx end)
        (void)
        (begin (func idx)
               (for (+ 1 idx) end func))
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
(define getInput
  (lambda (vec)
    (let ((e (read)))
      (if (eq? e eof)
          vec
          (begin (set! vec (vector-append vec (vector e)))
                 (getInput vec)
                 )
          )
      )
    )
  )

(define merge
  (lambda (vec)
    (define mid 0)
    (define leftVec (vector))
    (define rightVec (vector))
    (define ret (vector))
    (if (= (vector-length vec) 1)
        vec
        (begin (set! mid (floor (/ (vector-length vec) 2)))
               (set! leftVec (merge (vector-take vec mid)))
               (set! rightVec (merge (vector-drop vec mid)))
               (loop (vector-length vec)
                     (lambda ()
                       (cond ((= 0 (vector-length leftVec))
                              (begin (set! ret (vector-append ret (vector-take rightVec 1)))
                                     (set! rightVec (vector-drop rightVec 1))))
                             ((= 0 (vector-length rightVec))
                              (begin (set! ret (vector-append ret (vector-take leftVec 1)))
                                     (set! leftVec (vector-drop leftVec 1))))
                             (else
                              (if (> (vector-ref leftVec 0) (vector-ref rightVec 0))
                                  (begin (set! ret (vector-append ret (vector-take rightVec 1)))
                                         (set! rightVec (vector-drop rightVec 1)))
                                  (begin (set! ret (vector-append ret (vector-take leftVec 1)))
                                         (set! leftVec (vector-drop leftVec 1)))
                                  ))
                             )
                       )
                     )
               ret
               )
        )
    )
  )

(define unique
  (lambda (vec)
    (define ret (vector))
    (begin (set! vec (vector-append (vector (- inf)) vec))
           (for 1 (vector-length vec)
             (lambda (idx)
               (if (= (vector-ref vec idx) (vector-ref vec (- idx 1)))
                   (void)
                   (set! ret (vector-append ret (vector (vector-ref vec idx))))
                   )
               )
             )
           ret
           )
    )
  )

(define showAns
  (lambda (vec)
    (for 0 (vector-length vec)
      (lambda (idx)
        (begin (display (vector-ref vec idx)) (display " "))
        )
      )
    )
  )

(define originalVec (vector))
(set! originalVec (getInput (vector)))
(showAns (unique (merge originalVec)))