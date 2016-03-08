#lang racket
(let ((n (read)))
(begin (define (myloop n)
  
  (define (work)
    (let ((m (read))
          (max (read))
          )
      (begin
        (define (innerloop m max)
          (if (= m 1)
              max
             (let ((now (read)))
               (if (> now max)
                   (innerloop (- m 1) now)
                   (innerloop (- m 1) max))))
          )
      (innerloop m max)
      )
    )
    

   )
  (if (= n 0)
      (void)
      (begin (displayln (work)) (myloop (- n 1)))
      )
  )
  (myloop n))
  )