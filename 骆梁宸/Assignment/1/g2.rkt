#lang racket
(define for
  (lambda (idx end fun)
    (if (= idx end)
        (void)
        (begin (fun idx)
               (for (+ 1 idx) end fun))))
  )

(define loop
  (lambda (times fun)
    (for 0 times (lambda (redu) (fun)))))

(define (printAns line size)
  (loop line (lambda ()
               (for 0 size
                 (lambda (idx)
                   (begin (display (+ 1 idx))
                          (display ",")
                          (cond ((= idx (- size 1)) (newline))
                                (else (void))))))
               )))

(define (work)
  (let ((line (read)) (size (read)))
    (if (eq? line eof)
        (void)
        (begin (printAns line size)
               (work)))
    )
  )

(work)