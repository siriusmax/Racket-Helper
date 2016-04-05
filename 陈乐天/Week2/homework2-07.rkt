#lang racket

(define (dowork a)
  (if (pair? a)
      (append (dowork (car a)) (dowork (cdr a)))
      (if (null? a)
          empty
          (list a))))


(define (myloop)
  (let ((a (read)))
    (if (eq? a eof)
        (void)
        (begin (displayln (dowork a)) (myloop)))))

(myloop)