#lang racket


(define (dowork num)
  (if (null? num)
      empty
      (append (dowork (cdr num)) (list (car num)))))

(define (myloop)
  (let ((num (read)))
    (if (eq? num eof)
        (void)
        (begin (displayln (dowork num)) (myloop)))))
(myloop)