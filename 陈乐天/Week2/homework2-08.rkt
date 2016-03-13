#lang racket


(define (dowork a now)
  (if (pair? a)
      (if (= (- (length a) 1) now)
          (if (= now 0)
              (list (dowork (list-ref a now) 0))
              (dowork (list-ref a now) 0))
          (if (= now 0)
              (list (append (dowork a (+ 1 now)) (dowork (list-ref a now) 0)))
              (append (dowork a (+ 1 now)) (dowork (list-ref a now) 0))))
      (list a)))
  
  

(define (myloop)
  (let ((a (read)))
    (if (eq? a eof)
        (void)
        (begin (displayln (car (dowork a 0))) (myloop)))))

(myloop)