#lang racket
(require r5rs)

(define env (scheme-report-environment 5))
(eval  ;only after evaluating your count-pairs, the program in the input can use count-pairs
 '
 (begin
   (define counted '())
   (define (count-pairs x)
     (define (not-counted? x)
       (define (not-counted-single x now)
         (if (null? now)
             #t
             (if (eq? x (car now))
                 #f
                 (not-counted-single x (cdr now)))))
       (not-counted-single x counted))
     (if (not (pair? x))
         0
         (if (not-counted? x)
             (begin
               (set! counted (append counted (list x)))
               (+ 1
                  (count-pairs (car x))
                  (count-pairs (cdr x))))
             0))))
 env)

(define (myloop)
  (define (eval-codes codes last-val)
    (if (null? codes)
        last-val
        (eval-codes (cdr codes) (eval (car codes) env))))
    
  (let ((codes (read)))
    (if (eq? codes eof)
        (void)
        (begin (displayln (eval-codes codes (void))) (myloop)))))


(myloop)