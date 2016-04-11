#lang racket
(require r5rs)
(define env (scheme-report-environment 5))

(eval '(define (last-pair lst)
         (if (null? (cdr lst))
             lst
             (last-pair (cdr lst))))
      env)

(eval '(define (make-cycle lst)
         (set-cdr! (last-pair lst) lst)
         lst)
      env)

(eval '

 (begin
   (define counted '())
   (define (check-cycle x)
     (define (not-counted? x)
       (define (not-counted-single x now)
         (if (null? now)
             #t
             (if (eq? x (car now))
                 #f
                 (not-counted-single x (cdr now)))))
       (not-counted-single x counted))
     (if (null? x)
         #f
         (if (not-counted? x)
             (begin
               (set! counted (append counted (list x)))
               (check-cycle (cdr x)))
             #t))))
      
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