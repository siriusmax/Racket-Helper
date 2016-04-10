#lang racket
(require r5rs)

(define env (scheme-report-environment 5))
(eval  ;only after evaluating your count-pairs, the program in the input can use count-pairs
 '
 ;
 (begin
  (define pairs-counted '())
 (define (count-pairs x)
   (define (count x)
     (set! pairs-counted (cons x pairs-counted)))
   (define (pair-not-counted x)
     (define (p0 x now)
       (cond((null? now) #t)
            ((eq? (car now) x) #f)
            (else (p0 x (cdr now)))))
     (p0 x pairs-counted))
   (if(not (pair? x))
      0
      (if(pair-not-counted x)
         (begin (count x)
                (+ 1
                   (count-pairs (car x))
                   (count-pairs (cdr x))))
         0))))
 ;
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