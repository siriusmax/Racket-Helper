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
;
      (begin
        (define pairs-counted '())
        (define (check-cycle x)
          (define (count x)
            (set! pairs-counted (cons x pairs-counted)))
          (define (pair-not-counted x)
            (define (p0 x now)
              (cond((null? now) #t)
                   ((eq? (car now) x) #f)
                   (else (p0 x (cdr now)))))
            (p0 x pairs-counted))
          (if(null? x)
             #f
             (if(pair-not-counted x)
                (begin (count x) (check-cycle (cdr x)))
                #t))))
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