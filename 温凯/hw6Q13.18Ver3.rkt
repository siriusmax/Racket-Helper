#lang racket
(require r5rs)

(define env (scheme-report-environment 5))
(eval  ;only after evaluating your count-pairs, the program in the input can use count-pairs
 '
(define (count-pairs lst)
  (define (existeq? x lst)
  (if (null? lst)
      #f
      (if (eq? x (car lst))
          #t
          (existeq? x (cdr lst))
          )
      )
  )
  (define (genCiteLstSet inlst)
    (define outlist '())
    (define (gcls-iter inlist)
      (cond ((null? inlist) '())
            ((pair? inlist) (if (existeq? inlist outlist)
                                '()
                                (begin (set! outlist (append outlist (list inlist)))
                                       (gcls-iter (car inlist))
                                       (gcls-iter (cdr inlist))
                                                  )
                                       )
                                )
            (else '())
            )
      )
    (gcls-iter inlst)
    outlist
    )
            
  (length (genCiteLstSet lst)))


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




  
#|
(define tplst (genCiteLst (list 1 2 3 4 5 6)))
(length (genUniCiteLst tplst))
(define x4 (list (list 1) 2))  (set-cdr! (car x4) (cdr x4))
(length (genUniCiteLst (genCiteLst x4)))
(define e1 (cons 'a '())) (define e2 (cons e1 e1)) (define e7 (cons e2 e2)) (count-pairs e7)


(define (mymember_eq? x lst)
  (if (null? lst)
      #f
      (if (list? lst)
         (> 0
            (count (lambda (y) (eq? x y)) lst)
         )
         (let ((mylst (mymlist->list lst)))
           (> 0
            (count (lambda (y) (eq? x y)) mylst)
            )
           )
      )
  )
  )
|#

; 反思：早点用自己写的判断是否存在的函数就好了
                                         