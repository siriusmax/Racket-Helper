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

(define (check-cycle lst)
 (define (last-pair lst)
         (if (null? (cdr lst))
             lst
             (last-pair (cdr lst))))
(define (make-cycle lst)
         (set-cdr! (last-pair lst) lst)
         lst)
(define (existeq? x lst)
  (if (null? lst)
      #f
      (if (eq? x (car lst))
          #t
          (existeq? x (cdr lst))
          )
      )
  )
  (define (check-iter inlst outlst)
    (cond ((null? inlst) #f)
          ((pair? inlst) (if (existeq? inlst outlst)
                             #t
                             (or (check-iter (car inlst) (append outlst (list inlst)))
                                 (check-iter (cdr inlst) (append outlst (list inlst)))
                                 )
                             )
                         )
          (else #f)
          )
    )
  (check-iter lst '())
  )
      
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
(check-cycle (list 1 2 3 4 5 6))
(define x4 (list (list 1) 2))  (set-cdr! (car x4) (cdr x4))   (check-cycle x4)

(define e1 (cons 'a '())) (define e2 (cons e1 e1)) (define e7 (cons e2 e2)) (check-cycle e7)
(define clst (make-cycle (list 1 2))) (check-cycle clst)
|#