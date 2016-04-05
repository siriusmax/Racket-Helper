#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;以下是put和get的实现,不须搞明白也能完成本题
(require scheme/mpair)
(define (make-table)
  (let ((local-table (mlist '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (massoc key-1 (mcdr local-table))))
        (if subtable
            (let ((record (massoc key-2 (mcdr subtable))))
              (if record
                  (mcdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (massoc key-1 (mcdr local-table))))
        (if subtable
            (let ((record (massoc key-2 (mcdr subtable))))
              (if record
                  (set-mcdr! record value)
                  (set-mcdr! subtable
                            (mcons (mcons key-2 value)
                                  (mcdr subtable)))))
            (set-mcdr! local-table
                      (mcons (mlist key-1
                                  (mcons key-2 value))
                            (mcdr local-table)))))
      (void))    
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

(define conversion-table (make-table))
(define get-coercion (conversion-table 'lookup-proc))
(define put-coercion (conversion-table 'insert-proc!))
;以上是put和get的实现，不须搞明白也能完成本题
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;------------- integer package
(define (install-integer-package)
  (define (tag x)
    (attach-tag 'integer x))    
  (put 'add '(integer integer)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(integer integer)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(integer integer)
       (lambda (x y) (tag (* x y))))
  (put 'div '(integer integer)
       (lambda (x y) ((get 'make 'rational )  x y)))
  (put 'make 'integer
       (lambda (x) (tag x)))
  (void))

(define (make-integer n)
  ((get 'make 'integer) n))


;--------general functions
  
(define (attach-tag type-tag contents)
  (cons type-tag contents))
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))
; 在此处补充你的代码
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((t1->t2 (get-coercion type1 type2))
                      (t2->t1 (get-coercion type2 type1)))
                  (cond (t1->t2
                         (apply-generic op (int->poly a1 (cadr a2)) a2))
                        (t2->t1
                         (apply-generic op a1 (int->poly a2 (cadr a1))))
                        (else
                         (error "No method for these types"
                                (list op type-tags))))))
              (error "No method for these types"
                     (list op type-tags)))))))

(define (int->poly i var)
  (build-poly (list var (list 0 (cdr i)))))

(define (install-polynomial-package)
  (define (tag x) (cons 'polynomial x))
  (define (variable p) (cadr p))
  (define (term-list p) (cddr p))
  (define (get-high-var p1 p2)
    (let((v1 (symbol->string (variable p1)))
         (v2 (symbol->string (variable p2))))
      (if(string<? v1 v2)
         (variable p1)
         (variable p2))))
  (define (raise-var p var)
    (if(eq? var (variable p))
       p
       (make-poly var (list (make-term 0 p)))))
  (define (add-terms L1 L2)
    (cond((null? L1) L2)
         ((null? L2) L1)
         (else
          (let((t1 (car L1)) (t2 (car L2)))
            (let((order1 (car t1)) (order2 (car t2)))
              (cond((> order1 order2)
                    (cons t1 (add-terms (cdr L1) L2)))
                   ((< order1 order2)
                    (cons t2 (add-terms L1 (cdr L2))))
                   (else
                    (cons (make-term order1 (add (cadr t1) (cadr t2)))
                          (add-terms (cdr L1) (cdr L2))))))))))
  (define (mul-terms L1 L2)
    (if (null? L1)
        '()
        (add-terms (mul-term-by-all-terms (car L1) L2)
                   (mul-terms (cdr L1) L2))))
  (define (mul-term-by-all-terms t1 L)
    (if (null? L)
        '()
        (let ((t2 (car L)))
          (cons
           (make-term (+ (car t1) (car t2))
                      (mul (cadr t1) (cadr t2)))
           (mul-term-by-all-terms t1 (cdr L))))))
  (define (add-poly p1 p2)
    (set! p1 (tag p1))
    (set! p2 (tag p2))
    (define high-var (get-high-var p1 p2))
    (set! p1 (raise-var p1 high-var))
    (set! p2 (raise-var p2 high-var))
    (make-poly high-var
               (add-terms (term-list p1)
                          (term-list p2))))
  (define (mul-poly p1 p2)
    (set! p1 (tag p1))
    (set! p2 (tag p2))
    (define high-var (get-high-var p1 p2))
    (set! p1 (raise-var p1 high-var))
    (set! p2 (raise-var p2 high-var))
    (make-poly high-var
               (mul-terms (term-list p1)
                          (term-list p2))))
    
  (put 'make 'polynomial
       (lambda (var terms)
         (tag (cons var terms))))
  (put 'make 'polynomial-term
       (lambda (order coeff) (list order coeff)))
  (put-coercion 'integer 'polynomial (lambda (x) (int->poly x 'z)))
  (put 'add '(polynomial polynomial) add-poly)
  (put 'mul '(polynomial polynomial) mul-poly)    
  (void))

(define (display-poly poly)
  (define (get-polylist poly)
    (cons (cadr poly) (map get-termlist (cddr poly))))
  (define (get-termlist term)
    (if(eq? 'integer (caadr term))
       (list (car term) (cdadr term))
       (list (car term) (get-polylist (cadr term)))))
  (displayln (get-polylist poly)))

(define (build-term x)
  (if(list? (cadr x))
     (make-term (car x) (build-poly (cadr x)))
     (make-term (car x) (make-integer (cadr x)))))

(define (build-poly polylist)
  (if(list? polylist)
     (make-poly (car polylist) (map build-term (cdr polylist)))
     (make-integer polylist)))
; 以上是补充的代码
(install-integer-package)
(install-polynomial-package)
(define (add x y) (apply-generic 'add x y))
(define (mul x y) (apply-generic 'mul x y))


(define (make-poly var terms)
  ((get 'make 'polynomial) var terms))
(define (make-term order coeff) 
  ((get 'make 'polynomial-term) order coeff))

(displayln "******1")
(define e1 (make-poly 'a (list (list 1 (make-integer 3)) (list 0 (make-integer 2))))) ;3a+2
(define e2 (make-poly 'a (list (list 2 (make-integer 4)) (list 1 (make-integer 3))))) ;4a^2 + 3a
(displayln e1)
(displayln e2)
(displayln (add e1 e2))
(displayln (mul e1 e2))

(displayln "******2")

(define c1 (make-poly 'b (list (list 1 (make-integer 3)) (list 0 (make-integer 2))))) ;3b+2
(define c2 (make-poly 'b (list (list 2 (make-integer 4)) (list 1 (make-integer 3))))) ;4b^2 + 3b

(define e3 (make-poly 'a (list (list 1 c1) (list 0 (make-integer 2))))) 
(define e4 (make-poly 'a (list (list 2 (make-integer 4)) (list 1 c2)))) 

(displayln (add e3 e4))

(displayln "******")
(define (myloop)
  (let ((a (read)))
    (if (eq? a eof)
        (void)
        (let ((op (car a))
              (e1 (cadr a))
              (e2 (caddr a)))
          (if (eq? op '+)
              (display-poly (add (build-poly e1) (build-poly e2)))
              (display-poly (mul (build-poly e1) (build-poly e2))))
          (myloop)))))
              
(myloop)