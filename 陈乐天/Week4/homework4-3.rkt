#lang racket


(define (createList a)
  (if (list? a)
      a
      (list a)))

(define (variable? exp) (symbol? exp))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (sum? exp)
  (define (findPlus now)
    (if (null? now)
        #f
        (if (eq? (car now) '+)
            #t
            (findPlus (cdr now)))))
  (if (pair? exp)
      (findPlus exp)
      #f))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2)) (else (append (createList a1) (list '+) (createList a2)))))

(define (addend exp)
  (define (plusBefore now)
    (if (eq? (car now) '+)
        '()
        (cons (car now) (plusBefore (cdr now)))))
  (define beforeExp (plusBefore exp))
  (if (null? (cdr beforeExp))
      (car beforeExp)
      beforeExp))

(define (augend exp)
  (define (plusAfter now)
    (define (plusAfterReally now)
      (if (null? now)
          '()
          (cons (car now) (plusAfterReally (cdr now)))))
    (if (eq? (car now) '+)
        (plusAfterReally (cdr now))
        (plusAfter (cdr now))))
  (define afterExp (plusAfter exp))
  (if (null? (cdr afterExp))
      (car afterExp)
      afterExp))

(define (product? exp) (and (pair? exp) (eq? (cadr exp) '*)))

(define (make-product a1 a2)
  (cond ((or (=number? a1 0) (=number? a2 0)) 0)
        ((=number? a1 1) a2)
        ((=number? a2 1) a1)
        ((and (number? a1) (number? a2)) (* a1 a2)) (else (list a1 '* a2))))

(define (multiplicand exp)
  (if (null? (cdddr exp))
      (caddr exp)
      (cddr exp)))

(define (multiplier exp) (car exp))

(define (=number? a b)
  (if (number? a)
      (if (= a b)
          #t
          #f)
      #f))

(define (deriv exp var)
  (cond ((number? exp ) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum 
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        (else 
         (error "wrong format"))))

(define (myloop)
  (let ((a (read)))
    (if (eq? a eof)
        (void)
        (begin (display (deriv a 'x)) (newline) (myloop)))))

(myloop)