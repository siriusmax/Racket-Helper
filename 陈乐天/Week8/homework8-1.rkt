#lang racket
(define (square x) (* x x))
(define (divisible? x y ) (= (remainder x y ) 0))

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define-syntax cons-stream
  (syntax-rules ()
    [(cons-stream x y) (cons x (delay y))]))
 
(define the-empty-stream '())
 
(define (stream-null? stream)
  (null? stream))

(define (stream-ref s n)  ;取 stream里面第 n 项,n从0开始算
  (if (stream-null? s) the-empty-stream
      (if (= n 0)
          (stream-car s)
          (stream-ref (stream-cdr s) (- n 1)))))
 
(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s)) 
                   (stream-map proc (stream-cdr s)))))


(define (merge-weighted s1 s2 weight-function)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (let ((s1weight (weight-function s1car))
                 (s2weight (weight-function s2car)))
             (cond ((< s1weight s2weight) (cons-stream s1car (merge-weighted (stream-cdr s1) s2 weight-function)))
                   ((>= s1weight s2weight) (cons-stream s2car (merge-weighted s1 (stream-cdr s2) weight-function)))))))))
(define (weighted-pairs s t weight-function)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weighted
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (weighted-pairs (stream-cdr s) (stream-cdr t) weight-function)
    weight-function)))
(define (Ramanujan lst)
  (let ((one (stream-car lst))
        (two (stream-car (stream-cdr lst))))
    (if (= (weight3 one) (weight3 two))
        (cons-stream (weight3 one) (Ramanujan (stream-cdr lst)))
        (Ramanujan (stream-cdr lst)))))


(define (integers-from n)
  (cons-stream n (integers-from (+ n 1))))
(define integers (integers-from 1))
(define (cube x)  (* x x x))
(define weight3 (lambda (x) (+ (cube (car x)) (cube (cadr x)))))
(define lst (weighted-pairs integers integers weight3)) 
(define result-stream  (Ramanujan lst))



(define (myloop)
  (let ((n (read)))
    (if (eq? n eof)
        (void)
        (begin (displayln (stream-ref result-stream n)) (myloop)))))

(myloop)