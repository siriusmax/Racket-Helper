#lang racket

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

(define (display-stream-n s n)
  (if (= n 0)
      (void)
      (begin (displayln (stream-car s)) (display-stream-n (stream-cdr s) (- n 1)))))


(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))
(define (add-stream lhs rhs)
  (cons-stream 
    (+ (stream-car lhs) (stream-car rhs))
    (add-stream (stream-cdr lhs) (stream-cdr rhs))))
(define (integral integrand initial-value dt)
  (define int
    (cons-stream 
      initial-value
      (add-stream 
        (scale-stream integrand dt)
        int)))
  int)
(define (RC R C dt)
  (lambda (i u0)
    (add-stream (integral (scale-stream i (/ 1 C)) u0 dt) (scale-stream i R))))

(define (integers-from n)
  (cons-stream n (integers-from (+ n 1))))
(define integers (integers-from 1))
(define fibs 
  (cons-stream 0
               (cons-stream 1
                            (add-stream fibs (stream-cdr fibs)))))
               
(define RC1 (RC 5 1 0.5))
(display-stream-n (RC1 fibs 2) 10)
(displayln "******")
(display-stream-n (RC1 integers 2) 10)
(define RC2 (RC 15 10 0.2))
(displayln "******")
(display-stream-n (RC2 fibs 2) 10)
(displayln "******")
(display-stream-n (RC2 integers 2) 10)