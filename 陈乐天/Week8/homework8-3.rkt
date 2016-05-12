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
  (if (= n 1)
      (display (stream-car s))
      (begin (display (stream-car s)) (display " ") (display-stream-n (stream-cdr s) (- n 1)))))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))
(define (add-stream lhs rhs)
  (cons-stream 
    (+ (stream-car lhs) (stream-car rhs))
    (add-stream (stream-cdr lhs) (stream-cdr rhs))))
(define (integral delayed-integrand initial-value dt)
  (define int
    (cons-stream
     initial-value
     (let ((integrand (force delayed-integrand)))
       (add-stream (scale-stream integrand dt) int))))
  int)

(define (solve-2nd a b dt y0 dy0)
  (define dy (integral (delay ddy) dy0 dt))
  (define y (integral dy y0 dt))
  (define ddy (add-stream (scale-stream dy a) (scale-stream y b)))
  y)


(define (myloop)
  (let ((a (read)))
    (if (eq? a eof)
        (void)
        (let ((b (read))
              (dt (read))
              (y0 (read))
              (dy0 (read))
              (n (read)))
          (begin (display-stream-n (solve-2nd a b dt y0 dy0) n) (newline) (myloop))))))

(myloop)