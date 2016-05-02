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
;===================
;问题在于没有考虑流为空的情况
(define (add-stream str1 str2)
  (cons-stream (+ (stream-car str1) (stream-car str2))
               (add-stream (stream-cdr str1) (stream-cdr str2))
               )
  )
(define (scale-stream str sc)
  (cons-stream (* (stream-car str) sc)
               (scale-stream (stream-cdr str) sc)
               )
  )
(define (RC R C dt)
  (define (getRC strI v0)
    
    (define getidtc  ; 1/c * i * dt
      ;(cons-stream (+ (stream-car strI) v0)
      (cons-stream v0
                   (add-stream (scale-stream strI (/ dt C))
                               getidtc)
                   )
      )
     (define getri
       (scale-stream strI R)
       )
    (add-stream getidtc getri)
    )
  getRC
  )

;===================
(define (integers-from n)
  (cons-stream n (integers-from (+ n 1))))
(define integers (integers-from 1))
(define fibs 
  (cons-stream 0
               (cons-stream 1
                            (add-stream fibs (stream-cdr fibs)))))
               
(define RC1 (RC 5 1 0.5))
;(display-stream-n (RC1 fibs 2) 10)
(displayln "******")
(display-stream-n (RC1 integers 2) 10)
(define RC2 (RC 15 10 0.2))
(displayln "******")
(display-stream-n (RC2 fibs 2) 10)
(displayln "******")
(display-stream-n (RC2 integers 2) 10)