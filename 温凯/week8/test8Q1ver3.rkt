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

;=================
;// 在此处补充你的代码
(define (merge-weighted s1 s2 weight)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1uw (weight (stream-car s1)))
               (s2uw (weight (stream-car s2)))
               (s1u (stream-car s1))
               (s2u (stream-car s2)))
           (cond ((<= s1uw s2uw) (cons-stream s1u (merge-weighted (stream-cdr s1) s2 weight)))
                 ((> s1uw s2uw) (cons-stream s2u (merge-weighted s1 (stream-cdr s2)  weight))) ;不要求去重
                 )
           )
         )
        )
  )

;这里是如何调用的，不看答案真想不出来,把merge-weighted和weighted-pairs放在一起
(define (weighted-pairs ls ls2 weightProc)

    (cons-stream
     (list (stream-car ls) (stream-car ls2))
     (merge-weighted
      (stream-map (lambda (x) (list (stream-car ls) x)) (stream-cdr ls2))
      (weighted-pairs (stream-cdr ls) (stream-cdr ls2) weightProc)
      weightProc
      )
     )
  )
    
  

(define (Ramanujan str)
  (let ((this (weight3 (stream-car str)))
        (next (weight3 (stream-car (stream-cdr str)))))
    (if (= this next)
        (cons-stream this (Ramanujan (stream-cdr (stream-cdr str))))
        (Ramanujan (stream-cdr str))
        )
    )
  )


;==================
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
