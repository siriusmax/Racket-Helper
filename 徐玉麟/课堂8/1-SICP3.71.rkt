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
; 在此处补充你的代码
(define (weighted-pairs listi listj calw)
  (define (wp0 listi listj originj calw)
    (let((i (stream-car listi))
         (j (stream-car listj)))
      (if(= i j)
         (cons-stream (calw (list i j)) (wp0 (stream-cdr listi) originj originj calw))
         (cons-stream (calw (list i j)) (wp0 listi (stream-cdr listj) originj calw)))))
  (wp0 listi listj listj calw))

(define (Ramanujan lst)
  (define (listin alist ele)
    (cond((null? alist) #f)
         ((equal? (car alist) ele) #t)
         (else (listin (cdr alist) ele))))
       
  (define (r0 geted checked rested)
    (let ((head (stream-car rested))
          (tail (stream-cdr rested)))
      (cond((listin geted head)
            (r0 geted checked tail))
           ((listin checked head)
            (cons-stream head (r0 (cons head geted) checked tail)))
           (else (r0 geted (cons head checked) tail)))))
  (r0 '() '() lst))
           
; 以上
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