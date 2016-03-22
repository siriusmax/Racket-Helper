#lang racket
(define result (make-vector 93 0))
(define index 1)
(define (set-result! seq)
  (vector-set! result index seq)
  (set! index (+ 1 index))
  void)
(define (printn seq n)
  (if(= n 0)
     void
     (begin (display (car seq)) (printn (cdr seq) (- n 1)))))
(define (print-result i)
  (define seq (vector-ref result i))
  (printn seq 8)
  (newline)
  void)

(define (cango? line row seq)
  (define (cango0 cross1 cross2 row seqline seq)
    (if(null? seq)
       true
       (let(
         (result (or (= cross1 (+ seqline (car seq)))
                     (= cross2 (- seqline (car seq)))
                     (= row (car seq)))))
         (and (not result) (cango0 cross1 cross2 row (+ 1 seqline) (cdr seq))))))
  (define cross1 (+ line row))
  (define cross2 (- line row))
  (cango0 cross1 cross2 row 1 seq))
     

(define (preprocess line seq)
  (if(> line 8)
     (set-result! seq)
     (begin
       (if(cango? line 1 seq)
        (preprocess (+ line 1) (append seq (list 1)))void)
       (if(cango? line 2 seq)
        (preprocess (+ line 1) (append seq (list 2)))void)
       (if(cango? line 3 seq)
        (preprocess (+ line 1) (append seq (list 3)))void)
       (if(cango? line 4 seq)
        (preprocess (+ line 1) (append seq (list 4)))void)
       (if(cango? line 5 seq)
        (preprocess (+ line 1) (append seq (list 5)))void)
       (if(cango? line 6 seq)
        (preprocess (+ line 1) (append seq (list 6)))void)
       (if(cango? line 7 seq)
        (preprocess (+ line 1) (append seq (list 7)))void)
       (if(cango? line 8 seq)
        (preprocess (+ line 1) (append seq (list 8)))void))))

(define (loop)
  (define i (read))
  (if(eq? i eof)
     void
     (begin (print-result i) (loop))))

(define a (preprocess 1 '()))
(define b (read))

(define c (loop))
(newline)