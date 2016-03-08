#lang racket

(define (loopn n proc)
  (if(= 0 n)
     (void)
     (begin (proc) (loopn (- n 1) proc))))

(define (processline)
  (define n (read))
  (define max (read))
  (define (flashmax)
    (define tmp (read))
    (if(> tmp max)
       (begin (set! max tmp) void)
       (void)))
  (loopn (- n 1) flashmax)
  (display max)
  (newline)
  void)
    
(define n (read))
(loopn n processline)
