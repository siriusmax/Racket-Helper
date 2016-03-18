#lang racket
(define (map op lst)
  (if (null? lst)
      '()
      (cons (op (car lst))
            (map op (cdr lst)))))

(define (super-map op . w)
  (define (accumulate op initial sequence) (if (null? sequence)
        initial
        (op (car sequence)
(accumulate op initial (cdr sequence)))))
  (define (superr-map op w)
    (define (cal) (accumulate op 0 (map (lambda (lst) (car lst)) w)))
    (define (end? w)
      (define a (car w))
      (empty? a))
    (if (end? w)
        empty
        (append (list (cal)) (superr-map op (map (lambda (lst) (cdr lst)) w)))))
  (superr-map op w))



(define (myloop)
  (let ((a (read))
        (b (read))
        (c (read)))
    (if (eq? a eof)
        (void)
        (begin (displayln (super-map + a b c)) 
               (displayln (super-map (lambda (x y) (+ x (* 2 y) )) a b ))
               (myloop)))))
(myloop)