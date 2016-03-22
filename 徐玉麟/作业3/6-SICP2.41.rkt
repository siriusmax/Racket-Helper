#lang racket

(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq) (accumulate op init (cdr seq)))))

(define (enumerate-interval a b)
  (if (> a b)
      '()
      (cons a (enumerate-interval (+ a 1) b))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))


(define (tri-num-list n s)
  (define (tnl0 count seq rest)
    (if(and (= 0 count) (= 0 rest))
       (list '())
       (if(or (<= rest 0) (null? seq))
          '()
          (append
           (map (lambda (x) (cons (car seq) x)) (tnl0 (- count 1) (cdr seq) (- rest (car seq))))
           (tnl0 count (cdr seq) rest)))))
  (tnl0 3 (enumerate-interval 1 n) s))

  
(define (myloop)
  (let ((n (read))
        (s (read)))
    (if (eq? n eof)
        (void)
        (begin (display (tri-num-list n s)) (newline) (myloop)))))

(myloop)