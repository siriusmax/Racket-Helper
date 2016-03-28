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
  (define (gen-all a b n)
    (if(<= n 0)
       (list '())
       (if(> a b)
          '()
          (flatmap (lambda (x) (map (lambda (tuple) (cons x tuple)) (gen-all (+ 1 x) b (- n 1)))) (enumerate-interval a b)))))
  (filter (lambda (tuple) (= s (apply + tuple))) (gen-all 1 n 3)))

  
(define (myloop)
  (let ((n (read))
        (s (read)))
    (if (eq? n eof)
        (void)
        (begin (display (tri-num-list n s)) (newline) (myloop)))))

(myloop)