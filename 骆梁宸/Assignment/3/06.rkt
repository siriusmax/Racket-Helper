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
  (define range (enumerate-interval 1 n))
  (define (getPermutation size result)
    (if (= size 3)
      result
      (getPermutation (+ 1 size) (flatmap (lambda (per) (map (lambda (i) (append per (list i))) range)) result))))
  ((lambda ()
    (flatmap (lambda (per) 
      (if (and (= s (accumulate + 0 per)) (< (first per) (second per) (third per)))
          (list per)
          empty))
      (getPermutation 0 (list empty)))))
  )

(define (myloop)
  (let ((n (read))
        (s (read)))
    (if (eq? n eof)
        (void)
        (begin (display (tri-num-list n s)) (newline) (myloop)))))

(myloop)