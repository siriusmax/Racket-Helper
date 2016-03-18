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
  (define (make-triple)
    (flatmap (lambda (i)
               (flatmap (lambda (j)
                      (map (lambda (k)
                             (list i j k))
                           (enumerate-interval (+ j 1) n)))
                    (enumerate-interval (+ i 1) n)))
             (enumerate-interval 1 n)))
  (define raw-data (make-triple))
  (filter (lambda (lst)
            (let ((i (list-ref lst 0))
                  (j (list-ref lst 1))
                  (k (list-ref lst 2)))
              (= (+ i j k) s)))
          raw-data))


(define (myloop)
  (let ((n (read))
        (s (read)))
    (if (eq? n eof)
        (void)
        (begin (display (tri-num-list n s)) (newline) (myloop)))))

(myloop)