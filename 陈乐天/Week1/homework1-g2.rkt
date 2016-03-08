#lang racket

(define (dowork n m i)
  (define (print j)
    (display j)
    (display ",")
    (if (= j m)
        (display #\newline)
        (print (+ j 1))))
  (print 1)
  (if (= i n)
      (void)
      (dowork n m (+ i 1))))


(define (loop)
  (let ((n (read))
        (m (read)))
    (if (eq? n eof)
        (void)
        (begin (dowork n m 1) (loop)))))

(loop)