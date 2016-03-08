#lang racket
(define (qsort numbers)
  (if (<= (length numbers) 1)
      numbers
      (let*-values ([(key) (car numbers)]
                    ((small big) (partition (lambda (x) (< x key)) (cdr numbers))))
         (append (qsort small) (list key) (qsort big)))))

(define (myloop numbers)
  (let ((a (read)))
    (if (eq? a eof)
        (print (qsort (remove-duplicates numbers)) 0)
        (myloop (append numbers (list a))))))

(define (print numbers now)
  (if (= now (- (length numbers) 1))
      (begin (display (list-ref numbers now)))
      (begin (display (list-ref numbers now))
             (display " ")
             (print numbers (+ now 1)))))

(myloop empty)