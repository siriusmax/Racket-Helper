#lang racket


(define (outerloop i n numbers)
  (define (my-list-ref list position)
    (if (or (< position 0) (>= position (length list)))
        0
        (list-ref list position)))
  (define (innerloop j newnumbers)
    (if (= j i)
        (begin (display #\newline) newnumbers)
        (begin
          (let ((newitem (+ (my-list-ref numbers j) (my-list-ref numbers (- j 1)))))
            (if (= j 0)
                (void)
                (display " "))
            (display newitem)
            (innerloop (+ j 1) (append newnumbers (list newitem)))))))
  (define newnumbers (innerloop 0 empty))
  (if (= i n)
      (void)
      (outerloop (+ i 1) n newnumbers)))

(define (loop)
  (let ((n (read)))
    (if (eq? n eof)
        (void)
        (begin (outerloop 1 n (list 1)) (loop)))))
(loop)