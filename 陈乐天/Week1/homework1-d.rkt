#lang racket



(define (loop answer now)
  (define new (- (+ (list-ref answer (- now 1)) (* 4 (list-ref answer (- now 2)))
                    (* (list-ref answer (- now 3)) 5)
                    (* (list-ref answer (- now 5)) (list-ref answer (- now 5)) (list-ref answer (- now 5))))
                 (* 2 (list-ref answer (- now 4)) (list-ref answer (- now 4)))))
  (if (= now 51)
      (dowork answer)
      (loop (append answer (list new)) (+ now 1))))

(define (dowork answer)
  (let ((n (read)))
    (if (eq? n eof)
        (void)
        (begin (display (list-ref answer n)) (display #\newline) (dowork answer)))))
  


(loop (list 1 1 1 1 1) 5)