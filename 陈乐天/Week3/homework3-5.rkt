#lang racket

(define queens empty)
(define (save result)
  (set! queens (append queens (list result))))

(define (doSearch depth now)
  (define (findThisRow column)
    (define (isColumnOK? column)
      (define (ok? index)
        (if (= index (length now))
            #t
            (let ((value (list-ref now index)))
              (if (or (= value column)
                      (= (- value column) (- (+ 1 index) depth))
                      (= (- value column) (- depth (+ 1 index))))
                  #f
                  (ok? (+ index 1))))))
      (ok? 0))
    (if (= column 9)
        (void)
        (begin (if (isColumnOK? column)
                   (doSearch (+ depth 1) (append now (list column)))
                   (void))
               (findThisRow (+ column 1)))))
  (if (= depth 9)
      (save now)
      (findThisRow 1)))

(doSearch 1 empty)

(define (printFunc lst)
  (define (loop now result)
    (if (= now 8)
        result
        (loop (+ now 1) (+ (* result 10) (list-ref lst now)))))
  (loop 0 0))

(define (myloop)
  (let ((a (read)))
    (if (eq? a eof)
        (void)
        (begin (displayln (printFunc (list-ref queens (- a 1)))) (myloop)))))
(define n (read))
(myloop)