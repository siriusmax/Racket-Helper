#lang racket

(require scheme/mpair)
(define car mcar)
(define cdr mcdr)
(define list mlist)
(define set-car! set-mcar!)
(define set-cdr! set-mcdr!)
(define cons mcons)


(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))
(define (empty-queue? queue) (null? (front-ptr queue)))
(define (make-queue) (cons '() '()))
(define (front-queue queue) (if (empty-queue? queue) (error "FRONT called with an empty queue" queue) (car (front-ptr queue))))
(define (insert-queue! queue item)(let ((new-pair (cons item '()))) (cond ((empty-queue? queue) (set-front-ptr! queue new-pair) (set-rear-ptr! queue new-pair) queue) (else (set-cdr! (rear-ptr queue) new-pair)  (set-rear-ptr! queue new-pair) queue))))
(define (delete-queue! queue) (cond ((empty-queue? queue) (error "DELETE! called with an empty queue" queue)) (else (set-front-ptr! queue (cdr (front-ptr queue))) queue)))



(define (getPos solve)
  (car solve))
(define (getNStep solve)
  (car (cdr solve)))

(define minus1 1)
(define plus1 2)
(define double 3)
(define choices (list 1 2 3))


(define solveque (make-queue))
(define visitedPosVector (make-vector 200001 0))



(define (dosolve n k)
  (define (ds-iter)

    (let ((thissolve (front-queue solveque)))
      (if (= (getPos thissolve) k)
          (getNStep thissolve)
          (begin
            (delete-queue! solveque)
            (let ((x (getPos thissolve)))
              (if (or (< x 0) (> x 200000))
                  (ds-iter)
                    (if (= (vector-ref visitedPosVector x) 0)
                        (begin
                          (vector-set! visitedPosVector x 1)
                          (insert-queue! solveque (list (+ (getPos thissolve) 1) (+ (getNStep thissolve) 1)))
                          (insert-queue! solveque (list (- (getPos thissolve) 1) (+ (getNStep thissolve) 1)))
                          (insert-queue! solveque (list (* (getPos thissolve) 2) (+ (getNStep thissolve) 1)))
                          (ds-iter)
                          )
                        (ds-iter)
                    )
                  )
              )
            )
          )
      )
    )
  (insert-queue! solveque (list n 0))
  (ds-iter)
  )


  

;(dosolve 5 17)
(define (for-each-readTwo proc)
  (let ((x (read))
        (y (read)))
    (if (eq? x eof)
        (void)
        (begin 
          (displayln (proc x y))
          (for-each-readTwo proc))
        )
    )
  )
(for-each-readTwo dosolve)



;更新：一直没考虑农夫和牛可以位于位置0