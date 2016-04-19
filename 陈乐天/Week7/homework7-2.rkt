#lang racket
(require scheme/mpair)
(define list mlist)
(define cdr mcdr)
(define car mcar)
(define set-car! set-mcar!)
(define set-cdr! set-mcdr!)
(define cons mcons)

(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (empty-queue? queue) (null? (front-ptr queue)))
(define (make-queue) (cons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue)))) 

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else
         (set-front-ptr! queue (cdr (front-ptr queue)))
         queue)))


(define n (read))
(define k (read))

(define my-queue (make-queue))
(define has-went (make-vector 100001 0))
(vector-set! has-went n 1)

(define (find-cattle)
  (let ((front (car (front-queue my-queue)))
        (front-pace (cdr (front-queue my-queue))))
    (if (= front k)
        (display front-pace)
        (void))
    (define one (+ front 1))
    (if (and (<= one 100000) (= (vector-ref has-went one) 0))
        (begin
          (vector-set! has-went one 1)
          (insert-queue! my-queue (cons one (+ front-pace 1))))
        (void))
    (define two (- front 1))
    (if (and (>= two 0) (= (vector-ref has-went two) 0))
        (begin
          (vector-set! has-went two 1)
          (insert-queue! my-queue (cons two (+ front-pace 1))))
        (void))
    (define three (* front 2))
    (if (and (<= three 100000) (= (vector-ref has-went three) 0))
        (begin
          (vector-set! has-went three 1)
          (insert-queue! my-queue (cons three (+ front-pace 1))))
        (void))
    (delete-queue! my-queue)
    (if (empty-queue? my-queue)
        (void)
        (find-cattle))))


(define miaomiaomiao (insert-queue! my-queue (cons n 0)))
(find-cattle)