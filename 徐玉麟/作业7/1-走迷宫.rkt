#lang racket
(require scheme/mpair)

(define row 0)
(define col 0)
(define money 0)
(define themap (make-vector 0 0))
(define result 9999999999)
(define direction (list (cons 1 0) (cons 0 1) (cons -1 0) (cons 0 -1)))

(define (get-map-point xy)
  (let ((x (car xy))
        (y (cdr xy)))
    (vector-ref (vector-ref themap x) y)))
(define (get-land point)
  (mcar point))
(define (get-hasgo point)
  (mcdr point))

(define (xy-add xy dir)
  (cons (+ (car xy) (car dir)) (+ (cdr xy) (cdr dir))))

(define (outofrange xy)
  (let ((x (car xy))
        (y (cdr xy)))
    (or(< x 0)
       (> x col)
       (< y 0)
       (> y row))))

(define (update-result new-result)
  (if(< new-result result)
     (set! result new-result)
     (void)))

(define (cango xy)
  (let ((land (get-land xy)))
    (cond((eq? land 'B') #t)
         ((eq? land 'W') #f)
         ((> money 0) #t)
         (else #f))))

(define (set-hasgo xy)
  (set-mcdr! (get-map-point xy) #t))
(define (reset-hasgo xy)
  (set-mcdr! (get-map-point xy) #f))
(define (hasgo xy)
  (get-hasgo (get-map-point xy)))

(