#lang racket

(define (join li li2)
  (if(null? li)
     li2
     (cons (car li) (join (cdr li) li2))))

(define (flatten li)
  (if(null? li)
     li
     (if(pair? li)
        (if(null? (cdr li))
           (flatten (car li))
           (join (flatten (car li)) (flatten (cdr li))))
        (list li))))

(define (main li)
  (display (flatten li)))

(define (loop)
  (define li (read))
  (if(eq? li eof)
     (void)
     (begin (main li) (newline) (loop))))

(loop)