#lang racket

(define (join li item)
  (if(null? li)
     (list item)
     (cons (car li) (join (cdr li) item))))

(define (revertlist li)
  (if(null? li)
     li
     (join (revertlist (cdr li)) (car li))))

(define (main li)
  (display (revertlist li)))

(define (loop)
  (define li (read))
  (if(eq? li eof)
     (void)
     (begin (main li) (newline) (loop))))

(loop)