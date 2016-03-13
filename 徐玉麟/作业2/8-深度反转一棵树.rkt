#lang racket

(define (joinli li li2)
  (if(null? li)
     li2
     (cons (car li) (joinli (cdr li) li2))))

(define (join li item)
  (if(null? li)
     (list item)
     (cons (car li) (join (cdr li) item))))

(define (reverttree tree)
  (if(null? tree)
     tree
     (if(pair? (car tree))
        (joinli (reverttree (cdr tree))  (list(reverttree (car tree))))
        (join (reverttree (cdr tree)) (car tree)))))

(define (main li)
  (display (reverttree li)))

(define (loop)
  (define li (read))
  (if(eq? li eof)
     (void)
     (begin (main li) (newline) (loop))))

(loop)