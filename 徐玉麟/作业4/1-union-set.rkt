#lang racket

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))
(define (treeset2list tree)
  (if(null? tree)
     '()
     (append (treeset2list (left-branch tree)) (list (entry tree)) (treeset2list (right-branch tree)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))

(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq) (accumulate op init (cdr seq)))))

(define (union-set a b)
  (define seta (accumulate adjoin-set '() a))
  (accumulate adjoin-set seta b))

(define (loop)
  (define a (read))
  (define b (read))
  (if(eq? a eof)
     (void)
     (begin (displayln (treeset2list (union-set a b))) (loop))))

(loop)

