#lang racket
(define N 8)
(define (for idx end fun)
    (if (= idx end)
        (void)
        (begin
            (fun idx)
            (for (+ 1 idx) end fun))))

(define (getQueenSeqs)
    (define (getQueenSeqAsInt queenSeq)
        (define ret 0)
        (begin
            (map (lambda (col) (set! ret (+ 1 col (* 10 ret)))) queenSeq)
            ret)
        )
    (define (getIsVisited seq)
        (lambda (row col)
            (define (slash-iter idx result)
                (if (= idx (length seq))
                    result
                    (slash-iter (+ 1 idx) 
                        (or result 
                            (= (+ idx (list-ref seq idx)) (+ row col)) 
                            (= (- idx (list-ref seq idx)) (- row col))))))
            (if (empty? seq)
                false
                (or false 
                    (< row (length seq)) 
                    (ormap (lambda (item) (= item col)) seq)
                    (slash-iter 0 false))
                )
        ))
    (define ret empty)
    (define (dfs deep queenSeq)
        (define isVisited (getIsVisited queenSeq))
        (if (= N deep)
            (set! ret (append ret (list (getQueenSeqAsInt queenSeq))))
            (for 0 N
                (lambda (i)
                    (if (false? (isVisited deep i)) 
                        (dfs (+ 1 deep) (append queenSeq (list i)))
                        (void))))
            )
        )
    (begin
        (dfs 0 empty)
        ret
        )
    )

(define queenSeqs (getQueenSeqs))
((lambda () (begin (read) (void))))
(define (work)
    (let ([idx (read)])
        (if (eq? idx eof)
            (void)
            (begin 
                (displayln (list-ref queenSeqs (- idx 1)))
                (work)))))
(work)