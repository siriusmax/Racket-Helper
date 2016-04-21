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



(define (inSet? set val)
  (if (null? set)
      #f
      (if (equal? (car set) val)
          #t
          (inSet? (cdr set) val)
          )
      )
  )



(define (readAllData)
  (let ((x (read)))
    (if (eq? x eof)
        '()
        (mappend (list x) (readAllData))
        )
    )
  )

(define (getUntilNum readData)
  (if (null? readData)
      '()
      (if (symbol? (car readData))
          (mappend (list (car readData)) (getUntilNum (cdr readData)))
          '()
          )
      )
  )


(define moveRight 1)
(define moveDown 2)
(define moveLeft 3)
(define moveUp 4)

(define (make-solvelst r c k Nstep)
  (list r c k Nstep)
  )
(define (get-step solvelst)
  (car (cdr (cdr (cdr solvelst))))
  )
(define (get-money solvelst)
  (car (cdr (cdr solvelst)))
  )


(define (make-maze r c lst)
  (mappend (list r) (list c) lst))

; maze : (r c lst)
(define (maze-ref r c maze)
  ;(displayln maze)
  (let ((maxr (car maze))
        (maxc (car (cdr maze)))
        (lst (cdr (cdr maze)))
        )
    (if (or (> r maxr)
            (> c maxc)
            (<= r 0)
            (<= c 0)
            )
        #f
        (mlist-ref lst (- (+ (* (- r 1) maxc) c) 1))  ; （r - 1 ）* maxc + c - 1
        )
    )
  )
(define (get-maze-maxc maze)
  (car (cdr maze)))
(define (get-maze-list-index r c maze)
    (let ((maxr (car maze))
        (maxc (car (cdr maze)))
        (lst (cdr (cdr maze)))
        )
    (if (or (> r maxr)
            (> c maxc)
            (<= r 0)
            (<= c 0)
            )
        #f
        (- (+ (* (- r 1) maxc) c) 1)  ; (r - 1 )* maxc + c - 1
        )
    )
  )
  

;(define solveque (make-queue))

(define (isSolve solvelst maze)
  ;(displayln solvelst)
  (and (= (car solvelst) (car maze))
       (= (car (cdr solvelst)) (car (cdr maze)))
       (>= (car (cdr (cdr solvelst))) 0)
       )
  )



(define (findSolve k maze)
  (define solveque (make-queue))
  
  (insert-queue! solveque (make-solvelst 1 1 k 0))
  (define footprint (list (list 0 0 0)))  
  (define (mark-solve solvelst)
    (set! footprint (mappend footprint (list (list (car solvelst) (car (cdr solvelst)) (car (cdr (cdr solvelst)))))))
    )
  (define (isMarked solvelst)
        (inSet? footprint (list (car solvelst) (car (cdr solvelst)) (car (cdr (cdr solvelst)))))
    )
  (define (addUnmarkedSolveList thissolve)
     (let ((maxr (car maze))
           (maxc (car (cdr maze)))
           (r (car thissolve))
           (c (car (cdr thissolve)))
           (k (car (cdr (cdr thissolve))))
           (Nstep (car (cdr (cdr (cdr thissolve)))))  
           )
       (let ((nextc (- c 1)))
         (if (or (<= nextc 0) (> nextc maxc))
             (void)
             (let ((nextMazeLetter (maze-ref r nextc maze)))  
               (cond ((eq? nextMazeLetter 'B) 
                      (insert-queue! solveque (make-solvelst r nextc k (+ Nstep 1)))
                      )
                     ((eq? nextMazeLetter 'W) 
                      (void)
                      )
                     ((eq? nextMazeLetter 'M) 
                      (insert-queue! solveque (make-solvelst r nextc (- k 1) (+ Nstep 1)))
                      )
                     (else (error 'Unknown_maze_letter))
                     )
               )
             )
         )
       (let ((nextc (+ c 1)))
         (if (or (<= nextc 0) (> nextc maxc))
             (void)
             (let ((nextMazeLetter (maze-ref r nextc maze)))  
               (cond ((eq? nextMazeLetter 'B) 
                      (insert-queue! solveque (make-solvelst r nextc k (+ Nstep 1)))
                      )
                     ((eq? nextMazeLetter 'W) 
                      (void)
                      )
                     ((eq? nextMazeLetter 'M) 
                      (insert-queue! solveque (make-solvelst r nextc (- k 1) (+ Nstep 1)))
                      )
                     (else (error 'Unknown_maze_letter))
                     )
               )
             )
         )
       (let ((nextr (- r 1))) 
         (if (or (<= nextr 0) (> nextr maxr))
             (void)
             (let ((nextMazeLetter (maze-ref nextr c maze))) 
               (cond ((eq? nextMazeLetter 'B) 
                      (insert-queue! solveque (make-solvelst nextr c k (+ Nstep 1)))
                      )
                     ((eq? nextMazeLetter 'W) 
                      (void)
                      )
                     ((eq? nextMazeLetter 'M) 
                      (insert-queue! solveque (make-solvelst nextr c (- k 1) (+ Nstep 1)))
                      )
                     (else (error 'Unknown_maze_letter))
                     )
               )
             )
         )
       (let ((nextr (+ r 1)))
         (if (or (<= nextr 0) (> nextr maxr))
             (void)
             (let ((nextMazeLetter (maze-ref nextr c maze))) 
               (cond ((eq? nextMazeLetter 'B) 
                      (insert-queue! solveque (make-solvelst nextr c k (+ Nstep 1)))
                      )
                     ((eq? nextMazeLetter 'W) 
                      (void)
                      )
                     ((eq? nextMazeLetter 'M) 
                      (insert-queue! solveque (make-solvelst nextr c (- k 1) (+ Nstep 1)))
                      )
                     (else (error 'Unknown_maze_letter))
                     )
               )
             )
         )
       )
    )

  (define (fs-iter)

    (if (empty-queue? solveque)
        (displayln "inf") 
        (let ((thissolve (front-queue solveque)))
          (if (isSolve thissolve maze)
              (displayln (get-step thissolve))
              (begin 
                (delete-queue! solveque)
                (if (isMarked thissolve)
                    (fs-iter)
                    (begin 
                      (mark-solve thissolve)
                      (if (>= (get-money thissolve) 0)
                          (addUnmarkedSolveList thissolve)
                          (void)
                          )
                      (fs-iter)
                      )
                    )
                )
              )
          )
        )
    )
;  (displayln maze)
  (fs-iter)
                    
     

   )


#|
  (let ((r (car solvelst))
        (c (car (cdr solvelst)))
        (k (car (cdr (cdr solvelst))))
        (lst (cdr (cdr (cdr solvelst))))
        )
    (
|#


(define (analyzeReadData readData)
  (if (null? readData)
      (void)
      (if (number? (car readData))   
          (let ((x (car readData))
                (y (car (cdr readData)))
                (z (car (cdr (cdr readData))))
                )
            (let ((symlst (getUntilNum (cdr (cdr (cdr readData))))))
              (let ((maze (make-maze x y symlst)))
                (findSolve z maze)
                )
              (analyzeReadData (mlist-tail readData (+ (mlength symlst) 3)))
              )
            )
          (error 'UNKOWN_IDENTIFIER)
          )
      )
  )
          
(define mm (read))
(define input (readAllData))
(analyzeReadData input)


#|测试数据
1
3 10 1
B W B B B W W B B B
B B B W M B B B M B
B B B B B B B B B B



1
2 5 1
B W B B B
M B B M B

|#

#|
问题：在去重时，状态不同的应该区分对待，只剩一块钱和只剩两块钱的去重应该是两张表
问题2：
|#