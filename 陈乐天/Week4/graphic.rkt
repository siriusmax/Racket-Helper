#lang racket/gui
(require racket/gui/base)

;定义向量
(define (make-vect x y) (cons x y))
(define (xcor-vect v)(car v))
(define (ycor-vect v)(cdr v))
(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1)
                (xcor-vect v2))
             (+ (ycor-vect v1) (ycor-vect v2))))
(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1)
                (xcor-vect v2)) (- (ycor-vect v1)
                                   (ycor-vect v2))))
(define (scale-vect s v)
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect v))))
;定义线段
(define (make-segment start end) (cons start end))
(define (start-segment seg) (car seg))
(define (end-segment seg) (cdr seg))
(define (scale-segments seg-list scale)
  (map
   (lambda (segment)
     (cons (scale-vect scale (start-segment segment)) (scale-vect scale (end-segment segment))))
   seg-list))
;start, end都是线段端点(向量),坐标相对于绝对原点

;定义frame
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2)) ;edge1和edge2也可以看做点,其坐标是相对于frame原点的
(define (origin-frame f)  (car f))
(define (edge1-frame f)  (cadr f))
(define (edge2-frame f)  (caddr f))
;向量转换器
(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))
;定义painter
(define (segments->painter segment-list) ;segment-list是线段列表
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line ;假定draw-line可以画线(以绝对原点作为原点)
        ((frame-coord-map frame) (start-segment segment))
        ((frame-coord-map frame) (end-segment segment))))
     segment-list))) ;本过程生成一个painter,其原图形是一系列线段
;accumulate
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
;flatmap
(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))
;将点组成的集合的集合转化为线段
(define (point->segments lst)
  (flatmap
   (lambda (point-list)
     (define (getSegment points)
       (if (null? (cdr points))
           '()
           (cons (make-segment (car points) (cadr points)) (getSegment (cdr points)))))
     (getSegment point-list))
   lst))
;转换painter
(define (transform-painter painter origin corner1 corner2)
   (lambda (frame)
     (let ((m (frame-coord-map frame)))
       (let ((new-origin (m origin)))
         (painter
          (make-frame new-origin
                      (sub-vect (m corner1) new-origin);frame的两条边是相对于frame原点的
                      (sub-vect (m corner2) new-origin)))))))
;一系列转换painter的方法
;恒等
(define (identity painter) painter)
;水平翻转
(define (flip-horz painter)
  (transform-painter painter
                     (make-vect 1 0)
                     (make-vect 0 0)
                     (make-vect 1 1)))
;竖直翻转
(define (flip-vert painter)
  (transform-painter painter
                     (make-vect 0 1)
                     (make-vect 1 1)
                     (make-vect 0 0)))
;旋转180度
(define (rotate180 painter)
  (transform-painter painter
                     (make-vect 1 1)
                     (make-vect 0 1)
                     (make-vect 1 0)))
;上下排列
(define (below painter1 painter2)
  (let ((up (transform-painter painter1
                               (make-vect 0 0)
                               (make-vect 1 0)
                               (make-vect 0 0.5)))
        (down (transform-painter painter2
                                 (make-vect 0 0.5)
                                 (make-vect 1 0.5)
                                 (make-vect 0 1))))
    (lambda (frame)
      (up frame)
      (down frame))))
;左右排列
(define (beside painter1 painter2)
  (let ((left (transform-painter painter1
                               (make-vect 0 0)
                               (make-vect 0.5 0)
                               (make-vect 0 1)))
        (right (transform-painter painter2
                                 (make-vect 0.5 0)
                                 (make-vect 1 0)
                                 (make-vect 0.5 1))))
    (lambda (frame)
      (left frame)
      (right frame))))
;在右侧递归排列
(define (right-split painter n) ;生成新painter,右分n次
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))
;在上侧递归排列
(define (up-split painter n) ;生成新painter,右分n次
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below (beside smaller smaller) painter))))
;在右上递归排列
(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
             (bottom-right (below right right))
             (top-right (corner-split painter (- n 1))))
          (beside (below top-left painter) (below top-right bottom-right))))))
;将四个方块结合
(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter))) (bottom (beside (bl painter) (br painter))))
      (below top bottom))))
;square-limit
(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-horz identity
                                  rotate180 flip-vert)))
    (combine4 (corner-split painter n))))

;drawLine
(define linesToDraw '())
(define (draw-line a b)
  (set! linesToDraw (cons (make-segment a b) linesToDraw)))

;处理小人的数据
(define figure
  (list
   (list (make-vect 0 26)
         (make-vect 6 17)
         (make-vect 12 25)
         (make-vect 14 21)
         (make-vect 10 0))
   (list (make-vect 16 0)
         (make-vect 21 13)
         (make-vect 25 0))
   (list (make-vect 31 0)
         (make-vect 25 19)
         (make-vect 41 6))
   (list (make-vect 41 15)
         (make-vect 31 27)
         (make-vect 25 27)
         (make-vect 27 35)
         (make-vect 25 41))
   (list (make-vect 16 41)
         (make-vect 14 35)
         (make-vect 16 27)
         (make-vect 12 27)
         (make-vect 6 25)
         (make-vect 0 35))))
(define figure-segments (scale-segments (point->segments figure) 0.024))

;开始画图
(define wave (flip-vert (segments->painter figure-segments)))
(define frame1 (make-frame (make-vect 0 0) (make-vect 400 0) (make-vect 0 400)))
((square-limit wave 4) frame1)
(define frame2 (make-frame (make-vect 500 0) (make-vect 300 50) (make-vect 150 300)))
((beside wave (flip-horz (rotate180 wave))) frame2)

;创建窗体及画布
(define frame (new frame% [label "萌萌的小人"] [width 1000] [height 500]))
(new canvas% [parent frame]
             [paint-callback
              (lambda (canvas dc)
                (send dc set-pen "red" 1 'solid)
                (for-each
                 (lambda (line)
                   (let ((p1 (start-segment line))
                         (p2 (end-segment line)))
                     (send dc draw-line
                           (xcor-vect p1) (ycor-vect p1)
                           (xcor-vect p2) (ycor-vect p2))))
                 linesToDraw))])
(send frame show #t)