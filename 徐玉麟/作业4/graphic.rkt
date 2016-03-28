#lang racket/gui
;定义整个Windows窗体
(define global-frame (new frame%
                   [label "Wave"]
                   [width 800]
                   [height 500]))
;canvas采用回调的方式来画图，所以把所有要画的线段注册在这里
(define segments2paint '())
(define (global-draw-line v1 v2)
  (set! segments2paint (cons (make-segment v1 v2) segments2paint)))
;定义用于画图的canvas
(define global-canvas (new canvas%
     [parent global-frame]
     [paint-callback
      (lambda (canvas dc)
        (send dc set-pen "red" 1 'solid)
        (for-each
         (lambda (seg)
           (let((v1 (segment-start seg))
                (v2 (segment-end seg)))
             (send dc draw-line (vect-x v1) (vect-y v1) (vect-x v2) (vect-y v2))))
         segments2paint))]))
;向量的定义
(define (make-vect x y) (cons x y))
(define (vect-x v) (car v))
(define (vect-y v) (cdr v))
(define (vect-add v1 v2)
  (cons (+ (car v1) (car v2)) (+ (cdr v1) (cdr v2))))
(define (vect-sub v1 v2)
  (cons (- (car v1) (car v2)) (- (cdr v1) (cdr v2))))
(define (vect-scale s v)
  (make-vect (* s (vect-x v))
             (* s (vect-y v))))
;线段的定义
(define (make-segment v1 v2) (cons v1 v2))
(define (segment-start seg) (car seg))
(define (segment-end seg) (cdr seg))
;frame的定义
(define (make-frame v-origin v1 v2)
  (list v-origin v1 v2))
(define (get-frame-origin frame)
  (car frame))
(define (get-frame-v1 frame)
  (cadr frame))
(define (get-frame-v2 frame)
  (caddr frame))
(define (frame-coord-map frame)
  (lambda (v)
    (vect-add (get-frame-origin frame)
              (vect-add (vect-scale (vect-x v) (get-frame-v1 frame))
                        (vect-scale (vect-y v) (get-frame-v2 frame))))))
;painter的定义
(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (global-draw-line
        ((frame-coord-map frame) (segment-start segment))
        ((frame-coord-map frame) (segment-end segment))))
     segment-list)))
(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter
         (make-frame new-origin
                     (vect-sub (m corner1) new-origin)
                     (vect-sub (m corner2) new-origin)))))))
;定义一些对painter的操作
(define (right-split painter n) ; 生成新painter ,右分n次
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))
(define (up-split painter n) ; 生成新painter ,上分n次
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))
(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))
(define (flip-vert painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))
(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))
(define (identity painter) painter)
(define (shrink-to-upper-right painter)
  (transform-painter painter
                     (make-vect 0.5 0.5)
                     (make-vect 1.0 0.5)
                     (make-vect 0.5 1.0)))
(define (rotate180 painter)
  (transform-painter painter
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 0.0)))
(define (squash-inwards painter);将图形向中心收缩
  (transform-painter painter
                     (make-vect 0.0 0.0)
                     (make-vect 0.65 0.35)
                     (make-vect 0.35 0.65)))
(define (beside painter1 painter2) ;在frame左边画painter1,右边画painter2
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
           (transform-painter painter1
                              (make-vect 0.0 0.0)
                              split-point
                              (make-vect 0.0 1.0)))
          (paint-right
           (transform-painter painter2
                              split-point
                              (make-vect 1.0 0.0)
                              (make-vect 0.5 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))
(define (below painter1 painter2) ;在frame上面画painter1,下面画painter2
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-up
           (transform-painter painter1
                              (make-vect 0.0 0.0)
                              (make-vect 1.0 0.0)
                              split-point))
          (paint-down
           (transform-painter painter2
                              split-point
                              (make-vect 1.0 0.5)
                              (make-vect 0.0 1.0))))
      (lambda (frame)
        (paint-up frame)
        (paint-down frame)))))
(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))
(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-horiz identity
                                  rotate180 flip-vert)))
    (combine4 (corner-split painter n))))
;用点列表的列表生成线段列表
(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq) (accumulate op init (cdr seq)))))
(define (vectslist->segments vectslist)
  (accumulate append '()
              (map vects->segments vectslist)))
;用点列表生成线段列表
(define (vects->segments vects)
  (define (vects->segments0 vects segments)
    (if(or (null? vects) (null? (cdr vects)))
       segments
       (let((v1 (car vects))
            (v2 (cadr vects)))
         (vects->segments0 (cdr vects)
                           (cons (make-segment v1 v2) segments)))))
  (vects->segments0 vects '()))
;等比放大点列表的列表
(define (scale-vectslist s vectslist)
  (map (lambda (vects)
              (map (lambda (vect) (vect-scale s vect)) vects))
            vectslist))
;预处理wave的segment
(define wave-vectslist
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
(define wave-segments
  (vectslist->segments
   (scale-vectslist 0.024 wave-vectslist)))


;开始画图
(define wave (segments->painter wave-segments))
(define wave2 (beside (flip-vert wave) wave))
(define sqwave (square-limit wave 5))
(define sqframe
  (make-frame (make-vect 10 10) (make-vect 300 0) (make-vect 0 300)))
(define frame2
  (make-frame (make-vect 320 20) (make-vect 300 50) (make-vect 150 300)))
(wave2 frame2)
(sqwave sqframe)
;显示窗体
(send global-frame show #t)