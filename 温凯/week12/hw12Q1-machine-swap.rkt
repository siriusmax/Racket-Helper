#lang racket
(require r5rs)
;p18
(define (make-machine register-names ops controller-text)
  ;参数分别为寄存器列表、操作列表和指令序列
  ;首先检查是否有重复标签
  (if (not (check-controller controller-text)) 
      (begin (displayln "label error in machine") '()) 
      (let ((machine (make-new-machine)));machine开始是空机器
        (for-each (lambda (register-name)
                    ((machine 'allocate-register) register-name))
                  register-names)
        ((machine 'install-operations) ops)
        ((machine 'install-instruction-sequence)
         (assemble controller-text machine)) ;安装指令序列
        machine)))

(define (isexist? var lst)
  (if (null? lst)
      #f
      (if (eq? var (car lst))
          #t
          (isexist? var (cdr lst))
          )
      )
  )
; 检查是否有重复的label
(define (check-controller text)
  (define (check-and-add varlst lst)
    (if (null? varlst)
        #t; 一切正常
        (if (isexist? (car varlst) lst)
            #f  ;存在重复的标签
            (check-and-add (cdr varlst) (append lst (list (car varlst))))
            )
        )
    )
  (define (get-varlst text)
    (if (null? text)
        '()
        (if (symbol? (car text))
            (append (list (car text)) (get-varlst (cdr text)))
            (get-varlst (cdr text))
            )
        )
    )
  (let ((varlst (get-varlst text)))
    (check-and-add varlst '())
    )
  )
  

;p23
(define (instruction-execution-proc inst)
 (cdr inst))

(define (make-new-machine)
  (let ((pc (make-register 'pc)) ;名字 'pc 'flag没用
        ; (make-register的结果是个闭包,内部有状态contents,放着寄存器的值。
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '()));机器的指令序列
    (let ((the-ops ;操作列表
           (list (list 'initialize-stack
                       (lambda () (stack 'initialize)))))
          (register-table
           (list (list 'pc pc) (list 'flag flag))))
      (define (allocate-register name) ;添加一个寄存器表项。表项是(名字 寄存器）
        (if (assoc name register-table)
            (error "Multiply defined register: " name)
            (set! register-table
                  (cons (list name (make-register name))
                        register-table)))
        'register-allocated)
      (define (lookup-register name);根据名字查找寄存器
        (let ((val (assoc name register-table)))
          (if val
              (cadr val) ;返回结果是个闭包,代表寄存器。val形如 ('pc pc)
              (error "Unknown register:" name))))
      (define (execute)
        (let ((insts (get-contents pc)));insts是指令序列后缀
          (if (null? insts)
              'done
              (begin
                ((instruction-execution-proc (car insts)))
                ;(car insts)就是指令序列后缀中的第一条指令
                (execute)))))
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute)) ;pc指向执行指令序列的开头,从pc开始执行
              ((eq? message 'install-instruction-sequence)
               (lambda (seq) (set! the-instruction-sequence seq)))
              ;安装分析完成后的指令序列seq,在make-machine中用到
              ((eq? message 'allocate-register) allocate-register)
              ((eq? message 'get-register) lookup-register)
              ((eq? message 'install-operations) ;make-machine中用
               (lambda (ops) (set! the-ops (append the-ops ops))))
              ((eq? message 'stack) stack)
              ((eq? message 'operations) the-ops)
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))

      

;p26
(define (start machine)
 (machine 'start)) ;让机器开始运行
(define (get-register-contents machine register-name)
 (get-contents (get-register machine register-name)))
(define (set-register-contents! machine register-name value)
 (set-contents! (get-register machine register-name) value)
 'done)
(define (get-register machine reg-name)
;返回值是个register,就是个闭包,里面只有一个 content状态变量
 ((machine 'get-register) reg-name))

;p27
(define (make-register name) ;name没用
  (let ((contents '*unassigned*))
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
            ((eq? message 'set)
             (lambda (value) (set! contents value)))
            (else
             (error "Unknown request -- REGISTER" message))))
    dispatch))
(define (get-contents register)
  (register 'get))
(define (set-contents! register value)
  ((register 'set) value))

;p28
(define (make-stack)
  (let ((s '())) ;s就是栈
    (define (push x) (set! s (cons x s)))
    (define (pop)
      (if (null? s)
          (error "Empty stack -- POP")
          (let ((top (car s)))
            (set! s (cdr s))
            top)))
    (define (initialize)
      (set! s '()) 'done)
    (define (dispatch message)
      (cond ((eq? message 'push) push)
            ((eq? message 'pop) (pop))
            ((eq? message 'initialize) (initialize))
            (else (error "Unknown request -- STACK" message))))
    dispatch))

;p29

;分析原始的指令序列,并将分析后的指令序列添加到机器
;controller-text是原始形式的指令序列
(define (assemble controller-text machine)
  (extract-labels controller-text
                  (lambda (insts labels)
                    (update-insts! insts labels machine)
                    insts)))



;p30
(define (extract-labels text receive) ;只在assemble中被调用
  (if (null? text)
      (receive '() '());此处这个receive实际上就是assemble中的LBD
      (extract-labels (cdr text)
                      (lambda (insts labels)
                        (let ((next-inst (car text)))
                          (if (symbol? next-inst) ;为true说明是label,如 'gcd-done
                              (receive insts
                                       (cons (make-label-entry next-inst
                                                               insts)
                                             labels))
                              (receive (cons (make-instruction next-inst)
                                             insts)
                                       labels)))))))      


;p33

(define (update-insts! insts labels machine)
;为insts中的每条指令添加可执行过程, insts本来是文本形式的指令序列
  (let ((pc (get-register machine 'pc))
        (flag (get-register machine 'flag))
        (stack (machine 'stack))
        (ops (machine 'operations)));拿到上面这些东西的指针
    (for-each
     (lambda (inst)
       (set-instruction-execution-proc!
        inst ;为每条指令加上分析后得到的可执行过程
        (make-execution-procedure ;分析指令的结果是产生一个可执行过程
         (instruction-text inst) labels machine
         pc flag stack ops)))
     insts))) ;返回分析后的指令序列

;p34
(define (make-instruction text)
 ;text是指令的文字形式。本函数创建一个只有文字形式的指令
 (cons text '()))
(define (instruction-text inst)
 (car inst))

(define (set-instruction-execution-proc! inst proc)
 (set-cdr! inst proc))
;参数inst形如: ((test (op =) (reg b) (const 0)))
;执行之后指令变成了一个序对（非列表），形如：
; ((test (op =) (reg b) (const 0)) . <#procedureXXX>)

;p35
(define (make-label-entry label-name insts)
 (cons label-name insts))
;insts是机器里指令序列的一个后缀(从指令序列的开头或中间开始,直到结尾的子序列)
;返回值是一个label-entry,形如(L1 (assign...) (test ....) (branch ...)...)
;label-entry的cdr部分就是指令序列的一个后缀
(define (lookup-label labels label-name)
 ;labels里每个元素都是 label-entry
 (let ((val (assoc label-name labels)))
 (if val
 (cdr val)
 (error "Undefined label -- ASSEMBLE" label-name))))
;返回值就是一个指令序列后缀。表示从label-name开始往后的所有指令

; p36
;创建指令所对应的可执行过程。inst是一条指令,形如 (assign n (reg b))
(define (make-execution-procedure inst labels machine pc flag stack ops)
  (cond ((eq? (car inst) 'assign)
         (make-assign inst machine labels ops pc))
        ((eq? (car inst) 'test)
         (make-test inst machine labels ops flag pc))
        ((eq? (car inst) 'branch)
         (make-branch inst machine labels flag pc))
        ((eq? (car inst) 'goto)
         (make-goto inst machine labels pc))
        ((eq? (car inst) 'save)
         (make-save inst machine stack pc))
        ((eq? (car inst) 'restore)
         (make-restore inst machine stack pc))
        ((eq? (car inst) 'perform)
         (make-perform inst machine labels ops pc))
        ((eq? (car inst) 'swap);对swap的支持
         (make-swap inst machine pc))
        (else (error "Unknown instruction type -- ASSEMBLE"
                     inst))))
;====支持swap===feature 已完成
; inst形如 (swap a b)
(define (make-swap inst machine pc)
  ;首先取出来两个变量的值
  (let ((var-a (cadr inst))
        (var-b (caddr inst)))
    (let ((target-a (get-register machine var-a))
          (target-b (get-register machine var-b))); 这个时候得不到值
      (lambda ()
        (let ((val-a (get-register-contents machine var-a))
              (val-b (get-register-contents machine var-b)))  ;很可能会出错,现在还没有这种信息
          (set-contents! target-a val-b)
          (set-contents! target-b val-a)
        )
        (advance-pc pc)
        )
      )
    )
  )


; p37
(define (make-assign inst machine labels operations pc)
 ;inst 形如: (assign n (reg b)) n是寄存器名
  (let ((target ;target是寄存器，相当于 n
         (get-register machine (assign-reg-name inst))) ;取得n
        (value-exp (assign-value-exp inst))) ;value-exp 形如 ((reg b))
    (let ((value-proc ; (value-proc)是寄存器应被赋予的值
           (if (operation-exp? value-exp)
               ;value-exp形如((op rem))则是 operation-exp
               (make-operation-exp
                value-exp machine labels operations)
               (make-primitive-exp ;此时(car value-exp)形如 (reg b)
                (car value-exp) machine labels))))
      (lambda () ;assign指令对应的可执行过程
        (set-contents! target (value-proc))
        ;set-contents!设置寄存器target的值为 (value-proc)
        (advance-pc pc)))));程序计数器向前推进即pc.content=(cdr pc.content)
;p38
; assign-instruction 形如: (assign n (reg b)) n是寄存器名
(define (assign-reg-name assign-instruction)
 (cadr assign-instruction))
(define (assign-value-exp assign-instruction)
 (cddr assign-instruction))
(define (advance-pc pc)
 (set-contents! pc (cdr (get-contents pc))))
;pc里面有状态变量 content，指向指令序列里面某处 (cdr content)就指向再下一条指令。

;p39
(define (make-primitive-exp exp machine labels) ;返回对基本表达式求值的函数
  ;primitive exp形如: (reg b) 或 (const 3) 或 (label thing-done)
  ;返回值一定是个过程,执行该过程,得到exp的值。如果exp是个标号,则返回该标号代表的指
  ;令序列后缀
  (cond ((constant-exp? exp)
         (let ((c (constant-exp-value exp)))
           (lambda () c)))
        ((label-exp? exp)
         (let ((insts
                (lookup-label labels ;lookup-label的返回值是一个指令序列后缀
                              (label-exp-label exp))))
           (lambda () insts))) ;
        ((register-exp? exp)
         (let ((r (get-register machine
                                (register-exp-reg exp))))
           (lambda () (get-contents r)))) ;该lbd返回寄存器的值
        (else (error "Unknown expression type -- ASSEMBLE" exp))))

;p40
(define (register-exp? exp) (tagged-list? exp 'reg))
(define (register-exp-reg exp) (cadr exp)) ;取寄存器名
(define (constant-exp? exp) (tagged-list? exp 'const))
(define (constant-exp-value exp) (cadr exp)) ;取常数
(define (label-exp? exp) (tagged-list? exp 'label))
(define (label-exp-label exp) (cadr exp)) ;取标号名
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      #f)
  )

;p41
;operation exp形如: ((op rem) (reg a) (reg b))
(define (make-operation-exp exp machine labels operations)
  (let ((op (lookup-prim (operation-exp-op exp) operations))
        ;op是操作所对应的可执行过程
        (aprocs
         (map (lambda (e) ;e形如: (reg a)、 (const 3)、 (lable done)
                (make-primitive-exp e machine labels))
              (operation-exp-operands exp))))
    (lambda ()
      (apply op (map (lambda (p) (p)) aprocs)))))
;operations形如:
;(('+ +) ('< <) (initialize-stack (lambda () (stack 'initialize))))

;p42
;operation exp形如: ((op rem) (reg a) (reg b))
(define (operation-exp? exp)
 (and (pair? exp) (tagged-list? (car exp) 'op)))
(define (operation-exp-op operation-exp)
 (cadr (car operation-exp)))
(define (operation-exp-operands operation-exp)
 (cdr operation-exp))
(define (lookup-prim symbol operations);查找操作对应的可执行过程
  ;operations形如:
  ;(('+ +) ('< <) (initialize-stack (lambda () (stack 'initialize))))
  (let ((val (assoc symbol operations)))
    (if val
        (cadr val) ;返回操作所对应的可执行过程
        (error "Unknown operation -- ASSEMBLE" symbol))))


;p43
(define (make-test inst machine labels operations flag pc)
  (let ((condition (test-condition inst)))
    ;inst 形如 (test (op =) (reg n) (const 1))
    (if (operation-exp? condition)
        ;condition 形如 ((op =) (reg n) (const 1))
        (let ((condition-proc
               (make-operation-exp
                condition machine labels operations)))
          (lambda ()
            (set-contents! flag (condition-proc))
            (advance-pc pc)))
        (error "Bad TEST instruction -- ASSEMBLE" inst))))
(define (test-condition test-instruction)
  (cdr test-instruction))

;p44
(define (make-branch inst machine labels flag pc)
  ;branch 形如 (branch (label base-case))
  (let ((dest (branch-dest inst)))
    (if (label-exp? dest)
        (let ((insts ;insts是指令序列后缀
               (lookup-label labels (label-exp-label dest))))
          (lambda ()
            (if (get-contents flag)
                (set-contents! pc insts)
                (advance-pc pc))))
        (error "Bad BRANCH instruction -- ASSEMBLE" inst))))
(define (branch-dest branch-instruction)
  (cadr branch-instruction))

;p45
(define (make-goto inst machine labels pc)
  ;inst形如(goto (reg continue))或 (goto (label fact-loop))
  (let ((dest (goto-dest inst)))
    (cond ((label-exp? dest)
           (let ((insts
                  (lookup-label labels
                                (label-exp-label dest))))
             (lambda () (set-contents! pc insts))))
          ((register-exp? dest)
           (let ((reg
                  (get-register machine
                                (register-exp-reg dest))))
             (lambda ()
               (set-contents! pc (get-contents reg)))))
          (else (error "Bad GOTO instruction -- ASSEMBLE"
                       inst)))))
(define (goto-dest goto-instruction)
  (cadr goto-instruction))

;p46

(define (make-save inst machine stack pc) ;inst形如 (save n)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (push stack (get-contents reg))
      (advance-pc pc))))
(define (make-restore inst machine stack pc) ;inst形如 (restore n)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (set-contents! reg (pop stack))
      (advance-pc pc))))
(define (stack-inst-reg-name stack-instruction)
  (cadr stack-instruction)) ; stack-instruction形如 (restore n)
(define (pop stack)
 (stack 'pop))
(define (push stack value)
 ((stack 'push) value))


;p47
; inst形如:(perform (op <operation-name>) <input1> ... <inputn>)
(define (make-perform inst machine labels operations pc)
  (let ((action (perform-action inst)))
    (if (operation-exp? action)
        (let ((action-proc
               (make-operation-exp
                action machine labels operations)))
          (lambda ()
            (action-proc)
            (advance-pc pc)))
        (error "Bad PERFORM instruction -- ASSEMBLE" inst))))
(define (perform-action inst) (cdr inst))

;=====测试函数
#|
(define a '(make-machine
   '(a b t)
   (list (list 'rem remainder) (list '= =))
   '(test-b
       (test (op =) (reg b) (const 0))
       (branch (label gcd-done))
test-b
       (assign t (op rem) (reg a) (reg b))
       (assign a (reg b))
       (assign b (reg t))
       (goto (label test-b))
     gcd-done)))
(make-machine (cadr (cadr a)) (eval (caddr a) (scheme-report-environment 5)) (cadr (cadddr a)))

|#

;======= 自编函数部分

(define (make-machine? exp)
  (tagged-list? exp 'make-machine))
(define (set-regs? exp)
  (let ((x (car exp)))
    (pair? x)))
(define (get-reg? exp)
  (symbol? (car exp)))



;读入函数
(define (driver-loop machine)
  (let ((a (read)))
    (cond ((eq? a eof)
           (void))
          ((make-machine? a)
           (displayln "a new machine")
           (let ((mac (make-machine (cadr (cadr a)) 
                                    (eval (caddr a) (scheme-report-environment 5)) 
                                    (cadr (cadddr a)))))
             ;(displayln "succeed mac")
             (driver-loop mac)))
          ((set-regs? a)
           ;(display machine)
           (if (not (null? machine))  ;引导machine创建失败的情况
               (for-each (lambda (reg-val-pair) (set-register-contents! machine (car reg-val-pair) (cadr reg-val-pair)))
                         a))
               (driver-loop machine))
          ((get-reg? a)
           (if (not (null? machine))
               (begin
                 (start machine)
                 ;(displayln "ln get value")
                 (if (= (length a) 1)
                     (displayln (get-register-contents machine (car a)))
                     (let ((first (car a)) ;多余1个元素的情况
                           (rests (cdr a)))
                       (display (get-register-contents machine first))
                       (for-each (lambda (reg-name) (display " ") (display (get-register-contents machine reg-name))) rests)
                       (newline)
                       )
                     )))
           (driver-loop machine))          

          (else 
           ;(displayln "in else") 
           ;(displayln a) 
           (driver-loop machine))
          )
    )
  )
 
(driver-loop (void))
      
        
