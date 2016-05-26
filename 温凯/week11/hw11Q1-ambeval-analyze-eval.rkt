#lang racket
(require (except-in r5rs eval))

;====amb解释器有关函数

(define (amb? exp) (tagged-list? exp 'amb))
(define (amb-choices exp) (cdr exp))


;(define (eval exp env) ((analyze exp) env))

(define (ambeval exp env succeed fail) ((analyze exp) env succeed fail))
(define (analyze exp) ;返回值是一个可执行函数(不是源代码形式的也不是函数对象),以环境为参数。
 (cond ((self-evaluating? exp)  (analyze-self-evaluating exp))
       ((quoted? exp) (analyze-quoted exp))
       ((variable? exp) (analyze-variable exp))
       ((assignment? exp) (analyze-assignment exp))
       ((definition? exp) (analyze-definition exp))
       ((if? exp) (analyze-if exp))
       ((lambda? exp) (analyze-lambda exp))
       ((begin? exp) (analyze-sequence (begin-actions exp)))
       ((cond? exp) (analyze (cond->if exp)))
       ((or? exp) (analyze-or exp))
       ((and? exp) (analyze-and exp))
       ((let? exp) (analyze-let exp))
       ((amb? exp) (analyze-amb exp))
       ((application? exp) (analyze-application exp))
       (else (error "Unknown expression type -- ANALYZE" exp))))
;exp是函数调用表达式的前提下:
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
;下面ops是操作数的列表
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define (make-frame variables values)
 (cons variables values));框架形如 ((x y z) 1 2 3)
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
 (set-car! frame (cons var (car frame)))
 (set-cdr! frame (cons val (cdr frame))))
(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())
(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (last-exp? seq) (null? (cdr seq)));判断seq里是否只有一个表达式
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))
(define (sequence->exp seq);把表达式列表变成一个表达式
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))
(define (application? exp) (pair? exp))
(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp)) ;返回所有分支的列表
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond-else-clause? clause) (eq? (cond-predicate clause) 'else))

(define (cond->if exp)
 (expand-clauses (cond-clauses exp)))
;clauses是一个列表,每个元素是一个分支,元素形如: ((> x 3) (+ x 3) (* x 3))
(define (expand-clauses clauses)
  (if (null? clauses)
      'false ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))
(define (make-if predicate consequent alternative)
  ;(display consequent)
  (if (null? consequent)
      (if (eq? alternative 'false) ; 如果没有alternative
          predicate
          (list 'begin predicate alternative))
      (list 'if predicate consequent alternative)
      )
  )
(define (self-evaluating? exp)
 (cond ((number? exp) true) ;number?是scheme基本过程
 ((string? exp) true) ;string?是scheme基本过程
 (else false)))
(define (variable? exp) (symbol? exp)) ;symbol?是scheme基本过程
(define (quoted? exp)
 (tagged-list? exp 'quote))
;单引号开头的表达式会被scheme自动转换成 (quote ...)列表形式
;(define (text-of-quotation exp) (cadr exp))
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (assignment? exp)
 (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

(define (definition? exp)
 (tagged-list? exp 'define));exp形如(define ....)
(define (definition-variable exp)
 (if (symbol? (cadr exp))
 (cadr exp) ;针对第一种形式
 (caadr exp)));针对第二种形式,此时变量名就是函数名
(define (definition-value exp)
 (if (symbol? (cadr exp))
 (caddr exp);针对第一种形式
 (make-lambda (cdadr exp) ; formal parameters
 (cddr exp)))) ; body

(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp)) ;body可能是个表达式序列

(define (make-lambda parameters body) ;构造一个lambda表达式
 (cons 'lambda (cons parameters body)))

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))
(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
;下面seq是一个列表,每个元素都是exp
;(define (last-exp? seq) (null? (cdr seq)));判断seq里是否只有一个表达式
;(define (first-exp seq) (car seq))
;(define (rest-exps seq) (cdr seq))
;(define (sequence->exp seq);把表达式列表变成一个表达式
;  (cond ((null? seq) seq)
;        ((last-exp? seq) (first-exp seq))
;        (else (make-begin seq))))
;(define (make-begin seq) (cons 'begin seq))
(define (analyze-self-evaluating exp)   (lambda (env succeed fail)  (succeed exp fail)))
(define (analyze-quoted exp) 
  (let ((qval (text-of-quotation exp)))
 (lambda (env succeed fail)
 (succeed qval fail))))
  ;单引号开头的表达式会被scheme自动转换成 (quote ...)列表形式
(define (text-of-quotation exp) (cadr exp))
(define (analyze-variable exp)
  (lambda (env succeed fail)
 (succeed (lookup-variable-value exp env)
 fail)))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)));到外围环境继续找
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))
(define (lookup-vairable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars) (env-loop (enclosing-environment env)))
            ((eq? var (car vars)) (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variables" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))
        )
    )
  (env-loop env)
  )


(define (analyze-assignment exp)  
 ; (error "In assignment!")
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env succeed fail)  ;这里不是生成分派函数，而是调用分派函数
      (vproc env
             (lambda (val fail2)
               (let ((old-value (lookup-variable-value var env)))
                 (set-variable-value! var val env)
                 (succeed (void)
                          (lambda ()
                            (set-variable-value! var old-value env)
                            (fail2)
                            ))
                 )
               )
             fail
             )
      )
    )
  )

             
;(define (assignment-variable exp) (cadr exp))
;(define (assignment-value exp) (cddr exp))
(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars) (env-loop (enclosing-environment env)))
            ((eq? var (car vars)) (set-car! vals val))
            (else (scan (cdr vars) (cdr vals))))
      )
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame))
          )
        )
    )
  (env-loop env)
  )

(define (analyz-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env) ;vproc是一个可执行函数
      (define-variable! var (vproc env) env)
      'ok)))
(define (define-variable! var val env);仅被eval-definition 调用
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars);如果变量不存在,就加到env的第一个 frame里面 
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))
(define (true? val)
  ;(displayln "in true============")
  (or (eq? val 'true) val)
  )
(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp))) ;pproc是分派函数
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env succeed fail) ;分派函数 L7
      (pproc env ;pproc 以 (if-predicate exp)的值为参数去调用S3
             (lambda (pred-value fail2) ;成功函数,S3
               (if (true? pred-value)
                   (cproc env succeed fail2)
                   (aproc env succeed fail2)))
             fail))))
(define (analyze-lambda exp) ;exp是lambda表达式
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env succeed fail)
      (succeed (make-procedure vars bproc env)
               fail))))
(define (make-procedure parameters body env)
 (list 'procedure parameters body env))
(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env succeed fail)
      (fproc env
             (lambda (proc fail2) ;成功函数 L
               (get-args aprocs ;proc是fproc在env中的值,是个函数对象
                         env
                         (lambda (args fail3) ;成功函数 S
                           (execute-application
                            proc args succeed fail3))
                         fail2))
             fail))))
(define (get-args aprocs env succeed fail)
  (if (null? aprocs)
      (succeed '() fail)
      ((car aprocs) env
                    (lambda (arg fail2) ;成功函数K1
                      (get-args (cdr aprocs)
                                env
                                (lambda (args fail3) ;成功函数 K2
                                  (succeed (cons arg args)
                                           fail3))
                                fail2))
                    fail)))

(define (primitive-procedure? proc)
 (tagged-list? proc 'primitive))
(define (primitive-implementation proc) (cadr proc))
(define (compound-procedure? p)
 (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))


(define (execute-application proc args succeed fail)
  ;proc是函数对象,args是实际参数列表
  (cond ((primitive-procedure? proc)
         (succeed (apply-primitive-procedure proc args)
                  fail))
        ((compound-procedure? proc)
         ((procedure-body proc) ;是个分派函数
          (extend-environment (procedure-parameters proc)
                              args
                              (procedure-environment proc))
          succeed
          fail))
        (else
         (error
          "Unknown procedure type -- EXECUTE-APPLICATION"
          proc))))
(define (apply-primitive-procedure proc args)
  (apply (primitive-implementation proc) args)
  )
#|
(define (my-apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply (primitive-implementation procedure) arguments))
        ;(primitive-implementation proc) 返回形如: #<procedure:car> ,        #<procedure:my-square>之类的东西(如果my-square被定义成primitive的话)
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           arguments
           (procedure-enviroment procedure))))
        (else
         (error "unkonwn procedure type -- APPLY" procedure))))
|#

(define (analyze-sequence exps)
  ;exps是表达式列表,形如 ((* x y) (+ x 4)....)
  (define (sequentially a b)
    (lambda (env succeed fail) ;分派函数L6
      (a env
         (lambda (a-value fail2) ;成功函数S2
           (b env succeed fail2))
         fail)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence -- ANALYZE"))
    (loop (car procs) (cdr procs))))
(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)
               (define-variable! var val env) ;做好定义,然后再成功继续
               (succeed (void) fail2)) ;define表达式的返回值是 (void)
             fail))))


(define (analyze-and exp)
  (let ((pars (operands exp)))
    (lambda (env) (eval-and pars env))
     )
  )
(define (and? exp) (tagged-list? exp 'and))
(define (eval-and parameters env)   ;短路的and还是非短路的and? 能否保证paremeters一定非空?  先假设 and至少有一个parameter  以及,and的返回值究竟带不带',为什么  不带'就正确 带'就错误。在内部的表示是什么?不带'的?
  (define (and-iter2 newPars)
    (let ((thisval (eval (car newPars) env)))
      (if (null? (cdr newPars))
          thisval
          (if thisval
              (and-iter2 (cdr newPars))
              false
              )
          )
      )
    )
  (and-iter2 parameters)
  )
(define (or? exp) (tagged-list? exp 'or))
(define (analyze-or exp)
  (let ((pars (operands exp)))
    (lambda (env) (eval-or pars env))
     )
  )
(define (eval-or parameters env)   ;这里是短路或
  (define (or-iter pars)
    (if (null? pars)
        false
        (if (eval (car pars) env)
            true
            (or-iter (cdr pars))
            )
        )
    )
  (or-iter parameters)
  )
(define (analyze-amb exp) ;require中的(amb)也会经由这里来执行,其结果就是调用 fail
  (let ((cprocs (map analyze (amb-choices exp))))
    (lambda (env succeed fail) ;分派函数
      (define (try-next choices)
        (if (null? choices)
            (fail);回溯。选择用完了也会走这里
            ((car choices) env ;(car choice)是分派函数
                           succeed
                           (lambda () ;新的失败函数 F1,是个闭包,可以选amb的下一个值
                             (try-next (cdr choices))))))
      (try-next cprocs))))
(define (let? exp) (tagged-list? exp 'let))
(define (let-body exp)  (cddr exp))
(define (let-clauses exp)  (cadr exp))
(define (analyze-let exp)
  (analyze (cons (make-lambda (map car (let-clauses exp))
                              (let-body exp)) (map cadr (let-clauses exp)))))

#|
; update:2016年5月26日11:53:45 更新analyze-let
(define (analyze-let exp)
  (define (getVariableNames let-lst)
    ;(map (lambda (x) (eval (car x) env)) let-lst)
    (map (lambda (x) (car x)) let-lst)
    )
  (define (getVariableValues let-lst env succeed fail)  ; let-lst是let后面第一个双括号的部分
    (map (lambda (x) (ambeval (cadr x) env succeed fail)) let-lst)
  )
  (let ((vars (getVariableNames (cadr exp)))
        (vals (lambda (env succeed fail) (getVariableValues (cadr exp) env succeed fail)))
        (beginlst (analyze (cons 'begin (cddr exp)))))  ;更正:这里应该是cddr
    (lambda (env succeed fail) (beginlst (extend-environment vars (vals env succeed fail) env) succeed fail));相当于我在这里做了更多的工作。
     )
  )
|#
#|
(define (eval-let exp env success fail) ; TODO 这里的let要怎么写？
  (define (getVariableNames let-lst)
    ;(map (lambda (x) (eval (car x) env)) let-lst)
    (map (lambda (x) (car x)) let-lst)
   )
  (define (getVariableValues let-lst)  ; let-lst是let后面第一个双括号的部分
    (map (lambda (x) (eval (cadr x) env)) let-lst)
  )
  (let ((var (getVariableNames (car exp)))
        (val (getVariableValues (car exp))))
    ;(extend-environment var val glb-env)
;    (if (= (length exp) 1)
;        (eval (cdr exp))   ;不能确定exp是一个还是一组
    (eval (cons 'begin (cdr exp)) (extend-environment var val env)) ; 不过这样会导致执行效率大大降低啊  
  )
  )
|#
(define primitive-procedures ;预定义过程列表。预定义过程必须和scheme基本过程对应吗?
 (list
  
  (list 'car car)
 ;      (list 'and list)
;        (list 'or or)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list 'number? number?)
        (list 'symbol? symbol?)
        (list 'eq? eq?)
        (list 'pair? pair?)
        (list 'not not)
        (list 'length length)
        (list 'cadr cadr)
        (list 'caddr caddr)
        (list 'cadddr cadddr)
        (list 'void void)
        (list 'list list)
        (list 'append append)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list '> >)
        (list '< <)
        (list '= =)
        (list 'remainder remainder)
        (list 'sqrt sqrt)
        (list 'display display)
))
 
(define (primitive-procedure-names) ;预定义过程名字列表
 (map car
 primitive-procedures))
(define (primitive-procedure-objects) ;生成预定义过程的函数对象列表
 (map (lambda (proc) (list 'primitive (cadr proc)))
 primitive-procedures))

;====环境设置
(define rq '(define (require p)
 (if (not p) (amb)(void))))
(define glb-succeed
  (lambda (val next)
    ;(display "succeed,val = " )
    (if (void? val)
        (void)
        (display val))
    (newline)))
(define glb-fail
  (lambda ()
    (display "glb-failed") (newline)))

(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))
(define glb-env (setup-environment)) ;初始的全局环境
(ambeval rq glb-env (lambda (val fail) (void)) glb-fail)  ;更改成功函数以抑制输出

;====测试语句
#|
(define testdef '(let ((k (list (amb 1 2 3) (amb 4 5 6))))
  (require (> (+ (car k) (car (cdr k))) 7))
  k)
)
(ambeval testdef glb-env glb-succeed glb-fail)

|#

;(define testdef '(display (list (amb 1 2 3) (amb 4 5 6))))
#|
(define testdef '(define xxx (list (amb 1 2 3) (amb 4 5 6))))
(ambeval testdef glb-env glb-succeed glb-fail)
(define testdef2 '(display xxx))
(ambeval testdef2 glb-env glb-succeed glb-fail)
|#

;(set!)

;====输出设置
(define (user-print object)
  (cond ((compound-procedure? object) (void))
        ((void? object) (void))        
        (else (display object) (newline))
      )
  )
(define (driver-loop)
  (define (internal-loop try-again)
    (let ((input (read)))
      (if (eq? input eof)
          (void)
          (if (eq? input 'try-again)
              (try-again) ;输入为try-again时,会回溯到最后的amb表达式求另一个解
              (begin 
                ;(newline)(display ";;; Starting a new problem ")
                     (ambeval input
                              glb-env
                              (lambda (val next-alternative);成功函数
                                ;next-alternative是失败函数
                                (user-print val)
                                (internal-loop next-alternative))
                              ;第一次成功以后，try-again就变成失败函数
                              (lambda () ;失败函数
                                (display "no answer")
                                (newline)
                                ;(user-print input)
                                (driver-loop))))))))
  (internal-loop
   (lambda ()
     (newline) (display ";;; There is no current problem")
     (driver-loop))))
(driver-loop)

