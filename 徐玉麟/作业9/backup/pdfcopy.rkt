#lang racket
(require r5rs)

(define (eval exp env)
(cond ((self-evaluating? exp) exp); 自求值表达式
((variable? exp) (lookup-variable-value exp env))
((quoted? exp) (text-of-quotation exp)); 单引号表达式
((assignment? exp) (eval-assignment exp env)); 赋值语句
((definition? exp) (eval-definition exp env)); 特殊形式define
((if? exp) (eval-if exp env))
((lambda? exp)
(make-procedure (lambda-parameters exp)
(lambda-body exp)
env)); 生成过程对象
((begin? exp)
(eval-sequence (begin-actions exp) env))
((cond? exp) (eval (cond->if exp) env));cond 转换为if
((application? exp); 除了上面各种情况之外的,都认为是函数调用表达式
(my-apply (eval (operator exp) env)
(list-of-values (operands exp) env)))
(else
(error "Unknown expression type -- EVAL" exp))))

(define (self-evaluating? exp)
(cond ((number? exp) true) ;number? 是scheme 基本过程
((string? exp) true) ;string? 是scheme 基本过程
(else false)))
(define (variable? exp) (symbol? exp)) ;symbol? 是scheme 基本过程
(define (quoted? exp)
(tagged-list? exp 'quote))
; 单引号开头的表达式会被scheme 自动转换成 (quote ...) 列表形式
(define (text-of-quotation exp) (cadr exp))
(define (tagged-list? exp tag)
(if (pair? exp)
(eq? (car exp) tag)
false))

(define (assignment? exp)
(tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

(define (definition? exp)
(tagged-list? exp 'define));exp 形如(define ....)
(define (definition-variable exp)
(if (symbol? (cadr exp))
(cadr exp) ; 针对第一种形式
(caadr exp))); 针对第二种形式,此时变量名就是函数名
(define (definition-value exp)
(if (symbol? (cadr exp))
(caddr exp); 针对第一种形式
(make-lambda (cdadr exp) ; formal parameters
(cddr exp)))) ; body

(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp)) ;body 可能是个表达式序列

(define (make-lambda parameters body) ; 构造一个lambda 表达式
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
; 下面seq 是一个列表,每个元素都是exp
(define (last-exp? seq) (null? (cdr seq))); 判断seq 里是否只有一个表达式
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))
(define (sequence->exp seq); 把表达式列表变成 一个表达式
(cond ((null? seq) seq)
((last-exp? seq) (first-exp seq))
(else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))

(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp)) ; 返回所有分支的列表
; 以下clause 是一个条件分支,如 ((> x 3) (+ x 3) (* x 3))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond-else-clause? clause)
(eq? (cond-predicate clause) 'else))
(define (cond->if exp)
(expand-clauses (cond-clauses exp)))

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
(list 'if predicate consequent alternative))

(define (application? exp) (pair? exp))
;exp 是函数调用表达式的前提下:
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
; 下面ops 是操作数的列表
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))


(define (eval-assignment exp env)
(set-variable-value! (assignment-variable exp)
(eval (assignment-value exp) env)
env)
'ok)

(define (eval-definition exp env)
(define-variable! (definition-variable exp)
(eval (definition-value exp) env)
env)
'ok)

(define (eval-if exp env)
(if (true? (eval (if-predicate exp) env))
(eval (if-consequent exp) env)
(eval (if-alternative exp) env)))

(define (make-procedure parameters body env)
(list 'procedure parameters body env))
; 过程对象是一个列表,包含参数和函数体。
;parameters 是一个列表，元素就是参数的名字，形如(x y) 。
;body 是函数体，形如：(* x y)
(define (compound-procedure? p)
(tagged-list? p 'procedure))
; 过程对象形如： '(procedure (x y) (* x y) env) env是指向环境的指针
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

(define (eval-sequence exps env)
(cond ((last-exp? exps) (eval (first-exp exps) env))
(else (eval (first-exp exps) env)
(eval-sequence (rest-exps exps) env))))

(define (lookup-variable-value var env)
(define (env-loop env)
(define (scan vars vals)
(cond ((null? vars)
(env-loop (enclosing-environment env))); 到外围环境继续找
((eq? var (car vars))
(car vals))
(else (scan (cdr vars) (cdr vals)))))
(if (eq? env the-empty-environment)
(error "Unbound variable" var)
(let ((frame (first-frame env)))
(scan (frame-variables frame)
(frame-values frame)))))
(env-loop env))

(define (make-frame variables values)
(mcons variables values)); 框架形如 ((x y z) 1 2 3)
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
(set-car! frame (mcons var (car frame)))
(set-cdr! frame (mcons val (cdr frame))))
(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (extend-environment vars vals base-env)
(if (= (length vars) (length vals))
(cons (make-frame vars vals) base-env)
(if (< (length vars) (length vals))
(error "Too many arguments supplied" vars vals)
(error "Too few arguments supplied" vars vals))))

(define (set-variable-value! var val env); 仅被eval-assignment 调用
(define (env-loop env)
(define (scan vars vals) ;frame 形如:((a b c) 1 2 3)
(cond ((null? vars)
(env-loop (enclosing-environment env)))
((eq? var (car vars))
(set-car! vals val))
(else (scan (cdr vars) (cdr vals)))))
(if (eq? env the-empty-environment)
(error "Unbound variable -- SET!" var)
(let ((frame (first-frame env)))
(scan (frame-variables frame)
(frame-values frame)))))
(env-loop env))

(define (define-variable! var val env); 仅被eval-definition  调用
(let ((frame (first-frame env)))
(define (scan vars vals)
(cond ((null? vars); 如果变量不存在,就加到env 的第一个 frame 里面
(add-binding-to-frame! var val frame))
((eq? var (car vars))
(set-car! vals val))
(else (scan (cdr vars) (cdr vals)))))
(scan (frame-variables frame)
(frame-values frame))))

(define primitive-procedures
  (list (list 'car car)
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
        (list 'remainder remainder)))
(define (primitive-procedure-names) ; 预定义过程名字列表
(map car
primitive-procedures))
(define (primitive-procedure-objects) ; 生成预定义过程的函数对象列表
(map (lambda (proc) (list 'primitive (cadr proc)))
primitive-procedures))
; 预定义过程的函数对象形如 (primitive #<procedure:+>),不需要环境指针
(define (primitive-procedure? proc)
(tagged-list? proc 'primitive))
(define (primitive-implementation proc) (cadr proc))

(define (setup-environment)
(let ((initial-env
(extend-environment (primitive-procedure-names)
(primitive-procedure-objects)
the-empty-environment)))
(define-variable! 'true true initial-env)
(define-variable! 'false false initial-env)
initial-env))
(define glb-env (setup-environment))

(define (my-apply procedure arguments)
(cond ((primitive-procedure? procedure)
( apply (primitive-implementation procedure) arguments))
((compound-procedure? procedure)
(eval-sequence
(procedure-body procedure)
(extend-environment
(procedure-parameters procedure)
arguments
(procedure-environment procedure))))
; 每执行一次函数调用，哪怕是递归的，都要新建一个环境, 记录本次函数调用的参数的值
(else
(error "unkonwn procedure type -- APPLY" procedure))))

(define (list-of-values exps env)
    (if (no-operands? exps)
        '()
        (let ((first-value (eval (first-operand exps) env)))
            (cons first-value
                  (list-of-values (rest-operands exps) env)))))

(define (true? x) (equal? x true))

(define (driver-loop)
  (let ((input (read)))
    (if (eq? input eof)
        (void)
        (let ((output (eval input glb-env)))
          (user-print output)
          (driver-loop)))))

(define (user-print object)
  (cond ((equal? 'ok object) (void))
        ((compound-procedure? object)
         (displayln (list 'compound-procedure
                        (procedure-parameters object)
                        (procedure-body object)
                        '<procedure-env>)))
        (else (displayln object))))

(driver-loop)