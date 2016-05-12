#lang racket
(require (except-in r5rs eval))


;eval
(define (eval exp env)
  (cond ((self-evaluating? exp) exp);自求值表达式
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp));单引号表达式
        ((assignment? exp) (eval-assignment exp env));赋值语句
        ((definition? exp) (eval-definition exp env));特殊形式define
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp) env));生成过程对象
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env));cond转换为if
        ((and? exp) (eval-and (operands exp) env))
        ((or? exp) (eval-or (operands exp) env))
        ((let? exp) (eval-let (operands exp) env))
        ((application? exp);除了上面各种情况之外的,都认为是函数调用表达式
         (my-apply (eval (operator exp) env) (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))

(define (and? exp)
  (tagged-list? exp 'and))
(define (or? exp)
  (tagged-list? exp 'or))
(define (let? exp)
  (tagged-list? exp 'let))
(define (eval-and parameters env)
  (define (inner-eval-and parameters env)
    (let ((now (eval (car parameters) env)))
      (if (false? now)
          false
          (if (null? (cdr parameters))
              now
              (inner-eval-and (cdr parameters) env))))) 
  (inner-eval-and parameters env))

(define (eval-or parameters env)
  (define (inner-eval-or parameters env)
    (if (null? parameters)
        false
        (let ((now (eval (car parameters) env)))
          (if (true? now)
              true
              (if (null? (cdr parameters))
                  now
                  (inner-eval-or (cdr parameters) env))))))
  
  (inner-eval-or parameters env))

(define (eval-let body env)
  (let ((definelist (car body))
        (beginlist (cons 'begin (cdr body)))
        (vars '())
        (vals '()))
    (map (lambda (x)
           (set! vars (mcons (car x) vars))
           (set! vals (mcons (eval (cadr x) env) vals)))
         definelist)
    (eval beginlist (extend-environment vars vals env))))

;simple
(define (self-evaluating? exp)
  (cond ((number? exp) true) ;number?是scheme基本过程
        ((string? exp) true) ;string?是scheme基本过程
        (else false)))
(define (variable? exp) (symbol? exp)) ;symbol?是scheme基本过程
(define (quoted? exp)
  (tagged-list? exp 'quote))
(define (list-of-values lst env)
  (if (empty? lst) '()
      (cons (eval (car lst) env)
            (list-of-values (cdr lst) env))))
(define true #t)
(define false #f)
(define (false? x)
  (equal? x false))
(define (true? x)
  (not (false? x)))
;单引号开头的表达式会被scheme自动转换成 (quote ...)列表形式
(define (text-of-quotation exp) (cadr exp))
(define (tagged-list? exp tag) (if (pair? exp)
                                   (eq? (car exp) tag)
                                   false))

;set
(define (assignment? exp)
  (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))


;define
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

;lambda
(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp)) ;body可能是个表达式序列
(define (make-lambda parameters body) ;构造一个lambda表达式
  (cons 'lambda (cons parameters body)))


;if
(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

;begin
(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)));判断seq里是否只有一个表达式
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq);把表达式列表变成一个表达式
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))

;cond
(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp)) ;返回所有分支的列表 ;以下clause是一个条件分支,如 ((> x 3) (+ x 3) (* x 3))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause)
  (let ((action (cdr clause)))
    (if (null? action)
        (list (cond-predicate clause))
        action)))
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


;application
(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp)) ;下面ops是操作数的列表
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))


;eval-many
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
(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env) (eval-sequence (rest-exps exps) env))))

;procedure
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))



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
          (scan (frame-variables frame) (frame-values frame)))))
  (env-loop env))



;frame
(define (make-frame variables values)
  (cons variables values));框架形如 ((x y z) 1 2 3)
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame) (set-car! frame (cons var (car frame))) (set-cdr! frame (cons val (cdr frame))))
(define (enclosing-environment env) (cdr env)) (define (first-frame env) (car env))
(define the-empty-environment '())

;environment
(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

;set-variable-value
(define (set-variable-value! var val env);仅被eval-assignment调用
  (define (env-loop env)
    (define (scan vars vals) ;frame形如:((a b c) 1 2 3)
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


(define (define-variable! var val env);仅被eval-definition 调用
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars);如果变量不存在,就加到env的第一个 frame里面
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame) (frame-values frame))))


;setup-environment
(define (setup-environment)
  (let ((initial-env (extend-environment (primitive-procedure-names) (primitive-procedure-objects) the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env) initial-env))



;primitive-procedures
(define primitive-procedures ;预定义过程列表。预定义过程必须和scheme基本过程对应吗?
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
        (list 'remainder remainder)
        (list 'sqrt sqrt)))
(define (primitive-procedure-names) ;预定义过程名字列表
  (map car
       primitive-procedures))
(define (primitive-procedure-objects) ;生成预定义过程的函数对象列表
  (map (lambda (proc) (list 'primitive (cadr proc))) primitive-procedures))
;预定义过程的函数对象形如 (primitive #<procedure:+>),不需要环境指针
(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))
(define (primitive-implementation proc) (cadr proc))




(define (my-apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply (primitive-implementation procedure) arguments))
        ;(primitive-implementation proc) 返回形如: #<procedure:car> , #<procedure:my-square>之类的东西(如果my-square被定义成primitive的话)
        ((compound-procedure? procedure) (eval-sequence
                                          (procedure-body procedure)
                                          (extend-environment
                                           (procedure-parameters procedure)
                                           arguments
                                           (procedure-environment procedure)))) ;每执行一次函数调用,哪怕是递归的,都要新建一个环境,记录本次函数调用的参数的值
        (else
         (error "unkonwn procedure type -- APPLY" procedure))))



(define glb-env (setup-environment)) ;初始的全局环境
;(display glb-env)


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
         (display (list 'compound-procedure
                        (procedure-parameters object)
                        (procedure-body object)
                        '<procedure-env>))
         (newline))
        (else (display object)(newline))))

(driver-loop)