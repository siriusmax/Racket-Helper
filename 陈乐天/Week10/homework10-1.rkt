#lang racket

(require compatibility/mlist)

(define (eval exp env)
  ((analyze exp) env))
(define (analyze exp) ;返回值是一个可执行函数(不是源代码形式的也不是函数对象), 以环境为参数。
  (cond ((self-evaluating? exp)
         (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((and? exp) (analyze-and (operands exp)))
        ((or? exp) (analyze-or (operands exp)))
        ((let? exp) (analyze-let (operands exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((application? exp) (analyze-application exp))
        (else (error "Unknown expression type -- ANALYZE" exp))))



(define (and? exp)
  (tagged-list? exp 'and))
(define (or? exp)
  (tagged-list? exp 'or))
(define (let? exp)
  (tagged-list? exp 'let))
(define (analyze-and parameters)
  (let ((analyzed-and-parameters (map analyze parameters)))
    (define (inner-eval-and parameters env)
      (let ((now ((car parameters) env)))
        (if (false? now)
            false
            (if (null? (cdr parameters))
                now
                (inner-eval-and (cdr parameters) env))))) 
    (lambda (env) (inner-eval-and analyzed-and-parameters env))))

(define (analyze-or parameters)
  (let ((analyzed-or-parameters (map analyze parameters)))
    (define (inner-eval-or parameters env)
      (if (null? parameters)
          false
          (let ((now ((car parameters) env)))
            (if (true? now)
                true
                (if (null? (cdr parameters))
                    now
                    (inner-eval-or (cdr parameters) env))))))
    (lambda (env) (inner-eval-or analyzed-or-parameters env))))

(define (analyze-let body)
  (let ((definelist (car body))
        (beginlist (analyze (cons 'begin (cdr body))))
        (vars (mlist))
        (vals (mlist)))
    (map (lambda (x)
           (set! vars (mcons (car x) vars))
           (set! vals (mcons (analyze (cadr x)) vals)))
         definelist)
    (lambda (env) (beginlist (extend-environment vars (mmap (lambda(val) (val env)) vals) env)))))






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
            ((eq? var (mcar vars))
             (mcar vals))
            (else (scan (mcdr vars) (mcdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame) (frame-values frame)))))
  (env-loop env))



;frame
(define (make-frame variables values)
  (mcons variables values));框架形如 ((x y z) 1 2 3)
(define (frame-variables frame) (mcar frame))
(define (frame-values frame) (mcdr frame))
(define (add-binding-to-frame! var val frame)
  (set-mcar! frame (mcons var (mcar frame)))
  (set-mcdr! frame (mcons val (mcdr frame))))
(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment (list))

;environment
(define (extend-environment vars vals base-env)
  (if (= (mlength vars) (mlength vals))
      (cons (make-frame vars vals) base-env)
      (if (< (mlength vars) (mlength vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

;set-variable-value
(define (set-variable-value! var val env);仅被eval-assignment调用
  (define (env-loop env)
    (define (scan vars vals) ;frame形如:((a b c) 1 2 3)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (mcar vars))
             (set-mcar! vals val))
            (else (scan (mcdr vars) (mcdr vals)))))
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
            ((eq? var (mcar vars))
             (set-mcar! vals val))
            (else (scan (mcdr vars) (mcdr vals)))))
    (scan (frame-variables frame) (frame-values frame))))


;setup-environment
(define (setup-environment)
  (let ((initial-env (extend-environment (primitive-procedure-names) (primitive-procedure-objects) the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env) initial-env))



;primitive-procedures
(define primitive-procedures ;预定义过程列表。预定义过程必须和scheme基本过程对应吗?
  (mlist (list 'car car)
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
  (mmap car
        primitive-procedures))
(define (primitive-procedure-objects) ;生成预定义过程的函数对象列表
  (mmap (lambda (proc) (list 'primitive (cadr proc))) primitive-procedures))
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
                                           (list->mlist (procedure-parameters procedure))
                                           (list->mlist arguments)
                                           (procedure-environment procedure)))) ;每执行一次函数调用,哪怕是递归的,都要新建一个环境,记录本次函数调用的参数的值
        (else
         (error "unkonwn procedure type -- APPLY" procedure))))



(define glb-env (setup-environment)) ;初始的全局环境
;(display glb-env)















(define (analyze-self-evaluating exp)
  (lambda (env) exp))
(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env) qval)))
(define (analyze-variable exp)
  (lambda (env) (lookup-variable-value exp env)))

;assignment
(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env) ;vproc是一个可执行函数,(vproc env)才是变量的值
      (set-variable-value! var (vproc env) env)
      'ok)))
;definition
(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env) ;vproc是一个可执行函数
      (define-variable! var (vproc env) env) 'ok)))
;if
(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env)
      (if (true? (pproc env)) (cproc env)
          (aproc env)))))
(define (analyze-lambda exp) ;exp是lambda表达式
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence (lambda-body exp))));bproc是可执行函数,需要以环境作为参数
    (lambda (env) (make-procedure vars bproc env))))
(define (make-procedure parameters body env) (list 'procedure parameters body env))
;application
(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)));fproc是一个可执行函数
        (aprocs (map analyze (operands exp))));aprocs是可执行函数列表
    (lambda (env)
      (execute-application (fproc env);(fproc env)是函数对象
                           (map (lambda (aproc) (aproc env))
                                aprocs)))))
;execute-application
(define (execute-application proc args) ;proc形如: (procedure (x y) #<procedure:XXX> env) ;原来的my-apply中, #<procedure:XXX>处是具体的源代码
  (cond ((primitive-procedure? proc)
         (apply (primitive-implementation proc) args))
        ((compound-procedure? proc)
         ((procedure-body proc) ;形如: #<procedure:XXX>
          (extend-environment (list->mlist (procedure-parameters proc)) (list->mlist args)
                              (procedure-environment proc))))
        (else (error
               "Unknown procedure type -- EXECUTE-APPLICATION" proc))))
;sequence
(define (analyze-sequence exps) ;exps是表达式列表,形如 ((* x y) (+ x 4)....)
  (define (sequentially proc1 proc2) (lambda (env) (proc1 env) (proc2 env)))
  (define (loop first-proc rest-procs);rest-procs是表达式列表
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)));procs是可执行函数列表
    (if (null? procs)
        (error "Empty sequence -- ANALYZE")
        (void))
    (loop (car procs) (cdr procs))))






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