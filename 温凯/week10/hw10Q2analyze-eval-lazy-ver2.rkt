#lang racket
(require (except-in r5rs eval))

(define (eval exp env)
  (cond ((self-evaluating? exp) exp);自求值表达式
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp));单引号表达式
        ((assignment? exp) (eval-assignment exp env));赋值语句
        ((definition? exp) (eval-definition exp env));特殊形式define
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env));生成过程对象
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env));cond转换为if
        ((and? exp) (eval-and (operands exp) env))
        ((or? exp) (eval-or (operands exp) env))
        ((let? exp) (eval-let (operands exp) env))
        ((application? exp);除了上面各种情况之外的,都认为是函数调用表达式
         (my-apply (actual-value (operator exp) env)
                   ;(list-of-values (operands exp) env)
                   (operands exp)
                   env))
        (else
         (error "Unknown expression type -- EVAL" exp))))
; 在处理(define (f a (b lazy) c (d lazy-memo))时，会把(b lazy)判定为application
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))
(define (and? exp)
  (tagged-list? exp 'and))

; 为了支持and的语法,需要让and返回最后的值
(define (eval-and parameters env)   ;短路的and还是非短路的and? 能否保证paremeters一定非空?  先假设 and至少有一个parameter  以及,and的返回值究竟带不带',为什么  不带'就正确 带'就错误。在内部的表示是什么?不带'的?
  (define (and-iter2 newPars)
    (let ((thisval (actual-value (car newPars) env)))
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
(define (eval-or parameters env)   ;这里是短路或
  (define (or-iter pars)
    (if (null? pars)
        false
        (if (actual-value (car pars) env)
            true
            (or-iter (cdr pars))
            )
        )
    )
  (or-iter parameters)
  )
(define (let? exp) (tagged-list? exp 'let))
(define (eval-let exp env)
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
    (eval-sequence (cdr exp) (extend-environment var val env))
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
 (tagged-list? exp 'define));exp形如(define ....)
(define (definition-variable exp)
 (if (symbol? (cadr exp))
 (cadr exp) ;针对第一种形式
 (caadr exp)));针对第二种形式,此时变量名就是函数名
(define (definition-value exp) ; 针对记忆的功能做出改进
  (if (symbol? (cadr exp))
      (caddr exp);针对第一种形式 eg (define a 10)
      (make-lambda (cdadr exp) ; formal parameters
                   (cddr exp)))) ; body 这里先不去管它，把(a (b lazy) c (d lazy-memo))原样放进去
;(define (process-define-parameters parlst) ; parlst形如 (a (b lazy) c (d lazy-memo))
;  (

(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp)) ;body可能是个表达式序列
(define (make-lambda parameters body) ;构造一个lambda表达式
 (cons 'lambda (cons parameters body)))

;分支判断函数
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
(define (last-exp? seq) (null? (cdr seq)));判断seq里是否只有一个表达式
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))
(define (sequence->exp seq);把表达式列表变成一个表达式
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))

;cond 转成if
(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp)) ;返回所有分支的列表
;以下clause是一个条件分支,如 ((> x 3) (+ x 3) (* x 3))
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

(define (application? exp) (pair? exp))

;exp是函数调用表达式的前提下:
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
;下面ops是操作数的列表
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define (eval-assignment exp env)
 (set-variable-value! (assignment-variable exp)
 (eval (assignment-value exp) env)
 env)
 ;'ok)
  (void))
(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
    (eval (definition-value exp) env)
    env)
 ;'ok)
  (void))
(define (eval-if exp env)
  (if (true? (actual-value (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))
(define (true? val)
  ;(displayln "in true============")
  (or (eq? val 'true) val)
  )
  
;
(define (make-procedure parameters body env)
 (list 'procedure parameters body env))
;过程对象是一个列表,包含参数和函数体。
;parameters是一个列表,元素就是参数的名字,形如(x y)。
;body是函数体,形如:(* x y)
(define (compound-procedure? p)
 (tagged-list? p 'procedure))
;过程对象形如: '(procedure (x y) (* x y) env) env是指向环境的指针
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

  ;决定在从my-apply进入extend-environment之前就处理好
(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals)) ;在这里既能拿到形如 var ： ( a (b lazy) c (d lazy-memo))     又能拿到其对应的，经过delay的表达式 vals
        ; 没有经过很多的思考,决定把对lazy 和 lazy memo的处理放在这里。      
      (cons (make-frame vars
                        vals)
                        base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals)))
  )
  #|
(define (get-vals-according-to-var-label vars vals env)
  (let ((thisvar (car vars))
        (this-delayed-val (car vals)))
    (cond ((null? thisvar) '())
          ((pair? thisvar) (let ((lazy-label (cadr thisvar)))
                             (cond ((eq? lazy-label 'lazy) ())  ;默认情况下
                                   ((eq? lazy-label 'lazy-memo) (proc))
                                   (else (error "Invalid lazy-label: get-vals-according-to-var-label: " lazy-label))))
                           )
|#
(define (clean-vars vars) ; INPUT: var : ( a (b lazy) c (d lazy-memo))  OUTPUT: var (a b c d)
  (if (null? vars)
      '()
      (if (pair? (car vars))
          (cons (caar vars)
                (clean-vars (cdr vars)))
          (cons (car vars)  ; car vars 得到一个 symbol
                (clean-vars (cdr vars)))
          )
      )
  )
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
    (scan (frame-variables frame)
          (frame-values frame))))

(define primitive-procedures ;预定义过程列表。预定义过程必须和scheme基本过程对应吗?
 (list  (list 'car car)
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
        (list 'display display)
        (list 'newline newline)
        (list 'sqrt sqrt) 
 ; (list '+ +)
  
))
 
(define (primitive-procedure-names) ;预定义过程名字列表
 (map car
 primitive-procedures))
(define (primitive-procedure-objects) ;生成预定义过程的函数对象列表
 (map (lambda (proc) (list 'primitive (cadr proc)))
 primitive-procedures))
;预定义过程的函数对象形如 (primitive #<procedure:+>),不需要环境指针
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
(define glb-env (setup-environment)) ;初始的全局环境

(define (my-apply procedure arguments env)
  (cond ((primitive-procedure? procedure)
         (let ((tmp (list-of-arg-values arguments env)))
           (apply (primitive-implementation procedure) tmp)))
        ;tmp里面是已经求得最终值的参数的列表。原来tmp处就是arguments
        ;(primitive-implementation proc) 返回形如: #<procedure:car> ,
        ;#<procedure:my-square>之类的东西(如果my-square被定义成primitive的话)
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (clean-vars (procedure-parameters procedure))   ;这个函数会取出来形如( a (b lazy) c (d lazy-memo))
           (list-of-smart-delayed-args arguments (procedure-parameters procedure) env) ;这个会把输进去，真正要算的东西拿到
           (procedure-environment procedure))))
        ;生成一个延时求值对象的列表。每个对象对应于arguments中的一个参数
        ;每执行一次函数调用,哪怕是递归的,都要新建一个环境,记录本次函数调用的参数的值
        (else
         (error "unkonwn procedure type -- APPLY" procedure))))

(define (actual-value exp env)   (force-it (eval exp env)));(eval exp env)的结果可能是个延时求值对象
(define (list-of-arg-values exps env)
  (if (no-operands? exps)
      '()
      (cons (actual-value (first-operand exps) env)
            (list-of-arg-values (rest-operands exps) env))
      )
  )
; 从这里入手还是list-of-delayed-values?
(define (list-of-delayed-args exps env)
  (if (no-operands? exps)
      '()
      (cons (delay-it (first-operand exps) env)
            (list-of-delayed-args (rest-operands exps) env)))
  )

; 是list-of-delayed-values和list-of-values的综合体，作用是生成值或者延时求值对象
; labeled-vars 通过(procedure-parameters procedure)获得，形如 ( a (b lazy) c (d lazy-memo))
; args是单纯的计算式
; 没有对参数的长度进行检查
; 需要对'procedure的定义进行一定的拓展
; 对于过程的计算仅仅取决于env中有没有注册对应的变量，而不是在make-procedure的时候注册的变量名
(define (list-of-smart-delayed-args args labeled-vars env)
  (if (null? args)
      '()
      (let ((thisvar (car labeled-vars))
            (thisarg (car args)))
        (cond ((null? thisvar) '())
              ((pair? thisvar) (let ((lazy-label (cadr thisvar)))
                                 (cond ((eq? lazy-label 'lazy) (cons (delay-it-nomem thisarg env)
                                                                     (list-of-smart-delayed-args (cdr args) (cdr labeled-vars) env)))  
                                       ((eq? lazy-label 'lazy-memo) (cons (delay-it-memo thisarg env)
                                                                          (list-of-smart-delayed-args (cdr args) (cdr labeled-vars) env)))
                                       (else (error "Invalid lazy-label: get-vals-according-to-var-label: " lazy-label))))
                               )
              (else (cons (actual-value thisarg env)   ;默认情况下参数是严格的，假设actual-value总能获得最后的数值结果
                          (list-of-smart-delayed-args (cdr args) (cdr labeled-vars) env)))
              )
        )
      )
  )
(define (delay-it exp env)   (list 'thunk exp env))
(define (delay-it-nomem exp env) (list 'thunk-nomem exp env))
(define (delay-it-memo exp env) (list 'thunk-memo exp env))
(define (force-it obj)
 (cond ((or (thunk? obj) (thunk-memo? obj)) (let ((result (actual-value (thunk-exp obj) (thunk-env obj))))
                       (set-car! obj 'evaluated-thunk)
                       (set-car! (cdr obj) result)
                       (set-cdr! (cdr obj) '())
                       result))
       ((thunk-nomem? obj) (actual-value (thunk-exp obj) (thunk-env obj)))
       ((evaluated-thunk? obj) (thunk-value obj))
       (else obj))
  )
;槽相关定义
(define (thunk? obj) (tagged-list? obj 'thunk))
(define (thunk-nomem? obj) (tagged-list? obj 'thunk-nomem)) ;作为不记忆的槽使用，thunk直接作为带记忆的槽使用
(define (thunk-memo? obj) (tagged-list? obj 'thunk-memo))
(define (thunk-exp thunk) (cadr thunk)) 
(define (thunk-env thunk) (caddr thunk))
(define (evaluated-thunk? obj) ;判断槽是否已经求值过 
  (tagged-list? obj 'evaluated-thunk))
(define (thunk-value evaluated-thunk) (cadr evaluated-thunk))

(define (user-print object)
  (cond ((compound-procedure? object) (void))
        ((void? object) (void))        
        (else (display object) (newline))
      )
  )



(define (myloop)
  (let ((a (read)))
    (if (eq? a eof)
        (void)
        (begin
          (user-print (actual-value a glb-env))
          (myloop)
          ))))
(myloop)







(actual-value '(begin (display "case 1:") (newline)
(define (func (x lazy)) x)
(define w (func (/ 1 0)))
(define k w)
(define (try a (b lazy))
         (if (= a 0) 
             a 
             b))
(try 0 k)
(display "case 2:") (newline)
(define (p1 x)
  (define (p (e lazy))
    e
    x)
  (p (set! x (cons x '(2)))))
(p1 1)


(display "case 3:") (newline)
(define (p2 x)
  (define (p e)
    e
    x)
  (p (set! x (cons x '(2)))))
(p2 1)


(define (inc x)
  (display "x=") (display x) (newline)
  (set! x (+ 1 x))
  x)

(display "case 4:") (newline)
(define (f1 x)
  (> x 1) x)
(f1 (inc 3))


(display "case 5:") (newline)
(define (f2 (x lazy))
  (> x 1) x)
(f2 (inc 3))


(display "case 6:") (newline)
(define (f3 (x lazy-memo))
  (> x 1) x)
(f3 (inc 3))


(display "case 7:") (newline)
(define (f x (y lazy) (z lazy-memo))
  x (+ x 1) (+ x 1)
  y ( + y 1) (+ y 1) 
  z (> z 1) (+ z 1))
(f (inc 2) (inc 20) (inc 200))

) glb-env)



;=====测试
