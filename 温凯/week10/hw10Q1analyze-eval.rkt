#lang racket
(require (except-in r5rs eval))

(define (eval exp env)
 ((analyze exp) env))
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
(define (analyze-self-evaluating exp)   (lambda (env) exp))
(define (analyze-quoted exp) (let ((qval (text-of-quotation exp))) (lambda (env) qval)))
;单引号开头的表达式会被scheme自动转换成 (quote ...)列表形式
(define (text-of-quotation exp) (cadr exp))
(define (analyze-variable exp) (lambda (env) (lookup-variable-value exp env)))
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
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env)
      (set-variable-value! var (vproc env) env))
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
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env)
      (if (true? (pproc env))
          (cproc env)
          (aproc env)))))
(define (analyze-lambda exp) ;exp是lambda表达式
 (let ((vars (lambda-parameters exp))
 (bproc (analyze-sequence (lambda-body exp))))
;bproc是可执行函数,需要以环境作为参数
 (lambda (env) (make-procedure vars bproc env))))
(define (make-procedure parameters body env)
 (list 'procedure parameters body env))
(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)));fproc是一个可执行函数
        (aprocs (map analyze (operands exp))));aprocs是可执行函数列表
    (lambda (env) 
      (execute-application (fproc env);(fproc env)是函数对象
                           (map (lambda (aproc) (aproc env))
                                aprocs)))))
(define (primitive-procedure? proc)
 (tagged-list? proc 'primitive))
(define (primitive-implementation proc) (cadr proc))
(define (compound-procedure? p)
 (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

(define (execute-application proc args)
  ; 我的天，调了这么长时间，只是因为忘了一层括号！！！！！！
  (cond ((primitive-procedure? proc) (apply (primitive-implementation proc) args))
        ((compound-procedure? proc) ((procedure-body proc) (extend-environment (procedure-parameters proc)
                                                                              args
                                                                              (procedure-environment proc))))
        (else (error
               "Unknown procedure type -- EXECUTE-APPLICATION"
               proc))
        )
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
  (define (sequentially proc1 proc2)
    (lambda (env) (proc1 env) (proc2 env)))
  (define (loop first-proc rest-procs);rest-procs是表达式列表
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)));procs是可执行函数列表
    (if (null? procs)
        (error "Empty sequence -- ANALYZE"))
    (loop (car procs) (cdr procs))))
(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env) ;vproc是一个可执行函数
      (define-variable! var (vproc env) env)
      )))


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

(define (let? exp) (tagged-list? exp 'let))
(define (analyze-let exp)
  (let ((pars (operands exp)))
    (lambda (env) (eval-let pars env))
     )
  )
(define (eval-let exp env) ; TODO 这里的let要怎么写？
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
))
 
(define (primitive-procedure-names) ;预定义过程名字列表
 (map car
 primitive-procedures))
(define (primitive-procedure-objects) ;生成预定义过程的函数对象列表
 (map (lambda (proc) (list 'primitive (cadr proc)))
 primitive-procedures))

;====环境设置

(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))
(define glb-env (setup-environment)) ;初始的全局环境



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
          (user-print (eval a glb-env))
          (myloop)
          ))))
(myloop)




;======= 测试部分
(define following-def '(
                        (define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1 ) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))



(define (addend s)
  (define (add-iter s)
    (if (eq? (cadr s) '+)
        (list (car s))
        (cons (car s) (add-iter (cdr s)))))
  (if (eq? (cadr s) '+)
      (car s)
      (add-iter s)))


(define (augend s)
  (if (eq? (cadr s) '+)
      (let ((r (cdr (cdr s))))
        (if (= (length r) 1)
            (car r)
            r))
      (augend (cdr s))))
  
(define (sum? x) 
  (if (null? x)
      false
      (if (eq? (car x) '+)
          true
          (sum? (cdr x)))))
      
(define (make-sum a1 a2) 
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else 
          (cond ((and (pair? a1) (pair? a2)) (append a1 (append (list '+) a2)))
                ((and (pair? a1) (not (pair? a2))) (append a1 (list '+ a2)))
                ((and (not (pair? a1)) (pair? a2)) (cons a1 (cons '+ a2)))
                ((and (not (pair? a1)) (not (pair? a2))) (list a1 '+ a2))))))


(define (make-product a1 a2) 
  (cond ((=number? a1 0) 0)
        ((=number? a2 0) 0)
        ((=number? a1 1) a2)
        ((=number? a2 1) a1)
        ((and (number? a1) (number? a2)) (* a1 a2))
        (else 
          (cond ((and (pair? a1) (pair? a2)) 
                   (cond ((and (sum? a1) (sum? a2) (list a1 '* a2)))
                         ((and (sum? a1) (not (sum? a2)) (append (list a1 '*) a2)))
                         ((and (not (sum? a1) (sum? a2)) (append a1 (list '* a2))))
                         ((and (not (sum? a1) (not (sum? a2))) (append a1 (cons '* a2))))))
                ((and (pair? a1) (not (pair? a2)))
                      (cond ((sum? a1) (list a1 '* a2))
                         ((not (sum? a1)) (append a1 (list '* a2)))))
                ((and (not (pair? a1)) (pair? a2))
                      (cond ((sum? a2) (list a1 '* a2))
                         ((not (sum? a2)) (cons a1 (cons '* a2)))))
                ((and (not (pair? a1)) (not (pair? a2))) (list a1 '* a2))))))


(define (multiplier s)
  (car s))

(define (multiplicand s)
  (if (= 3 (length s))
      (cadr (cdr s))
      (cdr (cdr s))))

(define (product? x) 
  (if (not (pair? x))
      false
      (if (= (length x) 3)
          (eq? (cadr x) '*)
          (and (eq? (cadr x) '*) (product? (cdr (cdr x)))))))




(define (make-expo x n)  (list '** x n))
(define (expo-base s) (cadr s))
(define (expo-e s)  (caddr s))
(define (expo? s)  (and (pair? s ) (eq? (car s) '**)))



  
  



(define (deriv exp var)
  (cond ((number? exp ) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum 
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        ((expo? exp)
         (make-product
          (expo-e exp) (make-product (make-expo (expo-base exp) (- (expo-e exp) 1)) (deriv (expo-base exp) var))))
        (else 
         (error "wrong format"))))

(deriv '(x * 3) 'x)
(deriv '(y * x) 'x)
(deriv '(x * y) 'x)
(deriv '(x * y * (x + 3)) 'x)


(multiplier '(3 * 5 * 6));
(multiplicand '(3 * 5 * 6))
(make-product 3 'x)
(deriv '(x + 3) 'x)
(deriv '(x * x + x * x * y * (x + 3) + x * y) 'x)
                        ))
((analyze-sequence following-def) glb-env)

(define f-definition '(define (f x) 
  (define (g y)
    (k y))
  (define (k z)
    (+ z 1))
  (* (g x) x)))
(eval f-definition glb-env)
(eval '(f 5) glb-env)
(eval '(f 6) glb-env)


(eval '(define (myfun x) (+ x 1)) glb-env)

(display (eval '(myfun 5) glb-env) )


