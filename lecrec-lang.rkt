#lang eopl

(require rackunit "eopl-extras.rkt")

;; Q1 Answered

;; Q2 Answered

;; Q3 Not Answered

;; Q4 Not Answered

;;;;;;;;;;;;;;;; grammatical specification ;;;;;;;;;;;;;;;;

(define the-lexical-spec
  '((whitespace (whitespace) skip)
    
    (comment ("%" (arbno (not #\newline))) skip)
    
    (identifier
     (letter (arbno (or letter digit "_" "-" "?"))) symbol)
    
    (number (digit (arbno digit)) number)
    
    (number ("-" digit (arbno digit)) number)
    ))

(define the-grammar
  '((program (expression) a-program)
    
    (expression (number) const-exp)

    (expression ("true") true-exp)

    (expression ("false") false-exp)

    (expression (identifier) var-exp)
    
    (expression("-" "(" expression "," expression ")")diff-exp)
    
    (expression ("zero?" "(" expression ")") zero?-exp)
    
    (expression
     ("if" expression "then" expression "else" expression) if-exp)
    
    (expression 
     ("let" (arbno identifier "=" expression) "in" expression) let-exp)
    
    (expression
     ("letrec" (arbno identifier "(" (arbno identifier) ")" "=" expression)
               "in" expression) letrec-exp)
    
    (expression ("proc" "(" (arbno identifier) ")" expression) proc-exp)
    
    (expression ("(" expression (arbno expression) ")") call-exp)

    ;; UPDATING THE GRAMMAR Q1
    (expression ("cons" "(" expression "," expression ")") cons-exp)

    (expression ("car" "(" expression ")") car-exp)

    (expression ("cdr" "(" expression ")") cdr-exp)

    (expression ("null?" expression) null?-exp)

    (expression ("list" "(" (arbno expression (arbno ",")) ")") list-exp)

    (expression ("emptylist") emptylist-exp)

    ;; UPDATING THE GRAMMAR Q2
    (expression ("let*" (arbno identifier "=" expression) "in" expression) let*-exp)
    
    ))

;;;;;;;;;;;;;;;; sllgen boilerplate ;;;;;;;;;;;;;;;;

(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))

(define just-scan
  (sllgen:make-string-scanner the-lexical-spec the-grammar))

;;;;;    ENVIRONMENT    ;;;;;

(define-datatype environment environment?
  (empty-env)
  (extend-env 
   (bvar symbol?)
   (bval expval?)
   (saved-env environment?)))

(define (apply-env env search-sym)
  (cases environment env
    (empty-env ()
               (eopl:error 'apply-env "No binding for ~s" search-sym))
    (extend-env (var val saved-env)
                (if (eqv? search-sym var)
                    val
                    (apply-env saved-env search-sym)))))

;;;;;;;;;;;;;;;; expressed values: Num, Bool, Proc, emptylist, cons, list

;; INTERFACE

;; CONSTRUCTORS

;; emptylist-val: '() -> expval
;; cons-val: expval expval -> expval
;; list-val: (listof expval) -> expval

;; OBSERVERS

;; expval->emptylist: expval -> emptylist
;; expval->cons: expval -> cons
;; expval->list: expval -> list

(define-datatype expval expval?
  (num-val
   (value number?))
  (bool-val
   (boolean boolean?))
  (proc-val 
   (proc proc?))
  ;; NEW EXPVALS
  (emptylist-val
   (emptylist null?))
  (cons-val
   (car expval?)
   (cdr expval?))
  (list-val
   (lists (list-of expval?))))  

;;; extractors:

;; expval2num : ExpVal -> Int
;; expval --> Int throws error
;; Purpose: Extract number from given expval
(define (expval2num v)
  (cases expval v
    (num-val (num) num)
    (else (expval-extractor-error 'num-val v))))

;; expval --> Bool throws error
;; Purpose: Extract Boolean from given expval
(define (expval2bool v)
  (cases expval v
    (bool-val (bool) bool)
    (else (expval-extractor-error 'bool-val v))))

;; expval --> proc throws error
;; Purpose: Extract proc from given expval
(define (expval2proc v)
  (cases expval v
    (proc-val (proc) proc)
    (else (expval-extractor-error 'proc-val v))))

;; NEW EXPVAL FUNCTIONS

;; expval --> emptylist throws error
;; Purpose: Extract emptylist from given expval
(define (expval2emptylist v)
  (cases expval v
    (emptylist-val (emptylist) emptylist)
    (else (expval-extractor-error 'emptylist-val v))))

;; expval --> cons throws error
;; Purpose: Extract cons from given expval
(define (expval2cons v)
  (cases expval v
    (cons-val (car cdr)
              (cons (expval2cons car) (expval2cons cdr)))
    (num-val (num) num)
    (bool-val (bool) bool)
    (proc-val (proc) proc)
    (emptylist-val (mt) mt)
    (else (expval-extractor-error 'expval2cons v))))

;; expval --> list throws error
;; Purpose: Extract list from given expval
(define (expval2list v)
  (cases expval v
    (list-val (lists)
              (letrec [(l lists)]
                (if (null? l)
                    '()
                    (list (expval2list (car l)) (expval2list (car (cdr l)))))))
    (num-val (num) num)
    (bool-val (bool) bool)
    (proc-val (proc) proc)
    (emptylist-val (mt) mt)
    (else (expval-extractor-error 'expval2list v))))

;; symbol expval --> throws error
(define (expval-extractor-error variant value)
  (eopl:error 'expval-extractors "Looking for a ~s, given ~s"
              variant value))


;;;;;;;;;;;;;;;; procedures ;;;;;;;;;;;;;;;;

;; Any --> Boolean
;; Purpose: Determine if given value is a vector with a single environment
(define (voenv? penv)
  (and (vector? penv)
       (= (vector-length penv) 1)
       (environment? (vector-ref penv 0))))

(define-datatype proc proc?
  (procedure
   (var (list-of symbol?))
   (body expression?)
   (envv voenv?)))


;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

;; value-of-program : Program -> ExpVal
(define (value-of-program pgm)
  (cases program pgm
    (a-program (exp1)
               (value-of exp1 (empty-env)))))

;; value-of : expression environment -> expval
(define (value-of exp env)
  (cases expression exp
    
    (const-exp (num) (num-val num))

    (true-exp () (bool-val #t))

    (false-exp () (bool-val #f))
    
    (var-exp (var) (apply-env env var))
    
    (diff-exp (exp1 exp2)
              (let ((num1 (expval2num (value-of exp1 env)))
                    (num2 (expval2num (value-of exp2 env))))
                (num-val (- num1 num2))))
    
    (zero?-exp (exp1)
               (let ((val1 (expval2num (value-of exp1 env))))
                 (if (zero? val1)
                     (bool-val #t)
                     (bool-val #f))))
    
    (if-exp (exp1 exp2 exp3)
            (let ((val1 (value-of exp1 env)))
              (if (expval2bool val1)
                  (value-of exp2 env)
                  (value-of exp3 env))))
    
    (let-exp (vars exps body)       
             (let [(vals (map (lambda (e) (value-of e env)) exps))]
               (value-of body
                         (foldr (lambda (var val acc)
                                  (extend-env var val acc))
                                env
                                vars
                                vals))))

    (proc-exp (params body)
              (proc-val (procedure params body (vector env))))
    
    (call-exp (rator rands)
              (let [(proc (expval2proc (value-of rator env)))
                    (args (map (lambda (rand) (value-of rand env)) rands))]
                (apply-procedure proc args)))

    (letrec-exp (names params bodies letrec-body)
                (value-of letrec-body (mk-letrec-env names params bodies env)))

    ;; NEW VALUE-OF EXP Q1
    (cons-exp (exp1 exp2)
              (let [(val1 (value-of exp1 env))
                    (val2 (value-of exp2 env))]
                (cons-val val1 val2)))

    (car-exp (exp1)
             (let [(val (expval2cons (value-of exp1 env)))]
               (car val)))

    (cdr-exp (exp1)
             (let [(val (expval2cons (value-of exp1 env)))]
               (cdr val)))

    (null?-exp (exp1)
               (let [(val (value-of exp1 env))]
                 (if (null? val)
                     (bool-val #t)
                     (bool-val #f))))

    (list-exp (exps)
              (list-val (map (lambda (exp) (value-of exp env)) exps)))

    (emptylist-exp () (emptylist-val '()))

    ;; NEW VALUE-OF EXP Q2
    (let*-exp (vars exps body)
              (define (helper bind expr env1)
                (if (null? bind) env1
                    (let [(val (value-of (car expr) env1))]
                      (helper (cdr bind) (cdr expr) (extend-env (car bind) val env1)))))
              (helper vars exps env)
              (value-of body (helper vars exps env)))

    ))

;; INFERENCE RULES ;;

;; /'()∈(listofexpval)
;; L∈(listofexpval)/(consexpvalL)∈S

;; v1 = (expval2num (value-of e1 p) /\ v2 = (expval2num (value-of e2 p)
    ;;      ---------------------------------------------------------------
    ;; (value-of (cons-exp e1 e1) p) = (cons-val v1 v2)

    ;; car-exp
    ;; v = (value-of e p)
    ;; ------------------
    ;; (car-exp e) = (car v), if v = cons-val

    ;; cdr-exp
    ;; v = (value-of e p)
    ;; ------------------
    ;; (cdr-exp e) = (cdr v), if v = cons-val

    ;; null-exp
    ;; v = (value-of e p)
    ;; ------------------
    ;; (null?-exp e) = (bool-val #t), if v is an emptylist-val
    ;;                 (bool-val #f), if v is not an emptylist-val

    ;; list-exp
    ;; ((values = (map (lambda (e) (value-of e ρ)) (e1 ... en)))
    ;; ---------------------------------------------------------
    ;; (value-of (list-exp e1 ... en) ρ) = (list-val values)


;; (listof symbol) (listof (listof symbol)) (listof expression) environment --> environment
;; Purpose: Add the proc-vals for the given procedures in the given environment
(define (mk-letrec-env names params bodies env)
  (let* [(temp-proc-vals (map (lambda (p b)
                                (proc-val (procedure p b (vector (empty-env)))))
                              params
                              bodies))
         (new-env (foldl (lambda (name proc env)
                           (extend-env name
                                       proc
                                       env))
                         env
                         names
                         temp-proc-vals))]
    (begin
      (for-each (lambda (p)
                  (cases proc p
                    (procedure (p b ve)
                               (vector-set! ve 0 new-env))))
                (map (lambda (p) (expval2proc p))
                     temp-proc-vals))
      new-env)))
                           

;; apply-procedure : proc (listof expval) -> expval
;; Purpose: Apply the given procedure to the given values
(define (apply-procedure f vals)
  (cases proc f
    (procedure (params body envv)
               (let [(saved-env (vector-ref envv 0))]
                 (value-of body
                           (foldr (lambda (binding acc)
                                    (extend-env (car binding)
                                                (cadr binding)
                                                acc))
                                  saved-env
                                  (map (lambda (p v) (list p v))
                                       params
                                       vals)))))))

;;;;;;   EVALUATION WRAPPERS    ;;;;;;

;; string -> a-program
;; Purpose: Parse the given extended LC-program
(define (parse p) (scan&parse p))

;; string -> ExpVal
;; Purpose: Evaluate the given extended LC-program
(define (eval string)
  (value-of-program (parse string)))

;;;;; EXAMPLES OF EVALUATION

(check-equal? (eval "if zero?(1) then 1 else 2")
              (num-val 2))

(check-equal? (eval "-(15, 10)")
              (num-val 5))

(check-equal? (eval "let x = 10
                     in if zero?(-(x, x)) then x else 2")
              (num-val 10))

(check-equal? (eval "let decr = proc (a) -(a, 1) in (decr 30)")
              (num-val 29))

(check-equal? (eval "( proc (g) (g 30) proc (y) -(y, 1))")
              (num-val 29))

(check-equal? (eval "let x = 200
                     in let f = proc (z) -(z, x) 
                        in let x = 100 
                           in let g = proc (z) -(z, x) 
                              in -((f 1), (g 1))")
              (num-val -100))

(check-equal? (eval "let sum = proc (x) proc (y) -(x, -(0, y)) in ((sum 3) 4)")
              (num-val 7))

(check-equal? (eval "let sum = proc (x) proc (y) -(x, -(0, y))
                     in letrec sigma (n) = if zero?(n)
                                           then 0
                                           else ((sum n) (sigma -(n, 1)))
                        in (sigma 5)")
              (num-val 15))

(check-equal? (eval "letrec even(n) = if zero?(n)
                                      then zero?(n)
                                      else if zero?(-(n, 1))
                                           then zero?(n)
                                           else (even -(n, 2))
                     in (even 501)")
              (bool-val #f))

(check-equal? (eval "let x = 4
                     in cons(x, cons(cons(-(x,1), emptylist), emptylist))")
              (cons-val (num-val 4) (cons-val (cons-val (num-val 3) (emptylist-val '())) (emptylist-val '()))))

(check-equal? (eval "let x = 6
                     in cons(x, cons(cons(-(x,1), emptylist), emptylist))")
              (cons-val (num-val 6) (cons-val (cons-val (num-val 5) (emptylist-val '())) (emptylist-val '()))))

(check-equal? (eval "let x = 4
                     in list(x, -(x,1), -(x,3))")
              (list-val (list (num-val 4)(num-val 3)(num-val 1))))

(check-equal? (eval "let x = 10
                     in list(x, -(x,1), -(x,3))")
              (list-val (list (num-val 10)(num-val 9)(num-val 7))))

(check-equal? (eval "let x = 30
                     in let* x = -(x,1) y = -(x,2)
                        in -(x,y)")
              (num-val 2))

(check-equal? (eval "let* x = 1 y = -(x,1)
                     in list(x, y)")
              (list-val (list (num-val 1)(num-val 0))))

(check-equal? (eval "let x = 4 in cons(x, cons(cons(1, cons(2, cons(3, emptylist))), emptylist))")
              (cons-val (num-val 4) (cons-val (cons-val (num-val 1) (cons-val (num-val 2) (cons-val (num-val 3) (emptylist-val '())))) (emptylist-val '()))))

(check-equal? (eval "let* x = 5 in x")
              (num-val 5))

(check-equal? (eval "let* x = 5 y = 10 in -(x, y)")
              (num-val -5))

(check-equal? (eval "let* x = 1 y = 2 z = 3 in cons(x, cons(y, cons(z, emptylist)))")
              (cons-val (num-val 1) (cons-val (num-val 2) (cons-val (num-val 3) (emptylist-val '())))))


(check-equal? (eval "let* x = 5 y = -(x, 2) z = -(x, 3) in -(z, y)")
              (num-val -1))

(check-equal? (eval "let x = 4 in list(x)")
              (list-val (list (num-val 4))))

(check-equal? (eval "let x = 10 y = 5 in list(x,y)")
              (list-val (list (num-val 10) (num-val 5))))

(check-equal? (eval "let x = 5 in let* y = -(x, 2) z = -(x, 3) in cons(x, cons(y, cons(z, emptylist)))")
              (cons-val (num-val 5) (cons-val (num-val 3) (cons-val (num-val 2) (emptylist-val '())))))
 
(check-equal? (eval "emptylist")
              (emptylist-val '()))

(check-equal? (eval "let* x = 1 y = 2 in emptylist")
              (emptylist-val '()))

(check-equal? (eval "let* x = 5 y = 10 z = -(x, 3) in emptylist")
              (emptylist-val '()))


(check-equal? (eval "let x = 1 in let* y = 2 z = 3 in let a = cons(x, emptylist) b = emptylist in emptylist")
              (emptylist-val '()))