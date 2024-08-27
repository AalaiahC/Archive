#lang eopl
(require rackunit "eopl-extras.rkt")

(define the-lexical-spec
  '((whitespace (whitespace) skip)
    
    (comment ("%" (arbno (not #\newline))) skip)
    
    (identifier
     (letter (arbno (or letter digit "_" "-" "?"))) symbol)
    
    (number (digit (arbno digit)) number)
    
    (number ("-" digit (arbno digit)) number)
    
    (oparen ("(") symbol)
    (cparen (")") symbol)
    (comma (",") symbol)
    
    (bool-true ("true") symbol)
    (bool-false ("false") symbol)
    
    (op-list ("[" (arbno expression) "]") list)
    ))

(define the-grammar
  '((program (expression) a-program)
    
    (expression (number) const-exp)

    (expression (bool-true) true-exp)

    (expression (bool-false) false-exp)

    (expression (identifier) var-exp)
    
    (expression("-" "(" expression "," expression ")") diff-exp)
    
    (expression ("zero?" "(" expression ")") zero?-exp)
    
    (expression ("if" expression "then" expression "else" expression) if-exp)
    
    (expression 
     ("let" (arbno identifier "=" expression) "in" expression) let-exp)
    
    (expression
     ("letrec" (arbno identifier "(" (arbno identifier) ")" "=" expression)
               "in" expression) letrec-exp)
    
    (expression ("proc" "(" (arbno identifier) ")" expression) proc-exp)
    
    (expression ("(" expression (arbno expression) ")") call-exp)
    
    (expression (oparen expression "," expression cparen) cons-exp)
    
    (expression ("car" "(" expression ")") car-exp)
    
    (expression ("cdr" "(" expression ")") cdr-exp)
    
    (expression ("null?" "(" expression ")") null?-exp)
    
    (expression (op-list) list-exp)
    
    (expression ("emptylist") emptylist-exp)
    ))

(define (expval2list v)
  (cases expval v
    (list-val (lst) lst)
    (else (expval-extractor-error 'list-val v))))

(define (list-to-expval lst)
  (list-val lst))

(define (is-emptylist? v)
  (cases expval v
    (emptylist-val () #t)
    (else #f)))

(define-datatype expval expval?
  (num-val
   (value number?))
  (bool-val
   (boolean boolean?))
  (proc-val 
   (proc proc?))
  (list-val
   (lst (list-of expval?)))
  (emptylist-val
   (emptylist ())))

(define (eval-cons exp env)
  (let ((car-val (value-of (car-exp exp) env))
        (cdr-val (value-of (cdr-exp exp) env)))
    (list-to-expval (cons car-val (expval2list cdr-val)))))

(define (eval-car exp env)
  (let ((lst (value-of (car-exp exp) env)))
    (if (is-emptylist? lst)
        (eopl:error 'eval-car "Empty list cannot be used with car")
        (car (expval2list lst)))))

(define (eval-cdr exp env)
  (let ((lst (value-of (car-exp exp) env)))
    (if (is-emptylist? lst)
        (eopl:error 'eval-cdr "Empty list cannot be used with cdr")
        (cdr (expval2list lst)))))

(define (eval-null? exp env)
  (let ((lst (value-of (car-exp exp) env)))
    (if (is-emptylist? lst)
        (bool-val #t)
        (bool-val #f))))

(define (eval-list exp env)
  (let ((vals (value-of (list-exp exp) env)))
    (list-to-expval vals)))

(define (eval-emptylist exp env)
  (emptylist-val))

;; symbol expval --> throws error
(define (expval-extractor-error variant value)
  (eopl:error 'expval-extractors "Looking for a ~s, given ~s"
              variant value))


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

    ;; New cases for list operations
    (cons-exp (car cdr) (eval-cons exp env))
    (car-exp (exp) (eval-car exp env))
    (cdr-exp (exp) (eval-cdr exp env))
    (null?-exp (exp) (eval-null? exp env))
    (list-exp () (eval-list exp env))
    (emptylist-exp () (eval-emptylist exp env))))
