#lang racket
(require rackunit)

#| Environments and Interpreters |# 

#|

1. value-of 

|#

;; value-of: expr env -> val
;; Purpose: an interpreter that takes a Racket expression and returns the
;;          expression's value
(define (value-of expr env)
  (match expr
    [`,n #:when (number? n) n]
    [`,b #:when (boolean? b) b]
    [`,y #:when (symbol? y) (env y)]
    [`(zero? ,n) (zero? (value-of n env))]
    [`(sub1 ,n) (sub1 (value-of n env))]
    [`(* ,n1 ,n2) (* (value-of n1 env)
                     (value-of n2 env))]
    [`(if ,t ,c ,a) (if (value-of t env)
                        (value-of c env)
                        (value-of a env))]
    [`(let ([,x ,e]) ,b) (let ([a (value-of e env)])
                           (value-of b (λ (y)(if (eqv? y x) a (env y)))))]
    [`(lambda (,x) ,b) (λ (a) (value-of b
                                        (λ (y) (if (eqv? y x) a (env y)))))]
    [`(,rator ,rand) ((value-of rator env) (value-of rand env))]))

(test-equal?
 "A nearly-sufficient test-case of your program's functionality" 
 (value-of
  '(((lambda (f)
       (lambda (n) (if (zero? n) 1 (* n ((f f) (sub1 n))))))
     (lambda (f)
       (lambda (n) (if (zero? n) 1 (* n ((f f) (sub1 n)))))))
    5)
  (lambda (y) (error 'value-of "unbound variable ~a" y)))
 120)

(test-equal?
 "Test case with let"
 (value-of
  '(let ([x 2])
     (* x 3))
  (lambda (y) (error 'value-of "unbound variable ~a" y)))
 6)


#| 

2. value-of-fn. 

|#

;; empty-env-fn: () -> env
;; Purpose: creates an empty environment
(define (empty-env-fn)
  (λ (y) (error "Nuh uh uh" y)))

;; extend-env-fn: var val env -> env
;; Purpose: extends the given environment with the given variable and value
(define (extend-env-fn x a env)
  (λ (y) (if (eqv? y x)
             a
             (apply-env-fn env y))))

;; apply-env-fn: env var -> val
;; Purpose: applies the given environment on the given variable
(define (apply-env-fn env y)
  (env y))

;; value-of-fn: expr env -> val
;; Purpose: an interpreter that takes a Racket expression and returns the
;;          expression's value using helper functions
(define (value-of-fn expr env)
  (match expr
    [`,n #:when (number? n) n]
    [`,b #:when (boolean? b) b]
    [`,y #:when (symbol? y) (apply-env-fn env y)]
    [`(zero? ,n) (zero? (value-of-fn n env))]
    [`(sub1 ,n) (sub1 (value-of-fn n env))]
    [`(* ,n1 ,n2) (* (value-of-fn n1 env)
                     (value-of-fn n2 env))]
    [`(if ,t ,c ,a) (if (value-of-fn t env)
                        (value-of-fn c env)
                        (value-of-fn a env))]
    [`(let ([,x ,e]) ,b) (let ([a (value-of-fn e env)])
                           (value-of-fn b (extend-env-fn x a env)))]
    [`(lambda (,x) ,b) (λ (a) (value-of-fn b (extend-env-fn x a env)))]
    [`(,rator ,rand) ((value-of-fn rator env) (value-of-fn rand env))]))

(test-equal?
 "A nearly-sufficient test-case of your program's functionality" 
 (value-of-fn
  '(((lambda (f)
       (lambda (n) (if (zero? n) 1 (* n ((f f) (sub1 n))))))
     (lambda (f)
       (lambda (n) (if (zero? n) 1 (* n ((f f) (sub1 n)))))))
    5)
  (lambda (y) (error 'value-of-fn "unbound variable ~a" y)))
 120)

(test-equal?
 "Test case with let"
 (value-of-fn
  '(let ([x 2])
     (* x 3))
  (lambda (y) (error 'value-of "unbound variable ~a" y)))
 6)


#| 

3. value-of-ds

We walked through the steps of implementing this in lecture, at least
for the basic forms. We can also see this step from a software
engineering point-of-view as follows. In the above, we shimmed in an
interface to separate our client code that uses environment and our
implementation code that provides environment, across an interface
that is the three functions `apply-env-fn`, `extend-env-fn`, and
`empty-env-fn`. In this step, we will now demonstrate to ourselves
that this was a well-defined interface, that is, that our interface is
not "leaky." We will re-implement environment, using an entirely
different representation behind-the-scenes. Since our client (the
interpreter) is programming against the interface, though, the client
code won't have to change at all*.

With this switch to a representation of environments as data
structures, we notice another neat thing. We changed our environment
went from a higher-order, functional representation in the last part
of the assignment to now, a first-order data structure
representation. And we can match against our environment like you
would any old data definition. 


* Okay, so we're in point of fact changing both the interpreter client
code and the "helper" interface functions from -fn to -ds, but this is
just so that we can have the different versions of this interpreter in
the same file.

|#

;; empty-env-ds: () -> env
;; Purpose: creates an empty environment
(define (empty-env-ds)
  `(empty-env))

;; extend-env-ds: var val env -> env
;; Purpose: extends the given environment with the given variable and value
(define (extend-env-ds x a env)
  `(extend-env ,x ,a ,env))

;; apply-env-ds: env var -> val
;; Purpose: applies the given environment on the given variable
(define (apply-env-ds env y)
  (match env
    [`(empty-env) (error "Nuh uh uh" y)]
    [`(extend-env ,x ,a ,env)(if (eqv? y x)
                                 a
                                 (apply-env-ds env y))]
    [else (env y)]))

;; value-of: expr env -> val
;; Purpose: an interpreter that takes a Racket expression and returns the
;;          expression's value with data structure representation
(define (value-of-ds expr env)
  (match expr
    [`,n #:when (number? n) n]
    [`,b #:when (boolean? b) b]
    [`,y #:when (symbol? y) (apply-env-ds env y)]
    [`(zero? ,n) (zero? (value-of-ds n env))]
    [`(sub1 ,n) (sub1 (value-of-ds n env))]
    [`(* ,n1 ,n2) (* (value-of-ds n1 env)
                     (value-of-ds n2 env))]
    [`(if ,t ,c ,a) (if (value-of-ds t env)
                        (value-of-ds c env)
                        (value-of-ds a env))]
    [`(let ([,x ,e]) ,b) (let ([a (value-of-ds e env)])
                           (value-of-ds b (extend-env-ds x a env)))]
    [`(lambda (,x) ,b) (λ (a) (value-of-ds b (extend-env-ds x a env)))]
    [`(,rator ,rand) ((value-of-ds rator env) (value-of-ds rand env))]))

(test-equal?
 "A nearly-sufficient test-case of your program's functionality" 
 (value-of-ds
  '(((lambda (f)
       (lambda (n) (if (zero? n) 1 (* n ((f f) (sub1 n))))))
     (lambda (f)
       (lambda (n) (if (zero? n) 1 (* n ((f f) (sub1 n)))))))
    5)
  (lambda (y) (error 'value-of-ds "unbound variable ~a" y)))
 120)

(test-equal?
 "Test case with let"
 (value-of-ds
  '(let ([x 2])
     (* x 3))
  (lambda (y) (error 'value-of "unbound variable ~a" y)))
 6)


#| A New Syntax |#

#| 

4. Implement an interpreter fo-eulav. Let the below examples guide
you. I only require you to implement those forms I use in those
examples.

|# 

;; fo-eulav: expr env -> val
;; Purpose: an interpreter that takes a backwards Racket expression and returns the
;;          expression's value
(define (fo-eulav expr env)
  (match expr
    [`,n #:when (number? n) n]
    [`,y #:when (symbol? y) (env y)]
    [`(,n ?orez) (zero? (fo-eulav n env))]
    [`(,n 1bus) (sub1 (fo-eulav n env))]
    [`(,n1 ,n2 *) (* (fo-eulav n1 env)
                     (fo-eulav n2 env))]
    [`(,a ,c ,t fi) (if (fo-eulav t env)
                        (fo-eulav c env)
                        (fo-eulav a env))]
    [`(,b  (,x) adbmal) (λ (a) (fo-eulav b
                                        (λ (y) (if (eqv? y x) a (env y)))))]
    [`(,rand ,rator) ((fo-eulav rator env) (fo-eulav rand env))]))

(test-equal?
 "Ppa"
 (fo-eulav '(5 (x (x) adbmal)) (lambda (y) (error 'fo-eulav "unbound variable ~s" y)))
 5)

(test-equal?
 "Stnemugra sa Snoitcnuf"
 (fo-eulav '(((x 1bus) (x) adbmal) ((5 f) (f) adbmal)) (lambda (y) (error 'fo-eulav "unbound variable ~s" y)))
 4)

(test-equal?
 "Tcaf"
 (fo-eulav
  '(5  
    (((((((n 1bus) (f f)) n *) 1 (n ?orez) fi)
       (n) adbmal)
      (f) adbmal)
     ((((((n 1bus) (f f)) n *) 1 (n ?orez) fi)
       (n) adbmal)
      (f) adbmal))) 
  (lambda (y) (error 'fo-eulav "unbound variable ~s" y)))
 120)


;; Consider the following interpreter for a deBruijnized version of
;; the lambda-calculus (i.e. lambda-calculus expressions using lexical
;; addresses addresses instead of variables). Notice this interpreter
;; is representation-independent with respect to environments. There
;; are a few other slight variations in the syntax of the
;; language. These are of no particular consequence.


#| 

5. Without using lambda or the implicit lambda in an "MIT-define",
define apply-env-lex and extend-env-lex. A correct solution is very
short.  

|#

;; extend-env-lex: var val env -> env
;; Purpose: extends the given environment with the given variable and value
(define extend-env-lex cons)

;; apply-env-lex: env var -> val
;; Purpose: applies the given environment on the given variable to return th
(define (apply-env-lex env y)
  (list-ref env y))


;; value-of-lex: expr env -> val
;; Purpose: an interpreter that takes a Racket expression and returns the
;;          expression's value
(define (value-of-lex exp env)
  (match exp
    [`(const ,expr) expr]
    [`(mult ,x1 ,x2) (* (value-of-lex x1 env) (value-of-lex x2 env))]
    [`(zero ,x) (zero? (value-of-lex x env))]
    [`(sub1 ,body) (sub1 (value-of-lex body env))]
    [`(if ,t ,c ,a) (if (value-of-lex t env) (value-of-lex c env) (value-of-lex a env))]
    [`(var ,num) (apply-env-lex env num)]
    [`(lambda ,body) (lambda (a) (value-of-lex body (extend-env-lex a env)))]
    [`(,rator ,rand) ((value-of-lex rator env) (value-of-lex rand env))]))
 
(define (empty-env-lex)
  '())

(test-equal?
 "This test shows we're using a data-structure representation of environments."
 (value-of-lex '((lambda (var 0)) (const 5)) (empty-env-lex))
 5)

