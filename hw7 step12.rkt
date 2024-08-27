#lang racket
(require rackunit)
(require rackunit-abbrevs)


#| Assignment 7: Continuations and Representation Independence |#

#| ==== Part I: lex ==== |#

#| 

0. If you do not already have it installed, download and install the
package rackunit-abbrevs from DrRacket

1. Just when you thought you'd seen the last of it---lex is back. If
   you've got the previous version working, then all we're asking for is
   a handful of changes. Don't CPS this function.

  - Start by copying and pasting in your most recent implementation.
  - Replace your application line with the following [`(,rator ,rand) `(app ,(lex rator acc) ,(lex rand acc))]
  - Replace your zero? line with the following [`(zero? ,nexp) `(zero ,(lex nexp acc))]
  - Replace your * line with the following [`(* ,nexp1 ,nexp2) `(mult ,(lex nexp1 acc) ,(lex nexp2 acc))]
  - Add lines to lex that implement let/cc and throw. Of these, let/cc is the more interesting one.

These first three changes aren't required to correctly perform lexical addressing, but will make your lex useful in part II. 

|#

;; any (listof any) -> number
;; Purpose: takes an element and a list containing that element and
;;          returns the (base 0) index of that element in the list
(define (list-index-ofv e le)
  (cond
    [(eqv? (car le) e) 0]
    [else (add1 (list-index-ofv e (cdr le)))]))

;; lex: expr (listof Var) -> expr
;; Purpose: takes a lambda-calculus expression and an accumulator and returns
;;          the same expression with all bound variable references replaced
;;          by lists of two elements whose car is the symbol var and whose
;;          cadr is the lexical address of the referenced variable.

(define (lex lamb acc)
  (match lamb
    [`,y #:when (symbol? y) `(var ,(list-index-ofv y acc))]
    [`,n #:when (number? n) `(const ,n)]
    [`(zero? ,nexp) `(zero ,(lex nexp acc))]
    [`(sub1 ,n) `(sub1 ,(lex n acc))]
    [`(* ,nexp1 ,nexp2) `(mult ,(lex nexp1 acc) ,(lex nexp2 acc))]
    [`(if ,t ,c ,a) `(if ,(lex t acc)
                         ,(lex c acc)
                         ,(lex a acc))]
    [`(let ([,x ,e]) ,b) `(let ,(lex e acc)
                            ,(lex b (cons x acc)))]
    [`(lambda (,x) ,b) `(lambda ,(lex b (cons x acc)))]
    [`(let/cc ,x ,e) `(let/cc ,(lex e (cons x acc)))]
    [`(throw ,kexp ,vexp) `(throw ,(lex vexp (cons kexp acc)))]
    [`(,rator ,rand) `(app ,(lex rator acc) ,(lex rand acc))]))

(check-equal?
 (lex '(lambda (x) x) '())
 '(lambda (var 0)))

(check-equal?
 (lex '(lambda (x) 1) '())
 '(lambda (const 1)))

(check-equal?
 (lex '(lambda (y) (if (zero? 6) y (* 3 (sub1 3)))) '())
 '(lambda (if (zero (const 6)) (var 0) (mult (const 3) (sub1 (const 3))))))

(check-equal?
 (lex '(lambda (x)
         (lambda (d)
           (let ([y (* x 1)])
             (* y 5)))) '())
 '(lambda (lambda
              (let (mult (var 1) (const 1)) (mult (var 0) (const 5))))))

(check-equal?
 (lex '(lambda (x)
         (lambda (g)
           (let ([y (* x 1)])
             (lambda (z)
               (* z (* x (sub1 y)))))))
      '())
 '(lambda (lambda
              (let (mult (var 1) (const 1))
                (lambda (mult (var 0) (mult (var 3) (sub1 (var 1)))))))))


#| ==== Part II: The interpreter === |#

;; This part of the assignment will be completed in several
;; stages. You should do each of these steps in a separate file. Turn
;; in a file containing the last step, and call it hw7.rkt. But it is
;; **imperative** that you keep each of the previous steps. We will
;; ask to see them at some point in the future. Again, it is
;; **imperative** that you keep previous steps. You should have at
;; least these concrete milestones.

;; You should begin with the interpreter below. This is an interpreter
;; for a lexed language, like the interpreter of problem 5 on
;; assignment 3.

(define value-of
  (lambda (expr env)
    (match expr
      [`(const ,cexpr) cexpr]
      [`(mult ,nexp1 ,nexp2) (* (value-of nexp1 env) (value-of nexp2 env))]
      [`(sub1 ,nexp) (sub1 (value-of nexp env))]
      [`(zero ,nexp) (zero? (value-of nexp env))]
      [`(if ,test ,conseq ,alt) (if (value-of test env)
                                    (value-of conseq env)
                                    (value-of alt env))]
      [`(letcc ,body) (let/cc k
                        (value-of body (lambda (y) (if (zero? y) k (env (sub1 y))))))]
      [`(throw ,k-exp ,v-exp) ((value-of k-exp env) (value-of v-exp env))]
      [`(let ,e ,body) (let ((a (value-of e env)))
                         (value-of body (lambda (y) (if (zero? y) a (env (sub1 y))))))]
      [`(var ,y) (env y)]
      [`(lambda ,body) (lambda (a) (value-of body (lambda (y) (if (zero? y) a (env (sub1 y))))))]
      [`(app ,rator ,rand) ((value-of rator env) (value-of rand env))])))



#| 

You should take value-of as well as the definitions of
empty-env and empty-k, which will prove useful as you begin
your transformations. When you write your own test programs, you will
probably prefer not to write them directly in the language of this
interpreter. You can write test programs in Racket, use your lex
to transform them to the language of our interpreter, and the results
in value-of-cps. Use empty-env and empty-k when testing.

===Steps for Part II===

  1 CPS this interpreter. Call it value-of-cps. Use the "let trick" to
    eliminate the let binding when you CPS the let line. You might
    consider always using k^ as the additional continuation variable to
    your extended environments. Do not apply k^ to the call to error in
    empty-env. This is similar to the behavior of times-cps-shortcut from
    Assignment 6. Racket's let/cc **may not** be used in your CPSed
    interpreter. You might consider comment out some of your match
    clauses, and CPSing the interpreter a few lines at a time. But try to
    finish this step entirely before you move on to the next one. Since
    your closures and environments are both implemented as functions, you
    should consider renaming them to env-cps and c-cps, respectively.
|#
#|
  2 Define apply-env, apply-closure, and apply-k, and add all the
    calls to those functions. Notice that after CPSing, your apply-closure
    and apply-env now take three arguments. 
|#

#|
  3 Define extend-env and replace all 3 of your explicitly
    higher-order representations of environments with calls to extend-env.
|#

#|
  4 Add a ^ to each of the formal parameters of extend-env (not the
    inner function, mind you). Ensure that the inner functions in both
    extend-env and empty-env use the same formal parameters as the second
    argument to apply-env (here, typically y and k^).
|#

#|
  5 Replace your higher-order function representation of environments
    with a tagged-list data structure representation. Consider first
    ensuring that the last two formal parameters to apply-env (y and k)
    are the same as the formal parameters to the inner functions in
    extend-env and empty-env. Remember, if you add (else (env-cps y k)) as
    the last line of your match expression, you can test each
    transformation one at a time.
|#

#|
  6 If you used it, remove the (else (env-cps y k)) as the last line
    of your match expression in apply-env.
|#

#|
  7 Define make-closure and replace your explicitly higher-order
    representation of a closure with a call to make-closure.
|#

#|
  8 Replace your higher-order function representation of closures with
    a tagged-list data structure representation. Consider first ensuring
    that the last two formal parameters to apply-closure (a and k) are the
    same as the formal parameters to the inner function in make-closure.
|#

#|
  9 Define constructors for your continuations, and replace your
    explicitly higher-order function continuations with calls to these
    constructors. For nested continuations, you should consider working
    from the inside out. Remember, if you transform your constructors one
    at a time, you can test your program after each replacement.
|#

#|
 10 Add a ^ to each of the formal parameters of your continuation
    helpers, and change the body to match (that is, do an alpha
    substitution). Then, ensure that the inner function in each of your
    continuation helpers uses the same formal parameter as the second
    argument to apply-k (typically, v).
|#

#|
 11 Replace your higher-order function representations of
    continuations with a tagged-list data structure
    representation. Remember, if you add (else (k v)) as the last line of
    your match, you can test each transformation one at a time. If you're
    good, you can do almost all of this step with a keyboard macro.
|#

#|
 12 Remove the (else (k v)) line to ensure that you've properly
    removed all higher-order function representations of continuations.
|#

;; ENVIRONMENT

(define (empty-env)
  `(empty-env))

(define (extend-env a^ env-cps^)
  `(extend-env ,a^ ,env-cps^))

(define (apply-env env-cps^ y k^)
  (match env-cps^
    [`(extend-env ,a^ ,env-cps^)(if (zero? y)
                                    (apply-k k^ a^)
                                    (apply-env env-cps^ (sub1 y) k^))]
    [`(empty-env) (lambda ()
                    (lambda (y)
                      (error 'value-of "unbound identifier")))]))

;; CLOSURE

(define (make-closure body env-cps k)
  `(make-closure ,body ,env-cps ,k))

(define (apply-closure c-cps a k)
  (match c-cps
    [`(make-closure ,body ,env-cps ,k^) (value-of-cps body (extend-env a env-cps) k)]))

;; CONTINUATION

(define (empty-k)
  `(empty-k))

(define (apply-k k v)
  (match k
    [`(empty-k) v]
    [`(mult-outer-k ,v^ ,k^) (apply-k k^ (* v^ v))]
    [`(mult-inner-k ,nexp2^ ,env-cps^ ,k^) (value-of-cps nexp2^ env-cps^ (mult-outer-k v k^))]
    [`(sub1-k ,k^) (apply-k k^ (sub1 v))]
    [`(zero?-k ,k^) (apply-k k^ (zero? v))]
    [`(if-k ,conseq^ ,alt^ ,env-cps^ ,k^) (if v
                                              (value-of-cps conseq^ env-cps^ k^)
                                              (value-of-cps alt^ env-cps^ k^))]
    [`(throw-k ,v-exp^ ,env-cps^) (value-of-cps v-exp^ env-cps^ v)]
    [`(let-k ,body^ ,env-cps^ ,k^) (value-of-cps body^ (extend-env v env-cps^) k^)]
    [`(app-outer-k ,a^ ,k^) (apply-closure v a^ k^)]
    [`(app-inner-k ,rator^ ,env-cps^ ,k^) (value-of-cps rator^ env-cps^ (app-outer-k v k^))]))

;; CONSTRUCTORS

(define (mult-outer-k v^ k^)
  `(mult-outer-k ,v^ ,k^))

(define (mult-inner-k nexp2^ env-cps^ k^)
  `(mult-inner-k ,nexp2^ ,env-cps^ ,k^))

(define (sub1-k k^)
  `(sub1-k ,k^))

(define (zero?-k k^)
  `(zero?-k ,k^))

(define (if-k conseq^ alt^ env-cps^ k^)
  `(if-k ,conseq^ ,alt^ ,env-cps^ ,k^))

(define (throw-k v-exp^ env-cps^)
  `(throw-k ,v-exp^ ,env-cps^))

(define (let-k body^ env-cps^ k^)
  `(let-k ,body^ ,env-cps^ ,k^))

(define (app-outer-k a^ k^)
  `(app-outer-k ,a^ ,k^))

(define (app-inner-k rator^ env-cps^ k^)
  `(app-inner-k ,rator^ ,env-cps^ ,k^))

;; INTERPRETER
  
(define value-of-cps
  (lambda (expr env-cps k)
    (match expr
      [`(const ,cexpr) (apply-k k cexpr)]
      [`(mult ,nexp1 ,nexp2) (value-of-cps nexp1 env-cps (mult-inner-k nexp2 env-cps k))]
      [`(sub1 ,nexp) (value-of-cps nexp env-cps (sub1-k k))]
      [`(zero ,nexp) (value-of-cps nexp env-cps (zero?-k k))]
      [`(if ,test ,conseq ,alt) (value-of-cps test env-cps (if-k conseq alt env-cps k))]
      [`(letcc ,body) (value-of-cps body (extend-env k env-cps) k)]
      [`(throw ,k-exp ,v-exp) (value-of-cps k-exp env-cps (throw-k v-exp env-cps))]
      [`(let ,e ,body) (value-of-cps e env-cps (let-k body env-cps k))]
      [`(var ,y) (apply-env env-cps y k)]
      [`(lambda ,body) (apply-k k (make-closure body env-cps k))]
      [`(app ,rator ,rand) (value-of-cps rand env-cps (app-inner-k rator env-cps k))])))


(check-true* equal?
             ((value-of-cps '(const 5) (empty-env) (empty-k)) 5)
             ((value-of-cps '(mult (const 5) (const 5)) (empty-env) (empty-k)) 25)
             ((value-of-cps '(zero (const 5)) (empty-env) (empty-k)) #f)
             ((value-of-cps '(zero (mult (const 5) (const 5))) (empty-env) (empty-k)) #f)
             ((value-of-cps '(sub1 (const 5)) (empty-env) (empty-k)) 4)
             ((value-of-cps '(sub1 (sub1 (const 5))) (empty-env) (empty-k)) 3)
             ((value-of-cps '(sub1 (sub1 (const 5))) (extend-env 3 (empty-env)) (empty-k)) 3)
             ((value-of-cps '(zero (sub1 (const 6))) (empty-env) (empty-k)) #f)
             ((value-of-cps '(mult (sub1 (const 5)) (const 5)) (empty-env) (empty-k)) 20)
             ((value-of-cps '(mult (const 5) (sub1 (const 5))) (empty-env) (empty-k)) 20)
             ((value-of-cps '(if (zero (const 5)) (const 3) (const 4)) (empty-env) (empty-k)) 4)
             ((value-of-cps '(if (zero (const 0)) (const 4) (const 3)) (empty-env) (empty-k)) 4)
             ((value-of-cps '(mult (if (zero (const 0)) (const 4) (const 3)) (const 2)) (empty-env) (empty-k)) 8)
             ((value-of-cps '(mult (const 2) (if (zero (const 0)) (const 4) (const 3))) (empty-env) (empty-k)) 8)
             ((value-of-cps '(let (const 6) (const 4)) (empty-env) (empty-k)) 4)
             ((value-of-cps '(let (const 5) (var 0)) (empty-env) (empty-k)) 5)
             ((value-of-cps '(let (mult (const 5) (const 3)) (var 0)) (empty-env) (empty-k)) 15)
             ((value-of-cps '(let (const 5) (mult (var 0) (var 0))) (empty-env) (empty-k)) 25)
             ((value-of-cps '(let (const 5) (let (const 6) (var 1))) (empty-env) (empty-k)) 5)
             ((value-of-cps '(let (const 5) (let (const 6) (mult (var 0) (var 1)))) (empty-env) (empty-k)) 30)
             ((value-of-cps '(let (const 5) (let (const 6) (mult (var 1) (var 0)))) (empty-env) (empty-k)) 30)
             ((value-of-cps '(mult (const 5) (let (const 5) (var 0))) (empty-env) (empty-k)) 25)
             ((value-of-cps '(app (lambda (const 5)) (const 6)) (empty-env) (empty-k)) 5) 
             ((value-of-cps '(app (lambda (var 0)) (const 5)) (empty-env) (empty-k)) 5)
             ((value-of-cps '(app (app (lambda (lambda (var 1))) (const 6)) (const 5)) (empty-env) (empty-k)) 6)
             ((value-of-cps '(app (lambda (app (lambda (var 1)) (const 6))) (const 5)) (empty-env) (empty-k)) 5)
             ((value-of-cps '(app (lambda (if (zero (var 0)) (const 4) (const 5))) (const 3)) (empty-env) (empty-k)) 5)
             ((value-of-cps '(app (if (zero (const 4)) (lambda (var 0)) (lambda (const 5))) (const 3)) (empty-env) (empty-k)) 5)
             ((value-of-cps '(app (if (zero (const 0)) (lambda (var 0)) (lambda (const 5))) (const 3)) (empty-env) (empty-k)) 3)
             ((value-of-cps '(letcc (const 5)) (empty-env) (empty-k)) 5)
             ((value-of-cps '(letcc (throw (var 0) (const 5))) (empty-env) (empty-k)) 5)
             ((value-of-cps '(letcc (throw (var 0) (mult (const 5) (const 5)))) (empty-env) (empty-k)) 25)
             ((value-of-cps '(letcc (throw (app (lambda (var 0)) (var 0)) (mult (const 5) (const 5)))) (empty-env) (empty-k)) 25)
             ((value-of-cps '(letcc (sub1 (throw (var 0) (const 5)))) (empty-env) (empty-k)) 5)
             ((value-of-cps '(letcc (throw (throw (var 0) (const 5)) (const 6))) (empty-env) (empty-k)) 5)
             ((value-of-cps '(letcc (throw (const 5) (throw (var 0) (const 5)))) (empty-env) (empty-k)) 5)
             ((value-of-cps '(mult (const 3) (letcc (throw (const 5) (throw (var 0) (const 5))))) (empty-env) (empty-k)) 15)
             ((value-of-cps '(if (zero (const 5)) (app (lambda (app (var 0) (var 0))) (lambda (app (var 0) (var 0)))) (const 4))
                            (empty-env)
                            (empty-k))
              4)
             ((value-of-cps '(if (zero (const 0)) (const 4) (app (lambda (app (var 0) (var 0))) (lambda (app (var 0) (var 0)))))
                            (empty-env)
                            (empty-k))
              4)
             ((value-of-cps '(app (lambda (app (app (var 0) (var 0)) (const 2)))
                                  (lambda
                                      (lambda 
                                          (if (zero (var 0))  
                                              (const 1)
                                              (app (app (var 1) (var 1)) (sub1 (var 0)))))))
                            (empty-env)
                            (empty-k))
              1))
    

#| Part III |#

#| 

This week, you'll also get to learn about streams, a data-structure
that enables us to process infinite lists of items. Its a lazily-evaluated, memoized
(also termed //delayed//) list.

To play around, we'll first need to implement a few tools.

|# 

(define-syntax cons$
  (syntax-rules ()
    ((cons$ x y) (cons x (delay y)))))

(define car$ car)

(define cdr$
  (lambda ($) (force (cdr $))))

#| 

We'll get back to those helpers. For now, it's enough that they're
tweaked cons, car, and cdr.  The first question to ask is: how do you
build an infinite list? The only reasonable answer is: one item at a
time, as needed.  Here, we're going to define an infinite stream of
ones.

|# 

(define inf-1s (cons$ 1 inf-1s))

#| 

It looks like that can't possibly work. We're definining the stream in
terms of itself. That's a circular definition.  But, in fact, that's
precisely what we're after. We're defining a list that has a 1 in
front and whose cdr - well, whatever that thing is, it has a 1 in the
front of it. And thus it's 1s all the way down.

So we can build a procedure take$

|# 

(define take$
  (lambda (n $)
    (cond
      ((zero? n) '())
      (else (cons (car$ $) 
                  (let ((n- (sub1 n)))
                    (cond
                      ((zero? n-) '())
                      (else (take$ n- (cdr$ $))))))))))

#| 

... that pulls n items from the stream, and returns them in a
list (the $ stands for $tream, by the way).

|# 

;; > (take$ 5 inf-1s)
;; (1 1 1 1 1)
;; > (take$ 10 inf-1s)
;; (1 1 1 1 1 1 1 1 1 1)

#| 

So how is this all working? There's nothing to car$. That's just
car with fancy window-dressing. cons$ is the first macro
definition we've looked at in class. But there's nothing much to it,
either. You can think of it as performing a textual transformation --
every time we see something of the form (cons$ <thing1>
<thing2>), that code is actually transformed as (cons
<thing1> (delay <thing2>)) Importantly, <thing2> isn't
evaluated in the process. But do take note of the delay form, and
the force form in cdr$. As we've already seen, there are times
in which we might like to evaluate an expression only once, and
thereafter just return that already computed value. And we might like
to hold off on doing that evaluation, instead of doing it right
away. delay does both of those -- it creates a //promise//, of
which force can then force the evaluation. From there on out,
every time we force that promise, we get the same value.

|#

;; > (define worlds-worst-random
;;     (delay (random 4)))
;; > (force worlds-worst-random)
;; 2
;; > (force worlds-worst-random)
;; 2
;; > (force worlds-worst-random)
;; 2

#| 

So, to put it all together, we define inf-1s to be a cons whose
car is 1 and whose cdr is a promise. When we finally get
around to evaluating that promise, we find that its value is in fact
inf-1s -- that is, a cons whose car is 1 and whose cdr is
a promise.

Hopefully that all makes sense.

Your task this week is to implement the tribonacci stream. Its the
sequence from the exam: 0 is the first tribonacci number, 1 is the
second, 1 is the third, and the values thereafter are each the sum of
the three previous values in the sequence. Call it trib$.

 (check-true* equal? 
   ((car$ trib$) 0)
   ((car$ (cdr$ trib$)) 1)
   ((take$ 7 trib$) '(0 1 1 2 4 7 13)))

|# 


