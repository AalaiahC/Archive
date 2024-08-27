#lang racket
(require rackunit)

#| RI Closures and Dynamic Scope |# 

;; On two occasions I have been asked [by members of
;; Parliament],--"Pray, Mr. Babbage, if you put into the machine wrong
;; figures, will the right answers come out?"

;; – Charles Babbage, 1864
 
#| Assignment Guidelines |#

;; To begin with, copy your correctly-implemented value-of-fn
;; interpreter and its three environment helpers (extend-env-fn,
;; apply-env-fn, empty-env-fn) over from last assignment. If you did
;; not get this correct last week, please see a TA and get help on
;; this before you go further.

;; In this file, rename that interpreter value-of, rename its
;; environment helpers extend-env, apply-env, and empty-env,
;; respectively.

;; Copy your correctly-implemented lex over from the second
;; assignment. If you do not yet have this correct please seek
;; guidance from a TA. This is a second chance at a whack at that
;; earlier problem.

;; The main goal of this assignment is to implement dynamic scope; we
;; are weighting that part of the assignment more heavily.

#| Regression Testing and Enhancing |# 


#| 

1. The following regression tests from your last assignment should
still work on your re-copied lex and your newly-renamed value-of and
its associated environment help functions.

|# 

  (check-equal?
   (apply-env (extend-env 'x 5 (empty-env)) 'x)
   5)
  (check-equal? 
   (apply-env (extend-env 'x 7 (extend-env 'x 5 (empty-env))) 'x)
   7)
  (check-equal?
   (apply-env (extend-env 'x 7 (extend-env 'y 5 (empty-env))) 'y)
   5)

  (check-equal?
   (value-of
    '((lambda (x) (if (zero? x)
                      #t
                      #f))
      0)
    (empty-env))
   #t)   
  (check-equal?
   (value-of 
    '((lambda (x) (if (zero? x) 
                      12 
                      47)) 
      0) 
    (empty-env))
   12)    
  (check-equal?
   (value-of
    '(let ([y (* 3 4)])
       ((lambda (x) (* x y)) (sub1 6)))
    (empty-env))
   60)
  (check-equal?
   (value-of
    '(let ([x (* 2 3)])
       (let ([y (sub1 x)])
         (* x y)))
    (empty-env))
   30)
  (check-equal?
   (value-of
    '(let ([x (* 2 3)])
       (let ([x (sub1 x)])
         (* x x)))
    (empty-env))
   25)
  (check-equal?
   (value-of
    '(((lambda (f)
         (lambda (n) (if (zero? n) 1 (* n ((f f) (sub1 n))))))
       (lambda (f)
         (lambda (n) (if (zero? n) 1 (* n ((f f) (sub1 n)))))))
      5)
    (empty-env))
   120)

  (check-equal?
   (lex '(lambda (x) x) '())
   '(lambda (var 0)))
  (check-equal?
   (lex '(lambda (y) (lambda (x) y)) '())
   '(lambda (lambda (var 1))))
  (check-equal?
   (lex '(lambda (y) (lambda (x) (x y))) '())
   '(lambda (lambda ((var 0) (var 1)))))
  (check-equal?
   (lex '(lambda (x) (lambda (x) (x x))) '())
   '(lambda (lambda ((var 0) (var 0)))))
  (check-equal?
   (lex '(lambda (y) ((lambda (x) (x y)) (lambda (c) (lambda (d) (y c))))) '()) 
   '(lambda ((lambda ((var 0) (var 1))) (lambda (lambda ((var 2) (var 1)))))))
  (check-equal?
   (lex '(lambda (a)
           (lambda (b)
             (lambda (c)
               (lambda (a)
                 (lambda (b)
                   (lambda (d)
                     (lambda (a)
                       (lambda (e)
                         (((((a b) c) d) e) a))))))))) '())
   '(lambda
      (lambda
        (lambda
          (lambda
            (lambda
              (lambda
                (lambda
                  (lambda
                    ((((((var 1) (var 3)) (var 5)) (var 2)) (var 0)) (var 1)))))))))))
  (check-equal?
   (lex '(lambda (a)
           (lambda (b)
             (lambda (c)
               (lambda (w)
                 (lambda (x)
                   (lambda (y)
                     ((lambda (a)
                        (lambda (b)
                          (lambda (c)
                            (((((a b) c) w) x) y))))
                      (lambda (w)
                        (lambda (x)
                          (lambda (y)
                            (((((a b) c) w) x) y))))))))))) '())
   '(lambda 
      (lambda 
        (lambda 
          (lambda 
            (lambda 
              (lambda 
                ((lambda
                   (lambda
                     (lambda
                       ((((((var 2) (var 1)) (var 0)) (var 5)) (var 4)) (var 3)))))
                 (lambda
                   (lambda
                     (lambda
                       ((((((var 8) (var 7)) (var 6)) (var 2)) (var 1)) (var 0)))))))))))))


#| 

2. You previously implemented lex to handle variables, application,
and lambda-abstraction forms. Extend your previous definition of
lex so that it can handle not only those forms, but also numbers,
zero?, sub1, *, if, and let. This should be a
straightforward extension (let should be the only line that requires
any real effort), but it also serves as a chance to improve a
misbehaving lex from an earlier assignment. In order to disambiguate
numbers from lexical addresses, you should transform a number n into
(const n).

|# 


#| Representation Independence wrt Closures |# 

#| 

3. Create a version of your interpreter from the first part of this
assignment that is representation-independent with respect to closures
and uses a higher-order function representation named value-of-fn.
Name your two new closure helpers apply-closure-fn and
make-closure-fn.

|# 


#| 

4. Create a version of this interpreter named value-of-ds that is
representation-independent with respect to closures and uses a
tagged-list data structure representation. Name your two new closure
helpers apply-closure-ds and make-closure-ds.

Other than the cosmetic change of -fn to -ds, you shouldn't have to
change the implementations of valof. This is once again a good thing!

This is evidence we have a tight, well-defined interface for
closures. We've been able to, on the "behind the
interface"/implementation side of things, radically change how we're
implementing closures, and we haven't had to change the client
code. That's evidence we've done a good job designing our interface.


|#




#| Dynamic Scope |# 

#| 

The second part of this week's assignment is to create an
interpreter that uses dynamic scope.

So far, we have implemented our interpreters so that, if there are
free variables references in a procedure, they take their values from
the environment in which the lambda expression is defined. We
accomplish this by creating a closure for each procedure we see, and
we save the environment in the closure. We call this technique static
binding of variables, or static scope. Lexical scope is a kind of
static scope.

Alternatively, we could implement our interpreters such that any free
variable references in the body of a procedure get their values from
the environment from which the procedure is called, rather than from
the environment in which the procedure is defined.

For example, consider what would happen if we were to evaluate the
following expression in an interpreter that used lexical scope:

(let ([x 2])
  (let ([f (lambda (e) x)])
    (let ([x 5])
      (f 0))))

Our lexical interpreter would add x to the environment with a
value of 2. For f, it would create a closure that contained the
binding of x to 2, and it would add f to the environment with
that closure as its value. Finally, the inner let would add x to
the environment with a value of 5. Then the call (f 0) would be
evaluated, but since it would use the value of x that was saved
in the closure (which was 2) rather than the value of x that was
current at the time f was called (which was 5), the entire
expression would evaluate to 2.

Under dynamic scope, we wouldn't save the value of x in the
closure for f. Instead, the application (f 0) would use the
value of x that was current in the environment at the time it was
called, so the entire expression would evaluate to 5.

|# 

#| 

4. Define value-of-dynamic, an interpreter that implements dynamic
scope. You should be able to share use your environment helpers from
the first part of this assignment, but you should not implement an
abstraction for closures in this interpreter. This will largely
correspond to the dynamically-scoped interpreter we wrote in class
that passes an additional parameter, "env^". In your implementation,
you should shadow the name of the existing env. You'll find then, that
when you go to evaluate an application, there's only one environment
in which you can evaluate the body. This is a pretty simple change. To
liven things up a little (and also to allow us a more interesting test
case), this interpreter should also implement let, if, *, sub1, null?,
zero?, cons, car, cdr, and quote. When evaluating the expression (cons
1 (cons 2 '())) value-of-dynamic should return (1 2). Now quote is a
bit of a tricky beast. So here's the quote line for the interpreter.

[(quote ,v) v]

|# 

