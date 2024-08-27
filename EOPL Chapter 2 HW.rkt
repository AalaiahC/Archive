#lang eopl

#|
Data Structure Representation

<env> ::= (empty-env)
      ::= (extend-end <symbol> <Racket-value> <env>)
|#

;; -> env
;; Purpose: Construct the empty env
(define (empty-env)
  (list 'empty-env))

;; symbol X env -> env
;; Purpose: Add a binding to the given env
(define (extend-env var val e)
  (list 'extend-env var val e))

;;;;;; 2.7 ;;;;;;
;; Rewrite apply-env in figure 2.1 to give a more informative error message

;;apply-env: env var -> schemeval
(define (apply-env env search-var)
  (cond
    [(eqv? (car env) 'empty-env?)
     (report-no-binding-found search-var)]
    [(eqv? (car env) 'extend-env)
     (let ((saved-var (cadr env))
           (saved-val (caddr env))
           (saved-env (cadddr env)))
       (if (eqv? search-var saved-var)
           saved-val
           (apply-env saved-env search-var)))]
    [else (report-invalid-env env)]))

(define (report-no-binding-found search-var)
  (eopl:error 'apply-env "No binding for variable in environment" search-var))

(define (report-invalid-env env)
  (eopl:error 'apply-env "environment not valid for searching var" env))

;;;;;; 2.8 ;;;;;;;;
;; Add to the environment interface an observer called empty-env? and implement
;; it using the a-list representation

;;empty-env?: env -> boolean
(define (empty-env? env)
  (eq? (car env) (car(empty-env))))
      
(define e (empty-env))
(define f (extend-env 'x 3 e))

;;;;;; 2.9 ;;;;;;
;; Add to the environment interface an observer called has-binding? that takes an
;; an environment env and a variable s and tests to see if s has an associated value
;; in env. Implement using the a-list representation

;;has-binding?: env -> boolean
(define (has-binding? env s)
  (cond [(eq? (car env) 'empty-env) #f]
        [(eq? (cadr env) s) #t]
        [else (apply-env (cadddr env) s)]))

;;;;;; 2.12 ;;;;;;
;; Implement the stack data type of exercise 2.4 using a procedural representation

#|
INTERFACE FOR STACK
empty-stack: creates an empty stack
push: pushes an item into the stack 
pop: pops the top item out of the stack using LIFO
top: displays the top item in a stack
empty-stack?: determins if a stack is empty

(empty-stack)   c     = [0]
(push [s] val)  c     = s(val)
(pop [s])       c     = [t]
(top [s])       o     = s(val1)
(empty-stack? [s]) o  = #t/#f
                      where [t] = s - (val1)
|#

(define (empty-stack)
  (lambda (stack) (eq? stack 'empty?) #t))
  
(define (push stack val)
  (lambda (s)
    (cond [(eq? s 'empty?) #f]
          [(eq? s 'pop) stack]
          [(eq? s 'top) val])))

(define (pop stack)
  (stack 'pop))

(define (top stack)
  (stack 'top))

(define (empty-stack? stack)
  (stack 'empty?))

(define s (empty-stack))
(define y (push s 42))
(define g (pop y))

;;;;;; 2.21 ;;;;;;
;; Implement the data type of environments, as in section 2.2.2, using define-datatype.
;; then include has-binding? of exercise 2.9.

(define-datatype envrm env?
  (empty-env2)
  (extend-env2
   (var symbol?)
   (val number?)
   (env env?)))

(define (apply-env2 env search-var)
  (cases envrm env
    (empty-env2 () (eopl:error 'apply-env2 "No binding for ~s" search-var))
    (extend-env2 (var val env)(if (eqv? var search-var)
                                  val
                                  (apply-env2 env search-var)))))

(define (has-binding?2 env search-var)
  (cases envrm env
    (empty-env2 () #f)
    (extend-env2 (var val env)(or (eqv? var search-var)
                                  (has-binding?2 env search-var)))))


;;;;;; 2.22 ;;;;;;
;; Using define-datatype, implement the stack data type of exercise 2.4

(define-datatype stack2 stack?
  (empty-stack2)
  (push2
   (a-stack stack?)
   (val always?)))

(define (pop2 stack)
  (cases stack2 stack
    (empty-stack2 () (eopl:error 'pop "Cannot pop anything on an empty stack"))
    (push2 (a-stack val) a-stack)))

(define (top2 stack)
  (cases stack2 stack
    (empty-stack2 () (eopl:error 'pop "Cannot find top of an empty stack"))
    (push2 (a-stack val) val)))

(define (empty-stack?2 stack)
  (cases stack2 stack
    (empty-stack2 () #t)
    (push2 (a-stack val) #f)))
  

;;;;;; 2.24 ;;;;;;
;; Using the given definition of binary trees using define-datatype, implement
;; bintree-to-list so that
;; (bintree-to-list (interior-node 'a (leaf-node 3)(leaf-node 4))) returns the list
;; (interior-node a (leaf-node 3) (leaf-node 4))

(define-datatype bintree bintree?
  (leaf-node
   (num integer?))
  (interior-node
   (key symbol?)
   (left bintree?)
   (right bintree?)))

(define (bintree-to-list bt)
  (cases bintree bt
    (leaf-node (num) (list 'leaf-node num))
    (interior-node (key left right)(list 'interior-node
                                         key
                                         (bintree-to-list left)
                                         (bintree-to-list right)))))

(define eg (bintree-to-list (interior-node 'a (leaf-node 3)(leaf-node 4))))

;;;;;; 2.25 ;;;;;;
;; Use cases to write max-interior that takes in a binary tree with at least one
;; interior node and returns the symbol associated with an interior node with a
;; maximal leaf sum


;;;;;; 2.27 ;;;;;;

;;;;;; 2.28 ;;;;;;

;;;;;; 2.29 ;;;;;;

