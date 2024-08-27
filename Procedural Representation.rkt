#lang eopl

#|
Procedural Representation

<env> ::= (empty-env)
      ::= (extend-end <symbol> <Racket-value> <env>)
|#

;; -> env
;; Purpose: Construct the empty env
(define (empty-env)
  (lambda (search-var)
    (eopl:error 'apply-env "No binding for ~s" search-var)))

;; symbol X env -> env
;; Purpose: Add a binding to the given env
(define (extend-env var val e)
  (lambda (search-var)
    (cond [(eqv? search-var var) val]
          [else (apply-env e search-var)])))

;; env symbol -> X
;; Purpose: Get the value of given var in given env
(define (apply-env e var)
  (e var))

(define e (extend-env 'x 4 (empty-env)))

