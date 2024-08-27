#lang eopl
(require rackunit)
(require "eopl-extras.rkt")

#|
<exp>    ::= <number>
         ::= <boolean>
         ::= <id>
         ::= (lambda (<id>*) <exp>)
         ::= (<exp> <exp>*)
|#

(define-datatype expr expr?
  (var-expr
   (id symbol?))
  (num-expr
   (num number?))
  (bool-expr
   (b boolean?))
  (lambda-expr
   (params (list-of symbol?))
   (body expr?))
  (app-expr
   (op expr?)
   (args (list-of expr?))))


;; exp -> expr
;; Purpose: parse the given LC exp
(define (parse-lc-exp e)
  (cond [(symbol? e)(var-expr e)]
        [(number? e)(num-expr e)]
        [(boolean? e)(bool-expr e)]
        [(eq? (car e) 'lambda)
         (lambda-expr (cadr e) (parse-lc-exp (caddr e)))]
        [else (app-expr
               (parse-lc-exp (car e))
               (map (lambda (aexp)(parse-lc-exp aexp))
                    (cdr e)))]))

;; expr -> exp
;; Purpose: unparse the given LC expression
(define (unparse-lc-expr er)
  (cases expr er
    (var-expr (s) s)
    (num-expr (n) n)
    (bool-expr (b) b)
    (lambda-expr (params body)
                 (list 'lambda params (unparse-lc-expr body)))
    (app-expr (op args)
              (cons (unparse-lc-expr op)
                    (map (lambda (exr)(unparse-lc-expr exr)) args)))))

;; Sample exp (concrete syntax)
(define E0 'x)
(define E1 100)
(define E2 #t)
(define E3 '(lambda (x y) (+ x y)))
(define E4 '((lambda (x y) (+ x z)) 2 3))

(check-equal? (unparse-lc-expr (parse-lc-exp E0)) E0)
(check-equal? (unparse-lc-expr (parse-lc-exp E1)) E1)
(check-equal? (unparse-lc-expr (parse-lc-exp E2)) E2)
(check-equal? (unparse-lc-expr (parse-lc-exp E3)) E3)
(check-equal? (unparse-lc-expr (parse-lc-exp E4)) E4)

