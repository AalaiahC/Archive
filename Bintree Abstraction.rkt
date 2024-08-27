#lang eopl
(require rackunit)
(require "eopl-extras.rkt")

#|
<bintree> ::= <number>
          ::= (<symbol> <bintree> <bintree>)


(define-datatype bintree bintree?
  (leaf-node
   (datum number?))
  (interior-node
   (key symbol?)
   (left bintree?)
   (right bintree?)))


<slist>    ::= <({sexp}*)>
<sexp>     ::= <symbol>
           ::= <slist>

(define-datatype s-list s-list?
  (empty-s-list)
  (non-empty-s-list
   (first s-exp?)
   (rest s-list?)))
|#

(define-datatype s-exp s-exp?
  (symbol-symbol-exp
   (data symbol?))
  (s-list-symbol-exp
   (data s-list?)))

#|
<slist>    ::= <({sexp}*)>
<sexp>     ::= <symbol> | <slist>
|#

(define-datatype s-list s-list?
  (an-s-list
   (data (list-of symbol-exp?))))

(define (symbol-exp? e)
  (or(symbol? e)
     (and (pair? e)((list-of symbol-exp?) e))))

;; Write a program to sum the numbers in a bintree
(define-datatype bintree bintree?
  (leaf-node
   (data number?))
  (interior-node
   (key symbol?)
   (lst bintree?)
   (rst bintree?)))

;; sample bintree
(define BT0 (leaf-node 4))
(define BT1 (interior-node
             'T
             (leaf-node 2)
             (leaf-node -2)))
(define BT2 (interior-node
             'T
             (interior-node
              'L
              (leaf-node 10)
              (leaf-node 20))
             (interior-node
              'L
              (leaf-node 30)
              (leaf-node 40))))

;; bintree -> number
;; Purpose: Add nums in given bintree
(define (leaf-sum bt)
  (cases bintree bt
    (leaf-node (val)
               val)
    (interior-node (k l r)
                   (+ (leaf-sum l)(leaf-sum r)))))

;; TESTS
(check-equal? (leaf-sum BT0) 4)
(check-equal? (leaf-sum BT1) 0)
(check-equal? (leaf-sum BT2) 100)
