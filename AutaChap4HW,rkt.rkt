#lang fsm
;; Q1 - Implement a regular expression for the language that contains all words
;; that start with one or more a's and end with a b

(define A (singleton-regexp "a"))

(define B (singleton-regexp "b"))

(define AUB* (kleenestar-regexp (union-regexp A B)))

(define END-B (concat-regexp AUB* B))

(define START-A (concat-regexp A END-B))

(check-equal? (printable-regexp START-A) "a(a ∪ b)*b")

;; Q2 - Let L = {a b}. Implement a regular expression for the language that contains all words that have 3 b's

(define A2 (singleton-regexp "a"))

(define B2 (singleton-regexp "b"))

(define A2* (kleenestar-regexp A2))

(define A2*B (concat-regexp A2* B2))

(define THREE-B (concat-regexp A2*B
                               (concat-regexp A2*B
                                              (concat-regexp A2*B A2*))))

(check-equal? (printable-regexp THREE-B) "a*ba*ba*ba*")

;; Q3 - Let L = {a b}. Implement a regular expression for the language that contains all words of even length

(define A3 (singleton-regexp "a"))

(define B3 (singleton-regexp "b"))

(define A3UB3 (union-regexp A3 B3))

(define AND-AB (concat-regexp A3UB3 A3UB3))

(define EVEN-L (kleenestar-regexp AND-AB))

(check-equal? (printable-regexp EVEN-L) "(a ∪ b)(a ∪ b)*")

;; Q4 - Let L = {0 1}. Implement a regular expression for the language that contains all words that have a single 1

(define ZERO (singleton-regexp "0"))

(define ONE (singleton-regexp "1"))

(define ZERO1 (concat-regexp ZERO ONE))

(define ONE0 (concat-regexp ONE ZERO))

(define ZERO* (kleenestar-regexp ZERO))

(define ZERO10* (concat-regexp ZERO1 ZERO*))

(define 0*ONE0 (concat-regexp ZERO* ONE0))

(define ONLY-ONE (union-regexp ONE (union-regexp ZERO10* 0*ONE0)))

(check-equal? (printable-regexp ONLY-ONE) "(1 ∪ (010* ∪ 0*10))")






