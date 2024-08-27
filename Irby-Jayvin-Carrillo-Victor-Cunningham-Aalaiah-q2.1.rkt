#lang eopl
(require rackunit)
(require "eopl-extras.rkt")

#|
Quiz 2 (2.1): Implement the four required operations for bigits. Then use your
              implementation to calculate the factorial of 10. How does the
              execution time vary as this argument changes? How does the execution
              time vary as the base changes? Explain why.
              (Include nnint2dec and dec2nnint)

---INTERFACE---
 zero = |0|
(isZero? |n|), #t if n = 0 and #f if n ̸= 0
(succ |n|) = |n + 1|, n ≥ 0
(pred |n + 1|) = |n|, n ≥ 0
(dec2nnint |n|10) = |n|
(nnint2dec |n|) = |n|10
|#

#|
-----Bignum Representation-----

Numbers are represented in base N (large N) where large N is any number >= 10
The representation uses a list of numbers such that each number is in 0..N-1
(called bigits)

Inductive Definition of |n|
a nnint is either:
f (n) = (
           '() if n = 0
            (cons r |q|) otherwise, wheren = q ∗ N + r, 0 ≤ r < N
         )
|#

;; → nnint
;; Purpose: construct zero
(define zero '())

;; nnint → Boolean
;; Purpose: Determine if given nnint is zero
(define (isZero? n)
  (null? n))

(check-equal? (isZero? '()) #t)
(check-equal? (isZero? (cons 3 5)) #f)

;; nnint → nnint
;; Purpose: Construct the successor of the given nnint
(define N 10)

(define (succ n)
  (if (isZero? n)
      (cons 1 '())
      (if (> (car n) N)
          (cons (add1 (car n)) (cdr n))
          (if (= (add1 (car n)) N)
              (cons (*(car n)0) (+ (cdr n) 1))
              (cons (add1 (car n)) (cdr n))))))

(check-equal? (succ '()) (cons 1 '()))
(check-equal? (succ (cons 9 2)) (cons 0 3))
(check-equal? (succ (cons 2 0)) (cons 3 0))
(check-equal? (succ (cons 7 5)) (cons 8 5))

;; nnint → nnint
;; Purpose: Return the predecessor of given nnint
(define (pred n)
  (if (isZero? n)
      (eopl:error "Zero does not have a predecessor.")
      (if (and (= (car n) 1) (isZero? (cdr n)))
          '()
          (if (= (car n) 0)
              (cons 9 (- (cdr n) 1))
              (cons (- (car n) 1) (cdr n))))))

(check-equal? (pred (cons 0 5)) (cons 9 4))
(check-equal? (pred (cons 8 3)) (cons 7 3))
(check-equal? (pred (cons 4 0)) (cons 3 0))
(check-equal? (pred (cons 1 1)) (cons 0 1))
(check-equal? (pred '(1)) '())

;; number → nnint
;; Purpose: Construct nnint for the given decimal nnint
(define (dec2nnint decimal)
  (cond
    [(= decimal 0) zero]
    [(< decimal N) (cons decimal '())]  ; Base case for single digits
    [else (cons (remainder decimal N) (quotient decimal N))]))

(check-equal? (dec2nnint 0) zero)
(check-equal? (dec2nnint 23) (cons 3 2))
(check-equal? (dec2nnint 1)  (cons 1 '()))
(check-equal? (dec2nnint 109) (cons 9 10))

;; nnint → number
;; Purpose: Return decimal representation of given nnint
(define (nnint2dec n)
  (cond
    [(isZero? n) 0]
    [(isZero? (cdr n)) (car n)]
    [else (+ (* N (cdr n))(car n))]))

(check-equal? (nnint2dec '()) 0)
(check-equal? (nnint2dec (cons 3 2)) 23)
(check-equal? (nnint2dec (cons 6 20)) 206)
(check-equal? (nnint2dec (cons 9 '())) 9)


;; decimal → decimal
;; Purpose: Return the factorial of n
(define (factorial n)
  (nnint2dec (factorial-helper (nnint2dec (succ '())) n)))


;;decimal decimal → nnint
;;Purpose: To multiply the accum by n until n becomes 0

(define (factorial-helper accum n)
  (if (= n 0)
      (dec2nnint accum)
      (factorial-helper (* accum n) (sub1 n))))


(check-equal? (factorial 3) 6)
(check-equal? (factorial 0) 1)
(check-equal? (factorial 10) 3628800)
(check-equal? (factorial 12) 479001600) 

;When the argument of the factorial function becomes larger, the execution
;time increases and when the argument becomes smaller the execution time decreases.
;This is because when the product of 1 to n is calculated, the value increases
;exponentially

;As the base increases, the execution time decreases because there are fewer slots
;of memory being used when computing the factorial.

