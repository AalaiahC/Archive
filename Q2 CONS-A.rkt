#lang fsm

;; Let ∑ = {a b}
;; L = {w | w does not have two consecutive a's}
;; Name: CONS-A

;; STATES
;; S: no consecutive a's
;; A: one a
;; M: Consecutive a's

(define CONS-A (make-dfa '(S A M)
                        '(a b)
                        'S
                        '(S A)
                        '((S a A)
                          (S b S)
                          (A a M)
                          (A b S)
                          (M a M)
                          (M b M))
                        'no-dead))

(check-equal? (sm-apply CONS-A '()) 'accept)
(check-equal? (sm-apply CONS-A '(a a)) 'reject)
(check-equal? (sm-apply CONS-A '(a)) 'accept)
(check-equal? (sm-apply CONS-A '(b a a)) 'reject)
(check-equal? (sm-apply CONS-A '(b b b a)) 'accept)
(check-equal? (sm-apply CONS-A '(a b a b a)) 'accept)



(define CONSECUTIVE '(a a))
;; word word → Boolean
;; Purpose: Determine if the second given word appears in
;; the first given word
(define (contains? w pattern)
  (cond [(< (length w) (length pattern)) #f]
        [(equal? (take w (length pattern)) pattern) #t]
        [else (contains? (rest w) pattern)]))

;; Tests for contains?
(check-equal? (contains? '()
                         CONSECUTIVE) #f)
(check-equal? (contains? '(a b b a a)
                         CONSECUTIVE) #t)
(check-equal? (contains? '(b b b a b a b b a b a)
                         CONSECUTIVE) #f)
(check-equal? (contains? '(a b a a)
                         CONSECUTIVE) #t)
(check-equal? (contains? '(a b b b a b a a a)
                         CONSECUTIVE) #t)
(check-equal? (contains? '(a b a b a a a b a a b)
                         CONSECUTIVE) #t)



;;word -> boolean
;;Purpose: determine if given word has no consecutive a's
(define (S-INV ci)
  (not (contains? ci CONSECUTIVE)))

;;Tests for S-INV
(check-equal? (S-INV '(a)) #t)
(check-equal? (S-INV '(a b b b a)) #t)
(check-equal? (S-INV '()) #t)
(check-equal? (S-INV '(a a b b)) #f)



;;word -> boolean
;;Purpose: determine if given word has one a and no consecutive a's
(define (A-INV ci)
  (and (not (contains? ci CONSECUTIVE))
       (contains? ci '(a))))

;;Tests for A-INV
(check-equal? (A-INV '(a)) #t)
(check-equal? (A-INV '(a b b b a)) #t)
(check-equal? (A-INV '()) #f)
(check-equal? (A-INV '(a a b b)) #f)



;;word -> boolean
;;Purpose: determine if given word has consecutive a's
(define (M-INV ci)
  (contains? ci CONSECUTIVE))

;;Tests for M-INV
(check-equal? (M-INV '(a)) #f)
(check-equal? (M-INV '(a b b b a)) #f)
(check-equal? (M-INV '()) #f)
(check-equal? (M-INV '(a a b b)) #t)

;(sm-visualize CONS-A
;                (list 'S S-INV)
;                (list 'A A-INV)
;                (list 'M M-INV))

#|

M = EVEN-B
∑ = (sm-sigma M)
F = (sm-finals M)
w e ∑*
ci = the consumed input

Theorem: The state invariants hold when M is applies to w
Proof:
Proof by induction on the # of transitions, n, M makes to consume w

BASE CASE: n = 0
If n is 0, then the consumed input it '(), and M is in S.
This means the ci has no consecutive a's. Therefore, S-INV holds

INDUCTIVE STEP:
Assume: state invariants hold for n = k
Show: State invariants hold for n = k + 1

If n is 0, then the ci cannot be '() given that the machine must have consumed
at least one symbol. Therefore, we can state that ci = xa such that
|ci| = k + 1, x e ∑* and a e ∑. M's computation to consume ci has k + 1 steps:

(xa s) -> ^k (a r) -> ('() q), where r,q e S

Given that |x| = k, the inductive hypothesis informs us that the state invariants
hold when x is consumed by M. We must show that the state invariants hold for
the k + 1 transition into q. That is, we must show for every transition, the
invariant hold for the state transitioned into. Consider every transition:

(S a A): By Inductive Hypothesis, S-INV holds. Consuming an a means that 

(S b S):

(A a M):

(A b S):

(M a M):

(M b M):

|#