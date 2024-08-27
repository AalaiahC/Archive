#lang fsm

;; Let ∑ = {a b}
;; L = {w | w does not have two consecutive a's
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

(sm-visualize CONS-A
                (list 'S S-INV)
                (list 'A A-INV)
                (list 'M M-INV))