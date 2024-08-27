#lang fsm

;; Let ∑ = {a b}
;; L = {w | w has an even number of b}
;; Name: EVEN-B

;; STATES
;; S: even b's
;; B: odd b's

(define EVEN-B (make-dfa '(S B)
                         '(a b)
                         'S
                         '(S)
                         '((S a S)
                           (S b B)
                           (B a B)
                           (B b S))
                         'no-dead))

(check-equal? (sm-apply EVEN-B '()) 'accept)
(check-equal? (sm-apply EVEN-B '(a a)) 'accept)
(check-equal? (sm-apply EVEN-B '(a)) 'accept)
(check-equal? (sm-apply EVEN-B '(b a a)) 'reject)
(check-equal? (sm-apply EVEN-B '(b b b a)) 'reject)
(check-equal? (sm-apply EVEN-B '(b b)) 'accept)
(check-equal? (sm-apply EVEN-B '(b a b)) 'accept)


;; word -> boolean
;; Purpose: determine if given word has an even # of b's
(define (S-INV ci)
  (even? (length (filter (λ (s) (eq? s 'b)) ci))))

;;Tests for S-INV
(check-equal? (S-INV '(a)) #t)
(check-equal? (S-INV '(a b b b a)) #f)
(check-equal? (S-INV '()) #t)
(check-equal? (S-INV '(a a b b)) #t)


;; word -> boolean
;; Purpose: determine if given word has an odd # of b's
(define (B-INV ci)
  (odd? (length (filter (λ (s) (eq? s 'b)) ci))))

;;Tests for S-INV
(check-equal? (B-INV '(a)) #f)
(check-equal? (B-INV '(a b b b a)) #t)
(check-equal? (B-INV '()) #f)
(check-equal? (B-INV '(a a b b)) #f)

;(sm-visualize EVEN-B
;                (list 'S S-INV)
;                (list 'B B-INV))