#lang racket

(require fsm rackunit)

;;L = {w | w has an even number of a and an even number of b

;;STATES
;;S: Even number of a's and even number of b's
;;N: Odd number of a's and even number of b's
;;M: Even number of a's and odd number of b's
;;P: Odd number of a's and odd number of b's

(define EVEN-A-EVEN-B (make-dfa '(S N M P)
                        '(a b)
                        'S
                        '(S)
                        '((S a N)
                          (S b M)
                          (N a S)
                          (N b P)
                          (P a M)
                          (P b N)
                          (M a P)
                          (M b S))
                        'no-dead))

(check-equal? (sm-apply EVEN-A-EVEN-B '()) 'accept)
(check-equal? (sm-apply EVEN-A-EVEN-B '(a a)) 'accept)
(check-equal? (sm-apply EVEN-A-EVEN-B '(a)) 'reject)
(check-equal? (sm-apply EVEN-A-EVEN-B '(b a a)) 'reject)
(check-equal? (sm-apply EVEN-A-EVEN-B '(b b b a)) 'reject)
(check-equal? (sm-apply EVEN-A-EVEN-B '(b b b a a a a a)) 'reject)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;word -> boolean
;;Purpose: determine if given word has an even number of a
;;         and an even number of b
(define (S-INV ci)
  (and (even? (length (filter (λ (s) (eq? s 'a)) ci)))
       (even? (length (filter (λ (s) (eq? s 'b)) ci)))))

;;Tests for S-INV
(check-equal? (S-INV '(a)) #f)
(check-equal? (S-INV '(a b b b a)) #f)
(check-equal? (S-INV '()) #t)
(check-equal? (S-INV '(a a b b)) #t)



;;word -> boolean
;;Purpose: determine if given word has an odd number of a
;;         and an even number of b
(define (N-INV ci)
  (and (odd? (length (filter (λ (s) (eq? s 'a)) ci)))
       (even? (length (filter (λ (s) (eq? s 'b)) ci)))))

;;Tests for N-INV
(check-equal? (N-INV '(a a b)) #f)
(check-equal? (N-INV '(b b b a)) #f)
(check-equal? (N-INV '(a)) #t)
(check-equal? (N-INV '(a a a b b)) #t)



;;word -> boolean
;;Purpose: determine if given word has an even number of a
;;         and an odd number of b
(define (M-INV ci)
  (and (even? (length (filter (λ (s) (eq? s 'a)) ci)))
       (odd? (length (filter (λ (s) (eq? s 'b)) ci)))))

;;Tests for M-INV
(check-equal? (M-INV '(a)) #f)
(check-equal? (M-INV '(a a b b b a)) #f)
(check-equal? (M-INV '(b)) #t)
(check-equal? (M-INV '(a a b b b)) #t)



;;word -> boolean
;;Purpose: determine if given word has an odd number of a
;;         and an odd number of b
(define (P-INV ci)
  (and (odd? (length (filter (λ (s) (eq? s 'a)) ci)))
       (odd? (length (filter (λ (s) (eq? s 'b)) ci)))))

;;Tests for P-INV
(check-equal? (P-INV '(a)) #f)
(check-equal? (P-INV '(a b b b a)) #f)
(check-equal? (P-INV '(a b)) #t)
(check-equal? (P-INV '(a a a b b b)) #t)

(sm-visualize EVEN-A-EVEN-B
                (list 'S S-INV)
                (list 'N N-INV)
                (list 'M M-INV)
                (list 'P P-INV))