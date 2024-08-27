#lang racket

(require fsm rackunit)

;; L = {w | w has an even number of a and an odd number of b}

;;States
;;S: even number of a and even number of b, start state
;;M: odd number of a and odd number of b
;;N: even number of a and odd number of b, final state
;;P: odd number of a and even number of b

(define EVEN-A-ODD-B (make-dfa '(S M N P)
                               '(a b)
                               'S
                               '(N)
                               '((S a P)
                                 (S b N)
                                 (M a N)
                                 (M b P)
                                 (N a M)
                                 (N b S)
                                 (P a S)
                                 (P b M))
                               'no-dead))

;;Tests for EVEN-A-ODD-B
(check-equal? (sm-apply EVEN-A-ODD-B '()) 'reject)
(check-equal? (sm-apply EVEN-A-ODD-B '(a b b a)) 'reject)
(check-equal? (sm-apply EVEN-A-ODD-B '(b a b b a a)) 'reject)
(check-equal? (sm-apply EVEN-A-ODD-B '(a b)) 'reject)
(check-equal? (sm-apply EVEN-A-ODD-B '(a b b b b)) 'reject)
(check-equal? (sm-apply EVEN-A-ODD-B '(b a b b a a b)) 'reject)
(check-equal? (sm-apply EVEN-A-ODD-B '(b)) 'accept)
(check-equal? (sm-apply EVEN-A-ODD-B '(a a b)) 'accept)
(check-equal? (sm-apply EVEN-A-ODD-B '(a a a b a b b)) 'accept)



