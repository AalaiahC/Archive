#lang fsm

;;L = {w | there is a single Σ element not in w}

;;STATES
;;S: no elements 
;;A: only a
;;B: only b
;;C: only c
;;M: a and b
;;L: a and c
;;P: b and c
;;N: a, b and c

(define ABC (make-dfa '(S A B C M L P N)
                      '(a b c)
                      'S
                      '(M L P)
                      '((S a A) (S b B) (S c C)
                                (A a A) (A b M) (A c L)
                                (B a M) (B b B) (B c P)
                                (C a L) (C b P) (C c C)
                                (M a M) (M b M) (M c N)
                                (L a L) (L b N) (L c L)
                                (P a N) (P b P) (P c P)
                                (N a N) (N b N) (N c N))
                      'no-dead))

(check-equal? (sm-apply ABC '()) 'reject)
(check-equal? (sm-apply ABC '(a a)) 'reject)
(check-equal? (sm-apply ABC '(a)) 'reject)
(check-equal? (sm-apply ABC '(b a a)) 'accept)
(check-equal? (sm-apply ABC '(b b b a)) 'accept)
(check-equal? (sm-apply ABC '(b b b a a a a a)) 'accept)
(check-equal? (sm-apply ABC '(a b c)) 'reject)
(check-equal? (sm-apply ABC '(c b b a)) 'reject)
(check-equal? (sm-apply ABC '(c)) 'reject)
(check-equal? (sm-apply ABC '(b)) 'reject)
(check-equal? (sm-apply ABC '(c a)) 'accept)
(check-equal? (sm-apply ABC '(b c)) 'accept)
(check-equal? (sm-apply ABC '(c a b)) 'reject)
(check-equal? (sm-apply ABC '(b a c)) 'reject)
(check-equal? (sm-apply ABC '(a c c)) 'accept)
(check-equal? (sm-apply ABC '(c a a a)) 'accept)
(check-equal? (sm-apply ABC '(b a a a b b b b a)) 'accept)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;word -> boolean
;;Purpose: determine if the word has no elements
(define (S-INV ci)
  (empty? ci))

;;Tests for S-INV
(check-equal? (S-INV '()) #t)
(check-equal? (S-INV '(a b b b a)) #f)
(check-equal? (S-INV '(a b)) #f)
(check-equal? (S-INV '(a a a b b b)) #f)



;;word -> boolean
;;Purpose: determine if the word has only a
(define (A-INV ci)
  (if (andmap (λ (x) (eq? x 'a)) ci)
      (< 0 (length (filter (λ (x) (eq? x 'a)) ci)))
      #f))

;;Tests for A-INV
(check-equal? (A-INV '()) #f)
(check-equal? (A-INV '(a)) #t)
(check-equal? (A-INV '(a a)) #t)
(check-equal? (A-INV '(a b)) #f)
(check-equal? (A-INV '(a a a b b b)) #f)

;;word -> boolean
;;Purpose: determine if the word has only b
(define (B-INV ci)
  (if (andmap (λ (x) (eq? x 'b)) ci)
      (< 0 (length (filter (λ (x) (eq? x 'b)) ci)))
      #f))

;;Tests for B-INV
(check-equal? (B-INV '()) #f)
(check-equal? (B-INV '(b)) #t)
(check-equal? (B-INV '(b b)) #t)
(check-equal? (B-INV '(a b)) #f)
(check-equal? (B-INV '(a a a b b b)) #f)



;;word -> boolean
;;Purpose: determine if the word has only c
(define (C-INV ci)
  (if (andmap (λ (x) (eq? x 'c)) ci)
      (< 0 (length (filter (λ (x) (eq? x 'c)) ci)))
      #f))

;;Tests for C-INV
(check-equal? (C-INV '()) #f)
(check-equal? (C-INV '(c)) #t)
(check-equal? (C-INV '(c c)) #t)
(check-equal? (C-INV '(a c)) #f)
(check-equal? (C-INV '(c b b b)) #f)



;;word -> boolean
;;Purpose: determine if the word has only a and b
(define (M-INV ci)
  (if(andmap (λ (x) (or (eq? x 'a)
                        (eq? x 'b))) ci)
     (and (< 0 (length (filter (λ (x) (eq? x 'a)) ci)))
          (< 0 (length (filter (λ (x) (eq? x 'b)) ci))))
     #f))
  

;;Tests for M-INV
(check-equal? (M-INV '()) #f)
(check-equal? (M-INV '(a b)) #t)
(check-equal? (M-INV '(a a a b)) #t)
(check-equal? (M-INV '(a c)) #f)
(check-equal? (M-INV '(c b b b)) #f)
(check-equal? (M-INV '(a b c)) #f)
            


;;word -> boolean
;;Purpose: determine if the word has only a and c
(define (L-INV ci)
  (if(andmap (λ (x) (or (eq? x 'a)
                        (eq? x 'c))) ci)
     (and (< 0 (length (filter (λ (x) (eq? x 'a)) ci)))
          (< 0 (length (filter (λ (x) (eq? x 'c)) ci))))
     #f))

;;Tests for L-INV
(check-equal? (L-INV '()) #f)
(check-equal? (L-INV '(a c)) #t)
(check-equal? (L-INV '(a a a c)) #t)
(check-equal? (L-INV '(a b)) #f)
(check-equal? (L-INV '(c b b b)) #f)
(check-equal? (L-INV '(a b c)) #f)




;;word -> boolean
;;Purpose: determine if the word has only b and c
(define (P-INV ci)
  (if(andmap (λ (x) (or (eq? x 'b)
                        (eq? x 'c))) ci)
     (and (< 0 (length (filter (λ (x) (eq? x 'b)) ci)))
          (< 0 (length (filter (λ (x) (eq? x 'c)) ci))))
     #f))

;;Tests for P-INV
(check-equal? (P-INV '()) #f)
(check-equal? (P-INV '(b c)) #t)
(check-equal? (P-INV '(b b b c)) #t)
(check-equal? (P-INV '(a b)) #f)
(check-equal? (P-INV '(a b b b)) #f)
(check-equal? (B-INV '(a b c)) #f)



;;word -> boolean
;;Purpose: determine if the word has only a, b, and c
(define (N-INV ci)
  (if(andmap (λ (x) (or (eq? x 'b)
                        (eq? x 'c)
                        (eq? x 'a))) ci)
     (and (< 0 (length (filter (λ (x) (eq? x 'b)) ci)))
          (< 0 (length (filter (λ (x) (eq? x 'c)) ci)))
          (< 0 (length (filter (λ (x) (eq? x 'a)) ci))))
     #f))

;;Tests for N-INV
(check-equal? (N-INV '()) #f)
(check-equal? (N-INV '(a b c)) #t)
(check-equal? (N-INV '(b b a b c)) #t)
(check-equal? (N-INV '(a b)) #f)
(check-equal? (N-INV '(a b b b)) #f)



(sm-visualize ABC
                (list 'S S-INV)
                (list 'A A-INV)
                (list 'B B-INV)
                (list 'C C-INV)
                (list 'M M-INV)
                (list 'L L-INV)
                (list 'P P-INV)
                (list 'N N-INV))