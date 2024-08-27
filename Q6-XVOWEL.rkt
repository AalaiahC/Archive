#lang racket

(require fsm rackunit)

;;L = {w | each vowel occurs once and the
;;         vowels occur in alphabetical order}

;;STATES
;;S: Has only x*
;;A: Has x* one a and x* respectively
;;E: has a and e in order with x* inbetween each
;;I: has a, e, and i in order with x* inbetween each
;;O: has a, e, i, and o in order with x* inbetween each
;;U: has a, e, i, o, and u in order with x* inbetween each
;;N: order not allowed and repeated vowels

(define o-vowel (make-dfa '(S A E I O U N)
                          '(x a e i o u)
                          'S
                          '(U)
                          '((S a A) (S x S) (S e N) (S i N) (S o N) (S u N)
                            (A a N) (A x A) (A e E) (A i N) (A o N) (A u N)
                            (E a N) (E x E) (E e N) (E i I) (E o N) (E u N)
                            (I a N) (I x I) (I e N) (I i N) (I o O) (I u N)
                            (O a N) (O x O) (O e N) (O i N) (O o N) (O u U)
                            (U a N) (U x U) (U e N) (U i N) (U o N) (U u N))
                          'no-dead))

(check-equal? (sm-apply o-vowel '()) 'reject)
(check-equal? (sm-apply o-vowel '(a e i o u)) 'accept)
(check-equal? (sm-apply o-vowel '(a)) 'reject)
(check-equal? (sm-apply o-vowel '(a x e i x o x u)) 'accept)
(check-equal? (sm-apply o-vowel '(x x x x a e i o u)) 'accept)
(check-equal? (sm-apply o-vowel '(a e i x x x x o x x u x x)) 'accept)
(check-equal? (sm-apply o-vowel '(a e i o o u)) 'reject)
(check-equal? (sm-apply o-vowel '(x x a i u o)) 'reject)
(check-equal? (sm-apply o-vowel '(i o u a x e e)) 'reject)
(check-equal? (sm-apply o-vowel '(a e i o u x u)) 'reject)
(check-equal? (sm-apply o-vowel '(x a x e x i x o x u x)) 'accept)

(sm-visualize o-vowel)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; word -> boolean
;; Purpose: to determine if a word has an arbitrary # of x's
(define (S-INV ci)
  (andmap (λ (s) (eq? s 'x)) ci))

(check-equal? (S-INV '()) #t)
(check-equal? (S-INV '(x)) #t)
(check-equal? (S-INV '(x x x x x x x)) #t)
(check-equal? (S-INV '(x a)) #f)
(check-equal? (S-INV '(x x x i x)) #f)
(check-equal? (S-INV '(a)) #f)

;; word -> boolean
;; Purpose: to determine if a word has one a surrounded by
;;          an arbitrary number of x's
(define (A-INV ci)
  (let* [(x1 (takef ci (λ (s)(eq? s 'x))))
         (x2 (drop ci (length x1)))]
    (and (eq? (first x2) 'a)
         (andmap (λ (s)(eq? s 'x))(rest x2)))))

;(check-equal? (A-INV '()) #f)
;(check-equal? (A-INV '(x x x)) #f)
(check-equal? (A-INV '(a)) #t)
(check-equal? (A-INV '(x x x x a x x x x)) #t)
(check-equal? (A-INV '(x a a)) #f)
(check-equal? (A-INV '(a x x x a)) #f)
(check-equal? (A-INV '(x x a x e)) #f)

;; word -> boolean
;; Purpose: to determine if a word has one a and one e in order
;;          surrounded by an arbitrary number of x's
(define (E-INV ci)
  (let* [(x1 (takef ci (λ (s)(eq? s 'x))))
         (x2 (drop ci (length x1)))
         (x3 (takef (rest x2) (λ (s)(eq? s 'x))))
         (x4 (drop (rest x2) (length x3)))]
    (and (eq? (first x2) 'a)
         (eq? (first x4) 'e)
         (andmap (λ (s)(eq? s 'x))(rest x4)))))

;(check-equal? (E-INV '()) #f)
;(check-equal? (E-INV '(x x x x)) #f)
(check-equal? (E-INV '(a e)) #t)
(check-equal? (E-INV '(x x x a e)) #t)
(check-equal? (E-INV '(x x a x x e x x x)) #t)
(check-equal? (E-INV '(x x a e x x e)) #f)
(check-equal? (E-INV '(e x x x a x)) #f)
(check-equal? (E-INV '(x x x x a a x x e)) #f)

;; word -> boolean
;; Purpose: to determine if a word has one a, e and i in order
;;          surrounded by an arbitrary number of x's
(define (I-INV ci)
  (let* [(x1 (takef ci (λ (s)(eq? s 'x))))
         (x2 (drop ci (length x1)))
         (x3 (takef (rest x2)(λ (s)(eq? s 'x))))
         (x4 (drop (rest x2)(length x3)))
         (x5 (takef (rest x4)(λ (s)(eq? s 'x))))
         (x6 (drop (rest x4)(length x5)))]
    (and (eq? (first x2) 'a)
         (eq? (first x4) 'e)
         (eq? (first x6) 'i)
         (andmap (λ (s)(eq? s 'x))(rest x6)))))

;(check-equal? (E-INV '()) #f)
;(check-equal? (E-INV '(x x x x)) #f)
(check-equal? (I-INV '(a e i)) #t)
(check-equal? (I-INV '(x x x a e x x i)) #t)
(check-equal? (I-INV '(x x a x x e x x x i x)) #t)
(check-equal? (I-INV '(x x a i x x e)) #f)
(check-equal? (I-INV '(i x x x a x)) #f)
(check-equal? (I-INV '(x x x x a x x e i i)) #f)

;; word -> boolean
;; Purpose: to determine if a word has one a, e, i and o in order
;;          surrounded by an arbitrary number of x's
(define (O-INV ci)
  (let* [(x1 (takef ci (λ (s)(eq? s 'x))))
         (x2 (drop ci (length x1)))
         (x3 (takef (rest x2)(λ (s)(eq? s 'x))))
         (x4 (drop (rest x2)(length x3)))
         (x5 (takef (rest x4)(λ (s)(eq? s 'x))))
         (x6 (drop (rest x4)(length x5)))
         (x7 (takef (rest x6)(λ (s)(eq? s 'x))))
         (x8 (drop (rest x6)(length x7)))]
    (and (eq? (first x2) 'a)
         (eq? (first x4) 'e)
         (eq? (first x6) 'i)
         (eq? (first x8) 'o)
         (andmap (λ (s)(eq? s 'x))(rest x8)))))

;(check-equal? (O-INV '()) #f)
;(check-equal? (O-INV '(x x x x)) #f)
(check-equal? (O-INV '(a e i o)) #t)
(check-equal? (O-INV '(x x a x e x x x i o x)) #t)
(check-equal? (O-INV '(a x e x i x x x o)) #t)
(check-equal? (O-INV '(x a e x o x i x)) #f)
(check-equal? (O-INV '(a o e i)) #f)
(check-equal? (O-INV '(e a i o)) #f)
(check-equal? (O-INV '(x x x x a x x x e i o o)) #f)
(check-equal? (O-INV '(x a e x x i x x o x x e)) #f)

;; word -> boolean
;; Purpose: to determine if a word has one a, e, i, o  and u in order
;;          surrounded by an arbitrary number of x's
(define (U-INV ci)
  (let* [(x1 (takef ci (λ (s)(eq? s 'x))))
         (x2 (drop ci (length x1)))
         (x3 (takef (rest x2)(λ (s)(eq? s 'x))))
         (x4 (drop (rest x2)(length x3)))
         (x5 (takef (rest x4)(λ (s)(eq? s 'x))))
         (x6 (drop (rest x4)(length x5)))
         (x7 (takef (rest x6)(λ (s)(eq? s 'x))))
         (x8 (drop (rest x6)(length x7)))
         (x9 (takef (rest x8)(λ (s)(eq? s 'x))))
         (x10 (drop (rest x8)(length x9)))]
    (and (eq? (first x2) 'a)
         (eq? (first x4) 'e)
         (eq? (first x6) 'i)
         (eq? (first x8) 'o)
         (eq? (first x10) 'u)
         (andmap (λ (s)(eq? s 'x))(rest x10)))))

;(check-equal? (U-INV '()) #f)
;(check-equal? (U-INV '(x x x x x)) #f)
(check-equal? (U-INV '(a e i o u)) #t)
(check-equal? (U-INV '(x a e i x o x u)) #t)
(check-equal? (U-INV '(x x a x e x i x o x u x)) #t)
(check-equal? (U-INV '(x x a x x o x u x x i)) #f)
(check-equal? (U-INV '(a e i u o)) #f)
(check-equal? (U-INV '(a e i o u u)) #f)
(check-equal? (U-INV '(x x a e x x o x u x i)) #f)

;; word -> boolean
;; Purpose: to determine if a word has one a, e, i, o  and u
;;          not in order and repeated vowels
(define (N-INV ci)
  (and (not (empty? ci))
       (or
        (let* [(x1 (takef ci (λ (s)(eq? s 'x))))
               (x2 (drop ci (length x1)))]
          (not (eq? (first x2) 'a)))
        (let* [(x1 (takef ci (λ (s)(eq? s 'x))))
               (x2 (drop ci (length x1)))
               (x3 (takef (rest x2)(λ (s)(eq? s 'x))))
               (x4 (drop (rest x2)(length x3)))]
          (and (eq? (first x2) 'a)
               (not (eq? (first x4) 'e))))
        (let* [(x1 (takef ci (λ (s)(eq? s 'x))))
               (x2 (drop ci (length x1)))
               (x3 (takef (rest x2)(λ (s)(eq? s 'x))))
               (x4 (drop (rest x2)(length x3)))
               (x5 (takef (rest x4)(λ (s)(eq? s 'x))))
               (x6 (drop (rest x4)(length x5)))]
          (and (eq? (first x2) 'a)
               (eq? (first x4) 'e)
               (not (eq? (first x6) 'i))))
        (let* [(x1 (takef ci (λ (s)(eq? s 'x))))
               (x2 (drop ci (length x1)))
               (x3 (takef (rest x2)(λ (s)(eq? s 'x))))
               (x4 (drop (rest x2)(length x3)))
               (x5 (takef (rest x4)(λ (s)(eq? s 'x))))
               (x6 (drop (rest x4)(length x5)))
               (x7 (takef (rest x6)(λ (s)(eq? s 'x))))
               (x8 (drop (rest x6)(length x7)))]
          (and (eq? (first x2) 'a)
               (eq? (first x4) 'e)
               (eq? (first x6) 'i)
               (not (eq? (first x8) 'o))))
        (let* [(x1 (takef ci (λ (s)(eq? s 'x))))
               (x2 (drop ci (length x1)))
               (x3 (takef (rest x2)(λ (s)(eq? s 'x))))
               (x4 (drop (rest x2)(length x3)))
               (x5 (takef (rest x4)(λ (s)(eq? s 'x))))
               (x6 (drop (rest x4)(length x5)))
               (x7 (takef (rest x6)(λ (s)(eq? s 'x))))
               (x8 (drop (rest x6)(length x7)))
               (x9 (takef (rest x8)(λ (s)(eq? s 'x))))
               (x10 (drop (rest x8)(length x9)))]
          (and (eq? (first x2) 'a)
               (eq? (first x4) 'e)
               (eq? (first x6) 'i)
               (eq? (first x8) 'o)
               (not (eq? (first x10) 'u))))
        (let* [(x1 (takef ci (λ (s)(eq? s 'x))))
               (x2 (drop ci (length x1)))
               (x3 (takef (rest x2)(λ (s)(eq? s 'x))))
               (x4 (drop (rest x2)(length x3)))
               (x5 (takef (rest x4)(λ (s)(eq? s 'x))))
               (x6 (drop (rest x4)(length x5)))
               (x7 (takef (rest x6)(λ (s)(eq? s 'x))))
               (x8 (drop (rest x6)(length x7)))
               (x9 (takef (rest x8)(λ (s)(eq? s 'x))))
               (x10 (drop (rest x8)(length x9)))]
          (and (eq? (first x2) 'a)
               (eq? (first x4) 'e)
               (eq? (first x6) 'i)
               (eq? (first x8) 'o)
               (eq? (first x10) 'u)
               (not (andmap (λ (s)(eq? s 'x))(rest x10))))))))
         
       
(check-equal? (N-INV '(a e i o u)) #f)
(check-equal? (N-INV '(x a x e x i x o x u x)) #f)
(check-equal? (N-INV '(a i o e u)) #t)
(check-equal? (N-INV '(e i a o u)) #t)
(check-equal? (N-INV '(x x a x a)) #t)
(check-equal? (N-INV '(x x x x x x e x)) #t)
(check-equal? (N-INV '(a x x e i i)) #t)


