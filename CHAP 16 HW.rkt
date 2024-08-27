#lang fsm

;;  PRE: tape = (LM w) AND i=k>0 AND w in (a b BLANK)*
;; POST: tape = (LM w) AND i=k+2 AND w in (a b BLANK)*

(define R^2
  (make-tm '(S A F)
           '(a b)
           `(((S a)(A ,RIGHT))
            ((S b)(A ,RIGHT))
            ((S ,BLANK)(A ,RIGHT))
            ((A a)(F ,RIGHT))
            ((A b)(F ,RIGHT))
            ((A ,BLANK)(F ,RIGHT)))
           'S
           '(F)))

(check-equal? (last
               (sm-showtransitions R^2 `(,LM a b a) 1)) `(F 3 (,LM a b a))) 
(check-equal? (last
               (sm-showtransitions R^2 `(,LM a b a) 3)) `(F 5 (,LM a b a ,BLANK ,BLANK)))
(check-equal? (last
               (sm-showtransitions R^2 `(,LM b b a a) 4)) `(F 6(,LM b b a a ,BLANK ,BLANK)))


;;;;;;;;;;;;;;;;;;;;;;;;;;; 1 ;;;;;;;;;;;;;;;;;;;;;;;;

;;  PRE: tape = (LM w) AND i=k>1 AND w in (a b BLANK)*
;; POST: tape = (LM w) AND i=k-2 AND w in (a b BLANK)*

(define L^2
  (make-tm '(S A F)
           '(a b)
           `(((S a)(A ,LEFT))
            ((S b)(A ,LEFT))
            ((S ,BLANK)(A ,LEFT))
            ((A a)(F ,LEFT))
            ((A b)(F ,LEFT))
            ((A ,BLANK)(F ,LEFT)))
           'S
           '(F)))

(check-equal? (last
               (sm-showtransitions L^2 `(,LM a b a) 2)) `(F 0 (,LM a b a))) 
(check-equal? (last
               (sm-showtransitions L^2 `(,LM a b a) 3)) `(F 1 (,LM a b a)))
(check-equal? (last
               (sm-showtransitions L^2 `(,LM b b a a) 4)) `(F 2 (,LM b b a a)))

;;;;;;;;;;;;;;;;;;;;;;;;;;; 2 ;;;;;;;;;;;;;;;;;;;;;;;;

;;  PRE: tape = (LM w) AND i=k>0 AND tape[i] = s, where w in {a b}*
;; POST: tape = (LM w) AND i=k AND tape[i] = a
(define Wab-ba (make-tm '(S H)
                        '(a b)
                        `(((S a) (H b))
                          ((S b) (H a)))
                        'S
                        '(H)))

(check-equal? (last (sm-showtransitions Wab-ba `(,LM b) 1)) `(H 1 (,LM a)))
(check-equal? (last (sm-showtransitions Wab-ba `(,LM b a a a) 3)) `(H 3 (,LM b a b a)))

;;  PRE: tape = (LM w) AND i=k>0 AND tape[i] = s, where w in {a b}*
;; POST: tape = (LM w) AND i=k AND tape[i] = a
(define Wswap (make-tm '(S A B C D E F)
                        '(a b)
                        `(((S a) (A ,RIGHT))
                          ((S b) (B ,RIGHT))
                          ((A a) (F ,))
                          ((A b) (C a))
                          ((B a) (C b))
                          ((B b) (F ,LEFT))
                          ((C a) (E ,LEFT))
                          ((C b) (E ,LEFT)))
                        'S
                        '(F)))

(check-equal? (last (sm-showtransitions Wswap `(,LM b) 1)) `(F 1 (,LM b)))
(check-equal? (last (sm-showtransitions Wswap `(,LM b a a a) 3)) `(H 3 (,LM b a b a)))