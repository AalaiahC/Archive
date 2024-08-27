#lang fsm

;; States(i = head’s position)
;; S: tape[1..i-1] only contains as, startingstate
;; Y: tape[i] = BLANK and tape[1..i-1] only contains as, finalstate
;; N: tape[i] is b, finalstate

;; L = a*
;; PRE: tape = LMw_ AND i = 0
(define a* (make-tm '(S Y N)
                    '(a b)
                    `(((S a) (S ,RIGHT))
                      ((S b) (N b))
                      ((S ,BLANK) (Y ,BLANK)))
                    'S
                    '(Y N)
                    'Y))

;; Tests for a*

(check-equal?(sm-apply a* `(,LM a a a b a a)) 'reject)
(check-equal?(sm-apply a* `(,LM b a a)) 'reject)
(check-equal?(sm-apply a* `(,LM)) 'accept)
(check-equal?(sm-apply a* `(,LM a a a)) 'accept)



;; tape natnum → Boolean
;; Purpose: Everything in tape[1..i-1] is an a
(define(S-INV t i)
  (or (= i 0)
      (andmap(λ (s)(eq? s 'a))
             (take (rest t)(sub1 i)))))

;;TestsforS-INV
(check-equal?(S-INV `(,LM b ,BLANK) 2) #f)
(check-equal?(S-INV `(,LM a a b a a)4) #f)
(check-equal?(S-INV `(,LM ) 0) #t)
(check-equal?(S-INV `(,LM b) 1) #t)
(check-equal?(S-INV `(,LM a ,BLANK) 2) #t)
(check-equal?(S-INV `(,LM a a a a ,BLANK) 5) #t)



