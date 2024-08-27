#lang fsm

(define wcwˆr (make-ndpda '(S P Q F)
                          '(a b c)
                          '(a b)
                          'S
                          '(F)
                          `(((S ,EMP ,EMP)(P ,EMP))
                            ((P a ,EMP)(P (a)))
                            ((P b ,EMP)(P (b)))
                            ((P c ,EMP)(Q,EMP))
                            ((Q a (a))(Q ,EMP))
                            ((Q b (b))(Q ,EMP))
                            ((Q ,EMP ,EMP)(F ,EMP)))))

(define a^nb^n (make-ndpda '(S M F)
                           '(a b)
                           '(a)
                           'S
                           '(F)
                           `(((S ,EMP ,EMP) (M ,EMP))
                             ((S a ,EMP) (S (a)))
                             ((M b (a)) (M ,EMP))
                             ((M ,EMP ,EMP) (F ,EMP)))))

;; Tests for a^nb^n

(check-equal? (sm-apply a^nb^n '(a)) 'reject)
(check-equal? (sm-apply a^nb^n '()) 'accept)
(check-equal? (sm-apply a^nb^n '(b b)) 'reject)
(check-equal? (sm-apply a^nb^n '(a b b)) 'reject)
(check-equal? (sm-apply a^nb^n '(a b a a b b)) 'reject)
(check-equal? (sm-apply a^nb^n '(a a b b)) 'accept)


(define anbn
  (make-mttm
   '(S C A B E F)
   '(a b)
   'S
   '(F)
   (list
    (list (list 'S (list BLANK BLANK))
          (list 'C (list RIGHT RIGHT)))
    (list (list 'C (list BLANK BLANK))
          (list 'F (list BLANK BLANK)))
    (list (list 'C (list 'a BLANK))
          (list 'A (list 'a 'a)))
    (list (list 'A (list 'a 'a))
          (list 'A (list RIGHT RIGHT)))
    (list (list 'A (list 'a BLANK))
          (list 'A (list 'a 'a)))
    (list (list 'A (list 'b BLANK))
          (list 'B (list 'b LEFT)))
    (list (list 'B (list 'b 'a))
          (list 'E (list RIGHT BLANK)))
    (list (list 'E (list 'b BLANK))
          (list 'B (list 'b LEFT)))
    (list (list 'E (list BLANK BLANK))
          (list 'F (list BLANK BLANK))))  
   2
   'F))

;; Tests anbn

(check-equal? (sm-apply anbn `(,LM ,BLANK a a b b a) 1) 'reject)
(check-equal? (sm-apply anbn `(,LM ,BLANK a a a) 1) 'reject)
(check-equal? (sm-apply anbn `(,LM ,BLANK a b b) 1) 'reject)
(check-equal? (sm-apply anbn `(,LM ,BLANK) 1) 'accept)
(check-equal? (sm-apply anbn `(,LM ,BLANK a b a b) 1) 'reject)
(check-equal? (sm-apply anbn `(,LM ,BLANK a b b a a b a b b a) 1) 'reject)
(check-equal? (sm-apply anbn `(,LM ,BLANK a a a b b b) 1) 'accept)






