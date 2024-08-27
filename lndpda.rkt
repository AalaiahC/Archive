#lang fsm

;; ndfa→pda
;; Purpose: Convert the given ndfa to a pda
(define (ndfa->pda M)
  (let [(states (sm-states M))
        (sigma (sm-sigma M))
        (start (sm-start M))
        (finals (sm-finals M))
        (rules (sm-rules M))]
    (make-ndpda states
                sigma
                '()
                start
                finals
                (map (λ (r) (list (list (first r)(second r) EMP)
                                  (list (third r) EMP)))
                     rules))))

;;L={}∪aa*∪ab*
(define LNDFA
  (make-ndfa '(S A B F)
             '(a b)
             'S
             '(A B F)
             `((S a A)
               (S a B)
               (S ,EMP F)
               (A b A)
               (B a B))))
;;TestsforLNDFA
(check-equal?(sm-apply LNDFA '(a b a)) 'reject)
(check-equal?(sm-apply LNDFA '(b b b b b)) 'reject)
(check-equal?(sm-apply LNDFA '(a b b b b a a a)) 'reject)
(check-equal?(sm-apply LNDFA '()) 'accept)
(check-equal?(sm-apply LNDFA '(a)) 'accept)
(check-equal?(sm-apply LNDFA '(a a a a)) 'accept)
(check-equal?(sm-apply LNDFA '(a b b)) 'accept)


;; Sample pda
(define LNDFA-PDA (ndfa->pda LNDFA))
(check-equal?(sm-testequiv? LNDFA-PDA LNDFA)#t)

;; pda → mttm
;; Purpose: Convert the given pda to a mttm
(define (pda2mttm M)
  (let* [(pstates (sm-states M))
         (palphabet (sm-sigma M))
         (pstack-a (sm-gamma M))
         (pstart (sm-start M))
         (pfinals (sm-finals M))
         (prules (sm-rules M))
         (new-start (generate-symbol 'S pstates))
         (new-final (generate-symbol 'Y pstates))]
    (make-mttm pstates
               palphabet
               pstart
               pfinals
               (map (λ (r) (list (list (first r)(second r) EMP)
                                 (list (third r) (fourth r)EMP)))
                    prules)
               2
               'Y)))

(define a^nb^n (make-ndpda '(S M F)
                           '(a b)
                           '(a)
                           'S
                            '(F)
                           `(((S ,EMP ,EMP) (M ,EMP))
                             ((S a ,EMP) (S (a)))
                             ((M b (a)) (M ,EMP))
                             ((M ,EMP ,EMP) (F ,EMP)))))

(pda2mttm LNDFA-PDA)
