#lang fsm

;Fig. 68 Function to replace rules that pop two or more elements
;; (listof pda-rule) (listof state) → (listof pda-rule)
;; Purpose: Eliminate rules that pop more than two elements (define (generate-beta<2-rules rules states)
(define (generate-beta<2-rules rules states)
  ;; pda-rule (listof state) → (listof pda-rule)
  ;; Purpose: Create [betal = 1 rules for given rule
  (define (convert-beta=1 r states)
    ;; (listof symbol) (listof state) → (listof pda-rule)
    ;; Purpose: Generate pda rules for given pop list using given states (define (gen-intermediate-rules beta sts)
    (define (gen-intermediate-rules beta sts)
                                    (if (empty? (rest sts))
                                        '()
                                        (cons (mk-pda-rule (first sts) EMP (list (first beta))
                                                           (first (rest sts)) EMP)
                                              (gen-intermediate-rules (rest beta) (rest sts)))))
    (let* [(from (get-from r))
           (read (get-read r))
           (to (get-to r))
           (beta (get-pop r))
           (push (get-push r))
           (new-states (build-list
                        (sub1 (length beta))
                        (lambda (i) (generate-symbol 'B (cons 'B states)))))]
      (append (list
               (mk-pda-rule from EMP (list (first beta)) (first new-states) EMP)
               (mk-pda-rule (last new-states) read (list (last beta)) to push))
              (gen-intermediate-rules (rest beta) new-states))))
  (let* [(beta>=2-rules (filter (lambda (r) (and (not (eq? (get-pop r) EMP))
                                                 (>= (length (get-pop r)) 2)))
                                rules))
         (beta<2-rules (filter (lambda (r) (not (member r beta>=2-rules))) rules))]
    (append beta<2-rules (append-map (lambda (r) (convert-beta=1 r states))
                                     beta>=2-rules))))


;; (listofpda-rule) (listofsymbols)→(listofpda-rules)
;; Purpose: Substitute pop nothing rules with pop 1 rules
(define (generate-beta=1-rules rls gamma)
  (let*[(beta=0-rls (filter (λ (r)(eq? (get-pop r)EMP)) rls))
        (beta>0-rls (filter (λ (r)(not (member r beta=0-rls))) rls))]
    (append beta>0-rls
            (for*/list ([r beta=0-rls]
                        [g gamma])
              (list (list (get-from r)(get-read r)(list g))
                    (list(get-to r)
                         (if(eq?(get-push r)EMP)
                            (list g)
                            (append(get-push r)(list g)))))))))

;;(listofpda-rule)(listofstates)→(listofpda-rule)
;;Purpose:Replace rules that push more than 2 elements
(define (generate-theta<=2-rules rls sts)
  ;;(listofpda-rule)(listofstate)→(listofpda-rule)
  ;;Purpose:Generate rules with |theta|<=2 for given rules
  (define(gen-theta<=2-rules theta>2-rules sts)
    ;;pda-rule→(listofpda-rule)
    ;;Purpose:Generate |theta|<=2 rules for given rule
    (define(gen-rules r)
      ;;(listofstate)(listofsymbol)(listofsymbol)symbol→(listofpda-rule)
      ;;Purpose:Generate |theta|<=2 rules for given push and state lists
      (define(process-sts sts push pop read)
        (if(=(length sts)2)
           (list(mk-pda-rule(first sts)read pop(second sts)push))
           (cons(mk-pda-rule(first sts)EMP pop(second sts)
                            (append pop(list(first push))))
                (process-sts(rest sts)(rest push)pop read))))
      
      (let*[(from (get-from r))
            (read (get-read r))
            (pop(get-pop r))
            (to(get-to r))
            (push(get-push r))
            (new-states(build-list
                        (sub1(length push))
                        (λ(i)(generate-symbol 'T(cons 'T sts)))))
            (rev-push(reverse push))]
        (cons(mk-pda-rule from EMP pop(first new-states)
                          (append pop(list(first rev-push))))
             (process-sts(append new-states(list to))(rest rev-push)
                         pop read))))
    (append-map gen-rules theta>2-rules))
  (let*[(theta>2-rules (filter
                        (λ(r)(and(not(eq?(second(second r))EMP))
                                 (>(length(second(second r)))2)))
                        rls))
        (theta<=2-rules(filter(λ(r)(not(member r theta>2-rules)))
                              rls))]
    (append theta<=2-rules(gen-theta<=2-rules theta>2-rules sts))))


;;state symbol stacke states tacke→pda-rule
;;Purpose:Build a pda-rule
(define(mk-pda-rule from a pop to push)
  (list(list from a pop)(list to push)))

;;pda-rule→state
;;Purpose:Extractfromstate
(define(get-from r)(first(first r)))

;;pda-rule→symbol
;;Purpose:Extractreadsymbol
(define(get-read r)(second(first r)))

;;pda-rule→stacke
;;Purpose:Extractpopelements
(define(get-pop r)(third(first r)))

;;pda-rule→state
;;Purpose:Extracttostate
(define(get-to r)(first(second r)))

;;pda-rule→stacke
;;Purpose:Extractpushelements
(define(get-push r)(second(second r)))

;;(listofpda-rule)→(listofstate)
;;Purpose:Extractstatesinthegivenrules
(define(extract-states rls)
  (remove-duplicates(append-map(λ(r)(list(first(first r))(first(second r))))rls)))



;; pda→pda
;; Purpose: Convert given pda to a simple pda
(define (pda2spda p)
  (let* [(pstates (sm-states p))
         (psigma (sm-sigma p))
         (pgamma (sm-gamma p))
         (pstart (sm-start p))
         (pfinals (sm-finals p))
         (prules (sm-rules p))
         (new-start (generate-symbol 'S pstates))
         (new-final (generate-symbol 'F pstates))
         (bottom (generate-symbol 'Z pgamma))
         (initr (mk-pda-rule new-start EMP EMP pstart (list bottom)))
         (frules (map (λ (s) (mk-pda-rule s EMP (list bottom) new-final EMP))
                      pfinals))
         (beta<2-rules (generate-beta<2-rules prules pstates))
         (beta=1-rules (generate-beta=1-rules beta<2-rules (cons bottom pgamma)))
         (theta<=2-rules (generate-theta<=2-rules beta=1-rules
                                                  (extract-states beta=1-rules)))]
    (make-ndpda (append (list new-final new-start)
                        (remove-duplicates (cons pstart(extract-states theta<=2-rules))))
                psigma
                (cons bottom pgamma)
                new-start
                (list new-final)
                (cons initr (append theta<=2-rules frules)))))

(define anbn (make-ndpda '(S M F)
                           '(a b)
                           '(a)
                           'S
                           '(F)
                           `(((S ,EMP ,EMP) (M ,EMP))
                             ((S a ,EMP) (S (a)))
                             ((M b (a)) (M ,EMP))
                             ((M ,EMP ,EMP) (F ,EMP)))))


(define wcwr (make-ndpda '(S P Q F)
                         '(a b c)
                         '(a b)
                         'S
                         '(F)
                         `(((S ,EMP ,EMP)(P ,EMP))
                           ((P a ,EMP)(P (a)))
                           ((P b ,EMP)(P (b)))
                           ((P c ,EMP)(Q ,EMP))
                           ((Q a (a))(Q ,EMP))
                           ((Q b (b))(Q ,EMP))
                           ((Q ,EMP ,EMP)(F ,EMP)))))

(define anbns (pda2spda anbn))
(define wcwrs (pda2spda wcwr))
(check-equal? (sm-testequiv? anbn anbns)#t)
(check-equal? (sm-testequiv? wcwr wcwrs)#t)


(define (convert pda)
  (let* ([accept-state (generate-symbol 'Y (sm-states pda))]
         [states (cons accept-state (sm-states pda))]
         [sigma (sm-sigma pda)]  ; Alphabet of the NPDA
         [transitions (map (lambda (transition)
                             (let ([src (get-from transition)]
                                   [input (get-read transition)]
                                   [stack-read (get-pop transition)]
                                   [dest (get-to transition)]
                                   [stack-write (get-push transition)])
                               (cond
                                 [(and (equal? stack-read 'EMP) (not (equal? stack-write 'EMP)))
                                  ;; Push operation: Write to stack tape and move right
                                  (list (list src (list input BLANK))
                                        (list dest (list input stack-write RIGHT)))]
                                 [(and (not (equal? stack-read 'EMP)) (equal? stack-write 'EMP))
                                  ;; Pop operation: Move left on stack tape and erase
                                  (list (list src (list input stack-read))
                                        (list dest (list input BLANK LEFT)))]
                                 [(and (equal? stack-read 'EMP) (equal? stack-write 'EMP))
                                  ;; No stack operation: Input handling only
                                  (list (list src (list input BLANK))
                                        (list dest (list input BLANK BLANK)))]
                                 [else
                                  ;; Handle no-op for stack
                                  (list (list src (list input stack-read))
                                        (list dest (list input stack-write BLANK)))])))
                           (sm-rules pda))]
         [init-state (sm-start pda)])
    (make-mttm
     states
     sigma
     init-state
     (list accept-state)
     transitions
     2
     'Y)))



;(convert (pda2spda anbn))

(define (pda-to-mttm-transition pda-transition)
  (match pda-transition
    ((list (list current-state input-symbol stack-operation) (list next-state stack-operation-new))
     (list
      ;; MTTM transition from current state to next state
      (list (map-pda-state current-state) ; Map PDA state to MTTM state
            (list input-symbol (map-pda-stack stack-operation))) ; Read symbols on tapes
      (list (map-pda-state next-state) ; Map PDA state to MTTM state
            (list (map-pda-stack stack-operation-new))))) ; Write symbols on tapes
    (else
     (error "Invalid PDA transition format"))))

(define (map-pda-state state)
  ;; Map PDA state to MTTM state (e.g., S -> 'C, P -> 'P, Q -> 'Q, F -> 'F)
  (cond
    ((equal? state 'S) 'S)
    ((equal? state 'P) 'P)
    ((equal? state 'Q) 'Q)
    ((equal? state 'F) 'F)
    (else
     (error "Invalid PDA state"))))

(define (map-pda-stack stack-operation)
  ;; Map PDA stack operation to MTTM stack operation (e.g., ,EMP -> 'BLANK)
  (cond
    ((equal? stack-operation ',EMP) 'BLANK)
    ;; Add more mappings as needed for other stack symbols
    (else
     (error "Invalid PDA stack operation"))))

;; Example PDA transitions
(define pda-transition1 '((S a ,EMP) (P ,EMP)))
(define pda-transition2 '((P a ,EMP) (P (a))))
(define pda-transition3 '((P b ,EMP) (P (b))))
(define pda-transition4 '((P c ,EMP) (Q ,EMP)))
(define pda-transition5 '((Q a (a)) (Q ,EMP)))
(define pda-transition6 '((Q b (b)) (Q ,EMP)))
(define pda-transition7 '((Q ,EMP ,EMP) (F ,EMP)))

;; Transform PDA transitions to MTTM transitions
(define mttm-transition1 (pda-to-mttm-transition pda-transition1))
(define mttm-transition2 (pda-to-mttm-transition pda-transition2))
(define mttm-transition3 (pda-to-mttm-transition pda-transition3))
(define mttm-transition4 (pda-to-mttm-transition pda-transition4))
(define mttm-transition5 (pda-to-mttm-transition pda-transition5))
(define mttm-transition6 (pda-to-mttm-transition pda-transition6))
(define mttm-transition7 (pda-to-mttm-transition pda-transition7))

;; Display MTTM transitions
(display mttm-transition1)
(display mttm-transition2)
(display mttm-transition3)
(display mttm-transition4)
(display mttm-transition5)
(display mttm-transition6)
(display mttm-transition7)

