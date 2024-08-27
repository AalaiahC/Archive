#lang fsm

; Mr.Hacker claims to have designed his own version of
; EVEN-A-ODD-B (this is represented as M). His claim is
; highly suspicious because all he has done is add states
; that are unreachable from the start state. Design and
; implement a dfa constructor that takes as input a dfa, M,
; and that returns the dfam M', obtained by removing the
; unreachable states from M. Prove that L(M) = L(M').

; Why is it important to remove unreachable states?

; ----------------------------------------------------------



; Name = HACK-EVEN-A-ODD-B
; Σ: '(a b)
; L(HACK-EVEN-A-ODD-B) = {w | w∈{a b}* ∧ w has an even number of a
;                         and an odd number of b}


(define HACK-EVEN-A-ODD-B (make-dfa `(S M N P X Y Z ,DEAD)
                                    '(a b)
                                    'S
                                    '(N)
                                    `((S a P)
                                      (S b N)
                                      (M a N)
                                      (M b P)
                                      (N b S)
                                      (P a S)
                                      (P b M)
                                      (X a Y)
                                      (X b ,DEAD)
                                      (Y b Y)
                                      (Y a ,DEAD)
                                      (,DEAD a ,DEAD)
                                      (,DEAD b ,DEAD)
                                      (Z a ,DEAD)
                                      (Z b ,DEAD))
                                    'no-dead))



; Contract: dfa -> dfa
; Purpose: To change the given dfa by removing unreachable states.
(define (update-dfa dfa)
  ; Contract: dfa -> (listof states)
  ; Purpose: To create a list of states that are reachable from the start state of the given dfa
  (define (find-reachable dfa)
    ; Contract: state (listof states) -> (listof states)
    ; Purpose: To find if a state has been reached and recursively create a list of them
    ; Accumulator Invariant: accum: A list of states that have been visited and are reachable.
    ;                          How: Recursively keeps track of each state that has been reached by cons-ing
    ;                               any state that has been reached with the accum and going on to the next state. 
    (define (helper state accum)
      (if (member state accum)
          accum
          (let ((next-states (map caddr (filter (λ (x) (eq? (car x) state)) (sm-rules dfa)))))
            (foldl (λ (ns accum)(helper ns accum))
                   (cons state accum)
                   next-states))))
    (helper (sm-start dfa) '()))

  (let* ((reachable-states (find-reachable dfa))
         (updated-transitions (filter (lambda (x) (and (member (car x) reachable-states)
                                                    (member (caddr x) reachable-states)))
                                  (sm-rules dfa))))
    (make-dfa reachable-states
              (sm-sigma dfa)
              (sm-start dfa)
              (sm-finals dfa)
              updated-transitions
              'no-dead)))

(check-equal? (sm-apply HACK-EVEN-A-ODD-B '(a))(sm-apply (update-dfa HACK-EVEN-A-ODD-B) '(a)))
(check-equal? (sm-apply HACK-EVEN-A-ODD-B '(a b))(sm-apply (update-dfa HACK-EVEN-A-ODD-B) '(a b)))
(check-equal? (sm-apply HACK-EVEN-A-ODD-B '(b b a a))(sm-apply (update-dfa HACK-EVEN-A-ODD-B) '(b b a a)))
(check-equal? (sm-apply HACK-EVEN-A-ODD-B '())(sm-apply (update-dfa HACK-EVEN-A-ODD-B) '()))
(check-equal? (sm-apply HACK-EVEN-A-ODD-B '(b a a b b))(sm-apply (update-dfa HACK-EVEN-A-ODD-B) '(b a a b b)))
(check-equal? (sm-apply HACK-EVEN-A-ODD-B '(b b b))(sm-apply (update-dfa HACK-EVEN-A-ODD-B) '(b b b)))

(sm-sameresult? HACK-EVEN-A-ODD-B (update-dfa HACK-EVEN-A-ODD-B) '(b b b b a a b b a)) 
(sm-sameresult? HACK-EVEN-A-ODD-B (update-dfa HACK-EVEN-A-ODD-B) '(a a b b a))
(sm-sameresult? HACK-EVEN-A-ODD-B (update-dfa HACK-EVEN-A-ODD-B) '(a b b a b))
(sm-sameresult? HACK-EVEN-A-ODD-B (update-dfa HACK-EVEN-A-ODD-B) '(a a a a a a b b b b a b b a b a b))
(sm-sameresult? HACK-EVEN-A-ODD-B (update-dfa HACK-EVEN-A-ODD-B) '())

(sm-testequiv? HACK-EVEN-A-ODD-B (update-dfa HACK-EVEN-A-ODD-B))
(sm-testequiv? HACK-EVEN-A-ODD-B (update-dfa HACK-EVEN-A-ODD-B) 250)

#|
----------------------------------------------------------

Above we have provided the code that contains Mr.Hacker's
implementation of EVEN-A-ODD-B. His implementation contains
unreachable states so we designed a function that takes in
any dfa and outputs the given dfa with the unreachable states removed. To further prove
that this function works as intended we must prove L(M) = L(M').

Firstly we need to show that both machines accept the same language:
L(M) = {w | w∈{a b}* ∧ w has an even number of a and an odd number of b}.
To prove this we would show that L(M) is a subset of L(M') and L(M') is a subset of L(M). To show this
we must also show that any word accepted into L(M) is also accepted into L(M') and any word rejected by L(M)
is also rejected by L(M').

Lets assume w is accepted into M. This means that w went through a list of transitions
to reach a final state in M from the start state. Since M' has the same reachable transitions as M without unreachable states, it is safe to assume
that M' also has the same list of transitions to reach a final state from the start state. Hence, w would also be accepted in M'.

Lets assume w is accepted into M'. This means that w went through a list of transitions to reach a final state
in M' from the start state. Since M has the same transitions to reachable states as M' but with unreachable states, this does not affect the accepted
or rejected words because they are not reachable from the start state. It is safe to assume
that M also has this list of transitions to reach a final state from the start state. Hence, w would also be accepted in M.

Lets assume w is rejected in M. This means that when reading w, there was no list of transitions from the starting state to a final state in M.
In this case, since M has unreachable states, this does not affect the accepted or rejected words because they are not
reachable from the start state. Since M' has the same reachable transitions as M without unreachable states, it is safe to assume
that M' also rejects w since there is no list of transitions from the starting state to a final state in M'. Hence, w would also be rejected in M'.

Lets assume w is rejected in M'. This means that when reading w, there was no list of transitions from the starting state to a final state in M'.
In this case, since M has the same reachable transitions as M' but with unreachable states, this does not affect the accepted or rejected words because they are not
reachable from the start state. It is safe to assume that in M, when reading w, there was no list of transitions from the starting state
to a final state in M. Hence, w would also be rejected in M. 

This proves that L(M) is a subset of L(M') and L(M') is a subset of L(M), thus L(M) = L(M')

It is important to remove unreachable states in a dfa because unreachable states have no use in the final word reading of the machine.
Having unreachable states take up unnecessary memory and may make the dfa slower and not optimized.
It is always a good idea to have the least amount of states possible for a dfa because it makes proving
the state invariants hold easier and faster. Adding useless states to the machine may also cause unwanted errors when testing. 

|#