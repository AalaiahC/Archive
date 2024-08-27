#lang fsm

(define (update-direction direction steps)
  (list (remainder (+ direction 1) 4)
        (if (or (= direction 0) (= direction 2))
            (+ steps 1)
            steps)))

(modulo 0 5)
(modulo 1 5)
(modulo 2 5)
(modulo 3 5)
(modulo 4 5)
