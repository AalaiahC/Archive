#lang fsm

(define-struct posn (x y))
;; starting definitions for mutation
(define start-posn (make-posn 0 0))
(define direction 0)
(define steps-left 1)
(define steps 1)
;; posn -> 
(define (print-pair posn)
  (display "(")
  (display (posn-x posn))
  (display ", ")
  (display (posn-y posn))
  (display ") ")
  (newline))

(define (cart)
  

  (define (update-direction)
    (set! direction (remainder (+ direction 1) 4))
    (if (or (= direction 0) (= direction 2))
        (set! steps (+ steps 1))
        steps))

  (let iteration ((current-posn start-posn))
    (print-pair current-posn)
    (cond
      ((= direction 0) (set! current-posn (make-posn (+ (posn-x current-posn) 1) (posn-y current-posn))))
      ((= direction 1) (set! current-posn (make-posn (posn-x current-posn) (+ (posn-y current-posn) 1))))
      ((= direction 2) (set! current-posn (make-posn (- (posn-x current-posn) 1) (posn-y current-posn))))
      ((= direction 3) (set! current-posn (make-posn (posn-x current-posn) (- (posn-y current-posn) 1)))))

    (set! steps-left (- steps-left 1))
    (if (= steps-left 0)
        (and (update-direction)
             (set! steps-left steps)
             (iteration current-posn))
        (iteration current-posn))))

(cart)