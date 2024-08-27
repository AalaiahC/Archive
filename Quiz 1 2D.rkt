#lang fsm

(define (print-posn x y)
  (display "(")
  (display x)
  (display ", ")
  (display y)
  (display ") ")
  (newline))

;; -> (void)
;; To generate all the integer pairs on a cartesian 2D plane
(define (cart)
  ;; posn num num num -> posn
  ;; Purpose: helper function to generate the spiral of the cartesian plane
  (define (generate-cart x y direction steps remaining-steps)
    ;; num num -> list
    ;; Purpose: to update the direction to generate the next integer pair on the cartesian plane
    (define (update-direction direction steps)
      (list (remainder (+ direction 1) 4)
            (if (or (= direction 1) (= direction 3))
                (+ steps 1)
                steps)))

    (define (update-remaining remaining-steps)
      (- remaining-steps 1)) 
   
    (print-posn x y)
    (cond
      ((= direction 1)
       (generate-cart (+ x 1) y direction steps remaining-steps))
      ((= direction 2)
       (if (< y (+ steps 1))
           (generate-cart x (+ y 1) direction steps remaining-steps)
           (generate-cart x y 3 (+ steps 2))))
      ((= direction 3)
       (if (> x (- steps 1))
           (generate-cart (- x 1) y direction steps)
           (generate-cart x y 4 (+ steps 2))))
      ((= direction 4)
       (if (> y (- steps 1))
           (generate-cart x (- y 1) direction steps)
           (generate-cart x y 1 (+ steps 2))))))

(generate-cart (make-posn 0 0) 0 1 1))

(cart) ; This will print the described infinite spiral pattern