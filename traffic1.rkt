;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname traffic1) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;; A tick is an integer greater than or equal to 0.
;; A traffic light (tl) is either
;; 1. 0 --means the green light is on
;; 2. 1 --means the yellow light is on
;; 3. 2 --means the red light is on

(define BACKGROUND (rectangle 60 180 'solid 'black))
(define R-ON (overlay (above (circle 25 'solid 'red)
                             (square 10 'solid 'transparent)
                             (circle 25 'outline 'yellow)
                             (square 10 'solid 'transparent)
                             (circle 25 'outline 'green))
                      BACKGROUND))
(define Y-ON (overlay (above (circle 25 'outline 'red)
                             (square 10 'solid 'transparent)
                             (circle 25 'solid 'yellow)
                             (square 10 'solid 'transparent)
                             (circle 25 'outline 'green))
                      BACKGROUND))
(define G-ON (overlay (above (circle 25 'outline 'red)
                             (square 10 'solid 'transparent)
                             (circle 25 'outline 'yellow)
                             (square 10 'solid 'transparent)
                             (circle 25 'solid 'green))
                      BACKGROUND))



;; tl → image
;; Purpose: Return the traffic light image for the given tl
(define (image-of-tl a-tl)
  (cond [(= a-tl 5) G-ON]
        [(= a-tl 10) Y-ON]
        [else R-ON]))

;; Tests using sample values
(check-expect (image-of-tl 0) G-ON)
(check-expect (image-of-tl 1) Y-ON)
(check-expect (image-of-tl 3) R-ON)



;; tick → tl
;; Purpose: Convert the given tick to a tl
(define (tick->tl a-tick)
  (remainder a-tick 10))

;; Sample expressions for tick->tl
(define TL-0 5)
(define TL-1 10)
(define TL-2 20)

;; Tests for tick->tl using sample expressions
;(check-expect (tick->tl 33) TL-0)
;(check-expect (tick->tl 76) TL-1)
;(check-expect (tick->tl 152) TL-2)
  
;; Tests for tick->tl using sample values
;(check-expect (tick->tl 0) 0)
;(check-expect (tick->tl 451) 1)
;(check-expect (tick->tl 182) 2)



;; tick → image
;; Purpose: Compute the traffic light image for the given tick
(define (draw-tick a-tick)
  (image-of-tl (tick->tl a-tick)))

;; Sample expressions for draw-tick
(define TICK12-IMG (image-of-tl (tick->tl 12)))
(define TICK25-IMG (image-of-tl (tick->tl 25)))
(define TICK32-IMG (image-of-tl (tick->tl 32)))

;; Tests using sample expressions for ticks to images
;(check-expect (draw-tick 12) TICK12-IMG)
;(check-expect (draw-tick 25) TICK25-IMG)
;(check-expect (draw-tick 32) TICK32-IMG)
;  
;;; Tests using sample values for ticks to images
;(check-expect (draw-tick 77) R-ON)
;(check-expect (draw-tick 99) G-ON)
;(check-expect (draw-tick 241) Y-ON)

(animate draw-tick)
