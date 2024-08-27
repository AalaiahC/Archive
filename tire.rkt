;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname tire) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;; The following is your exam question. You should follow our expected
;; design recipe style, and include all the expected components of a
;; well-designed software solution.

;; There are four tires in a set of tires, or five if you include the
;; spare. Each tire has a tire identification number and a size, and
;; each tire also has a mileage on it. A tire identification number
;; (TIN), also known as the tire "serial number", is a 13-digit
;; number, where the last four digits tell you the week and year of
;; manufacture (for instance a TIN with the last four digits 1319
;; tells you that tire was made in the 13th week of the year 2019). A
;; tire’s size consists of three pieces of information: (1) the tire’s
;; width, from sidewall to sidewall, in millimeters, (2) its _aspect_
;; _ratio_ (which is to say, sidewall height/width), and (3) the
;; diameter (in mm) of the wheel the tire will fit. The mileage on
;; a tire is just the number of miles over which the tire has been
;; driven.

;; (a) Write a function average-miles-per-week that, given a tire and
;; the number of weeks since the tire was made, calculates the average
;; number of miles driven on that tire per week.

;; (b) Write a function named average-rotations that, given a set of
;; tires, computes the average number of rotations for a tire in that
;; set.

;;set of tires: tire tire tire tire
(define-struct SOT4 (t1 t2 t3 t4))
;;INVENTORY
;;make-SOT4 creates a set of tires with 4 tires
;;SOT4-t1 this is the first tire
;;SOT4-t2 this is the second tire
;;SOT4-t3 this is the third tire
;;SOT4-t4 this is the fourth tire
;;SOT4? determines if the input is a SOT4

;;set of tires: tire tire tire tire tire
(define-struct SOT5 (t1 t2 t3 t4 t5))
;;INVENTORY
;;make-SOT5 creates a set of tires with 5 tires
;;SOT5-t1 this is the first tire
;;SOT5-t2 this is the second tire
;;SOT5-t3 this is the third tire
;;SOT5-t4 this is the fourth tire
;;SOT5-t4 this is the fifth tire
;;SOT5? determines if the input is a SOT5

;;tire: TIN size mileage
(define-struct tire (TIN size mileage))
;;INVENTORY
;;make-tire creates a tire
;;tire-TIN this is the Tire Identification Number of a tire
;;tire-size this is the size of a tire
;;tire-mileage this is the mileage of a tire
;;tire? determines if the input is a tire

;;TIN: 9num week-year
(define-struct TIN (ID week year))
;;INVENTORY
;;make-TIN creates a TIN
;;TIN-ID this is the ID number of a TIN
;;TIN-week this is the amount of weeks into the year the TIN was created
;;TIN-year this is the year the TIN was created
;;TIN? determines if the input is a TIN

;;size: width aspectratio diameter
(define-struct size (wdth ar diam))
;;INVENTORY
;;make-size creates a tire size
;;size-wdth this is the width of the tire
;;size-ar this is the aspect ratio of the tire
;;size-diam this is the diameter of a tire
;;size? determines if the input is a size


(define tire1 (make-tire
               (make-TIN 146382911 14 22)
               (make-size 185 .75 355)
               7000))
(define tire2 (make-tire
               (make-TIN 173583922 10 19)
               (make-size 190 .75 381)
               283829))
(define tire3 (make-tire
               (make-TIN 195738203 34 13)
               (make-size 235 .75 482)
               3473))
(define tire4 (make-tire
               (make-TIN 102847194 50 20)
               (make-size 150 .7 400)
               13673))
(define tire5 (make-tire
               (make-TIN 129483325 12 05)
               (make-size 195 .7 304)
               4362784))


(define CAR1 (make-SOT4 tire1 tire2 tire3 tire4))
(define CAR2 (make-SOT5 tire1 tire2 tire3 tire4 tire5))

;; tire -> num
;; Purpose:  calculates the average number of miles driven on that tire per week
(define (average-miles-per-week a-tire a-week)
  (/ (tire-mileage a-tire) a-week))

(check-expect (average-miles-per-week tire1 5) 1400)
(check-within (average-miles-per-week tire2 12) 23652.4 0.1)
(check-within (average-miles-per-week tire3 58) 59.8 0.1)
(check-within (average-miles-per-week tire4 10) 1367.3 0.1)
(check-within (average-miles-per-week tire5 26) 167799.4 0.1)

;; SOT -> num
;; Purpose: computes the average number of rotations for a tire in that set.
(define (average-rotations a-SOT)
  (cond [(SOT4? a-SOT)
         (/ (+ (rotations (SOT4-t1 a-SOT))
               (rotations (SOT4-t2 a-SOT))
               (rotations (SOT4-t3 a-SOT))
               (rotations (SOT4-t4 a-SOT))) 4)]
        [(SOT5? a-SOT)
         (/ (+ (rotations (SOT5-t1 a-SOT))
               (rotations (SOT5-t2 a-SOT))
               (rotations (SOT5-t3 a-SOT))
               (rotations (SOT5-t4 a-SOT))
               (rotations (SOT5-t5 a-SOT))) 5)]))

(check-within (average-rotations CAR1) 937.4 0.1)
(check-within (average-rotations CAR2) 12976.4 0.1)


;; tire -> num
;; Purpose: computes the circumference of a tire
(define (circum a-tire)
  (/ (* pi (+ (* 2
              (* (size-ar (tire-size a-tire))
                 (size-wdth (tire-size a-tire))))
           (size-diam (tire-size a-tire)))) 25.4))

(check-within (circum tire1) 78.2 0.1)
(check-within (circum tire2) 82.3 0.1)
(check-within (circum tire3) 103.2 0.1)
(check-within (circum tire4) 75.4 0.1)
(check-within (circum tire5) 71.3 0.1)

;; tire -> num
;; Purpose: compute the rotations of a tire
(define (rotations a-tire)
  (/ (tire-mileage a-tire) (circum a-tire)))

(check-within (rotations tire1) 89.4 0.1)
(check-within (rotations tire2) 3445.6 0.1)
(check-within (rotations tire3) 33.6 0.1)
(check-within (rotations tire4) 181.2 0.1)
(check-within (rotations tire5) 61132.4 0.1)              

