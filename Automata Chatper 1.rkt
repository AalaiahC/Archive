#lang fsm

;; Binary tree of numbers, (btof number), is either:
;; 1. '()
;; 2. number
;; 3. (list number (btof number) (btof number))

;; (btof number) ... > ...
;; Purpose: ...
(define (f

;; (btof number) number -> (btof number)
;; Purpose: Scale the given (btof number) by the given scalar
(define (scale-bt a-bt k)
  (cond [(empty? a-bt) a-bt]
        [(number? a-bt) (* k a-bt)]
        [else (list (* k (first a-bt))
                    (scale-bt (second a-bt) k)
                    (scale-bt (third a-bt) k))]))

;; Tests
(check-equal? (scale-bt '() 10) '())
(check-equal? (scale-bt -50 2) -100)
(check-equal? (scale-bt 40 8) 320)
(check-equal? (scale-bt (list 10 '() (list -8 -4 '())) -2)
              (list -20 '() (list 16 8 '())))
(check-equal? (scale-bt (list 0
                              (list 1 2 3)
                              (list 4
                                    (list 5 '() '())
                                    (list 6 7 8))) 3)
              (list 0
                    (list 3 6 9)
                    (list 12
                          (list 15 '() '())
                          (list 18 21 24))))


;; Chapter 1 HW

;; Q1 - Design and implement a recursiive function to insert in the right place
;;      a number in a sorted list of numbers.
;;      Make sure to follow all the steps of the design recipe.

;; number (sorted-list-of number)
;; Purpose: Insert the given number into the correct place in a sorted list
(define (insert-num num slist)
  (cond [(empty? slist)
         (list num)]
        [(< num (first slist))
         (cons num slist)]
        [else (cons (first slist) (insert-num num (rest slist)))]))

;; Tests
(check-equal? (insert-num 3 '(1 2 4)) '(1 2 3 4))
(check-equal? (insert-num 3 '()) '(3))
(check-equal? (insert-num 3 '(4 5 6 7 8)) '(3 4 5 6 7 8))
(check-equal? (insert-num 3 '(0 1 2)) '(0 1 2 3))
(check-equal? (insert-num 3 '(-5 -1 1)) '(-5 -1 1 3))
(check-equal? (insert-num -3 '(-6 -4 -1)) '(-6 -4 -3 -1))


;; Q2 - Design and implement a function to find the longest string in a list of strings.

;; (list-of string) -> string
;; Purpose: Find the longest string in the given list of strings
(define (longest-string str-list longest)
  (cond [(empty? str-list)
         longest]
        [(> (string-length (first str-list)) (string-length longest))
         (longest-string (rest str-list) (first str-list))]
        [else (longest-string (rest str-list) longest)]))
               
         

;; Tests
(check-equal? (longest-string '() "") "")
(check-equal? (longest-string '("hi" "goodbye" "yellow" "fun") "") "goodbye")
(check-equal? (longest-string '("alphabet" "not" "yo") "") "alphabet")
(check-equal? (longest-string '("hap" "map" "nap") "") "hap")
              

;; Q3 - Design and implement a function that takes as input two natural numbers greater than
;;      or equal to 2, a and b, and that returns the greatest common divisor of the two given numbers.

;; num num -> num
;; Purpose: Find the greatest common divisor of the given nnumbers
(define (GCD a b)
  (if (= b 0)
      a
      (GCD b (remainder a b))))

;; Tests
(check-equal? (GCD 3 2) 1)
(check-equal? (GCD 32 16) 16)
(check-equal? (GCD 78 42) 6)
(check-equal? (GCD 2 2) 2)
(check-equal? (GCD 24 32) 8)


;; Q4 - Design and implement a function that merges two sorted list of numbers into one sorted list of numbers

