#lang racket

(require fsm rackunit)

(define RG (make-rg '(S A)
                    '(a b)
                    ((S ,ARROW ,EMP)
                     (S ,ARROW aA)
                     (A ,ARROW bS))
                    'S))