#lang fsm



; List of symbols representing all lower case letters
; within the Roman alphabet

(define lowers '(a b c d e f g h i j k l m n o p q r s t u v w x y z))

; List of symbols representing all upper case letters
; within the Roman alphabet

(define uppers '(A B C D E F G H I J K L M N O P Q R S T U V W X Y Z))

; List of numbers representing the numbers from 0-9

(define numbers '(0 1 2 3 4 5 6 7 8 9))

; List of strings representing the ending to domains

(define endings '(".com" ".net" ".edu"))

; Contract: (listof symbols) -> (listof regexp)
; Purpose: turns a list of symbols into a list of singleton expressions
(define lc (map
                 (λ (lol) (singleton-regexp (symbol->string lol)))
                 lowers))

; Contract: listof symbols -> (listof regexp)
; Purpose: turns a list of symbols into a list of singleton expressions
(define uc (map
                 (λ (lol) (singleton-regexp (symbol->string lol)))
                 uppers))

; Contract: (listof numbers) -> (listof regexp)
; Purpose: turns a list of numbers into a list of singleton expressions
(define num (map
                 (λ (lol) (singleton-regexp (number->string lol)))
                 numbers))

; Contract: (listof numbers) -> (listof regexp)
; Purpose: To turns a list of endings into a list of singleton expressions
(define end (map
             (λ (los) (singleton-regexp (regexp-quote los)))
             endings))

; Contract: (listof regexp) -> union-regexp
; Purpose: Create a union-regexp using the given list of regular expressions
(define (create-union-regexp L)
  (cond [(< (length L) 2)
         (error "create-union-regexp: list too short")]
        [(empty? (rest (rest L)))
         (union-regexp (first L) (second L))]
        [else
         (union-regexp (first L)
                       (create-union-regexp (rest L)))]))

; TESTS FOR CREATE-UNION-REGEXP
(check-equal?
 (create-union-regexp (list (first lc) (first uc)))
 (union-regexp (singleton-regexp "a")
               (singleton-regexp "A")))

; Generates a single lower case letter at random

(define LOWERS (create-union-regexp lc))

; Generates a single upper case letter at random

(define UPPERS (create-union-regexp uc))

; Generates a single number 1-9 at random

(define NUMBERS (create-union-regexp num))

; Generates a single ending at random

(define ENDINGS (create-union-regexp end))

; 

(define USERNAME (kleenestar-regexp
               (union-regexp LOWERS
                             (union-regexp UPPERS NUMBERS))))

(define USERS (concat-regexp USERNAME (singleton-regexp "@")))

(define COMPANY (kleenestar-regexp
                 (union-regexp LOWERS UPPERS)))

(define DOMAINS (concat-regexp COMPANY ENDINGS))

(define EMAIL (concat-regexp USERS DOMAINS))

(gen-regexp-word EMAIL)
 
; Contract:
; Purpose:
(define (email->string email)
  (list->string
   (map (λ (s)
          (cond
            ((and (symbol? s)
                  (= (length (string->list (symbol->string s))) 1))
             (first (string->list (symbol->string s))))
            ((symbol? s) (first (string->list (symbol->string s))))
            ((number? s) (first (string->list (number->string s))))
            (else error "error: email->string: Emails can only contain symbols and numbers")))                                              
        email)))


; TESTS FOR EMAIL->STRING
(check-equal? (email->string '(h E l L O))
              "hElLO")
(check-equal? (email->string '(V i C t o R 1))
              "ViCtoR1")

; Contract:
; Purpose: 
(define (generate-email)
  (email->string (gen-regexp-word EMAIL)))

(generate-email)

(define a '(A B C D .net))

