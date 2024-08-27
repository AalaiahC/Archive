#lang eopl

(define (fact0 n)
  (if (= n 0)
      1
      (* n (fact0 (- n 1)))))


;;;;;;;;;;


(define (fact/k n k)
  (if (= n 0)
      (k 1)
      (fact/k (- n 1)(lambda (fn1) (k (* n fn1))))))

(define (fact n)
  (fact/k n (lambda (i) i)))

