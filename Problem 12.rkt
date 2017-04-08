#lang racket

(require "auxiliaries.rkt")

;; Problem 12
; What is the value of the first triangle number to have over five hundred divisors?
(define (triangle-number n)
  (if (= n 1)
      1
      (+ n (triangle-number (- n 1)))))

; counts the numbers of divisors of n
(define (divisors-num n)
  (define (iter x result)
    (if (>= (square x) n)
        (+ (* result 2) (if (= (square x) n) 1 0))
        (if (= (remainder n x) 0)
            (iter (+ x 1) (+ result 1))
            (iter (+ x 1) result))))
  (iter 1 0))

; start with 500 bc that's the absolute minimum if every number 1-500 was a divisor.
; have to find a greater lower bound
; to optimize further you could have a lookup table to store already known divisors
; then you start from the top and if you find on that is in the table you add all of its divisors
(define (p12)
  (define (iter x tri)
    #|
    (define (error-msg divs)
      (display "Reached end of range.") (newline)
      (display "x: ") (display x)
      (display ", b: ") (display b) (newline)
      (display "Divisors: ") (display divs))
    |#
    (let ((divs (divisors-num tri)))
      (if (> divs 500)
          tri
          ;(if (>= x b)
           ;   (error-msg divs)
          (iter (+ x 1) (+ tri (+ x 1))))))
  (iter 500 (triangle-number 500)))