#lang racket

(require "prime.rkt")

;; Problem 1
; Find the sum of all the multiples of 3 or 5 below 1000.
(define (p1) (sum-of-multiples 1000 3 5))

; finds sum of multiples of [numbers] below [threshold]
(define (sum-of-multiples threshold . numbers)
  ; tests if [x] is multiple of any element of [numbers]
  (define (multiple? x)
    (define (iter to-test)
      (if (null? to-test)
          #f
          (if (= (remainder x (car to-test)) 0)
              #t
              (iter (cdr to-test)))))
    (iter numbers))
  ; make interval between 1 and [threshold] - 1, filter multiples and sum them up
  (apply + (filter
            (lambda (x) (multiple? x))
            (enumerate-interval 1 (- threshold 1)))))

; makes a list containing (a, a+1, ..., b)
(define (enumerate-interval a b)
  (if (> a b)
      '()
      (cons a (enumerate-interval (+ a 1) b))))

;; Problem 2
;By considering the terms in the Fibonacci sequence whose values do not exceed four million, find the sum of the even-valued terms.
(define (p2) (sum-of-even-fibs 4000000))
; sums up all even fibonacci numbers up to [thresh]
(define (sum-of-even-fibs thresh)
  ; creates a list with all even fibs up to [thresh]
  (define (make-list n result)
    (let ((fibn (fib n)))
      (if (> fibn thresh)
          result
          (make-list (+ n 1) (if (even? fibn)
                                 (cons fibn result)
                                 result)))))
    (apply + (make-list 1 '())))
(define (even? x) (= (remainder x 2) 0))
(define (fib n)
  (if (or (= n 1) (= n 2))
      1
      (+ (fib (- n 1))
         (fib (- n 2)))))

;; Problem 3
; What is the largest prime factor of the number 600851475143 ?
; primitive prime procedure
(define (p3) (highest-prime-factor 600851475143))
(define (highest-prime-factor n)
  (define (next-factor)
    (define (iter x)
      (define (next p)
        (if (= p 2)
            3
            (+ p 2)))
      (if (= (remainder n x) 0)
          (if (miller-rabin x)
              x
              (iter (next x)))
          (iter (next x))))
    (iter 2))      
  (if (miller-rabin n)
      n
      (highest-prime-factor (/ n (next-factor)))))

; different approach where I just print all prime factors
; seems to be faster as I use miller rabin less
(define (square x) (* x x))
(define (print-factors n)
  (define (iter x)
    (define (next p)
      (if (= p 2)
          3
          (+ p 2)))
    (if (> (square x) n)
        0
        (if (= (remainder n x) 0)
            (if (miller-rabin x)
                (let ((k 1))
                  (newline)
                  (display x)
                  (iter (next x)))
                (iter (next x)))
            (iter (next x)))))   
  (iter 2))