#lang racket

(require "auxiliaries.rkt")

;; Problem 4
; Find the largest palindrome made from the product of two 3-digit numbers.
(define (palindrome? n)
  (let ((digits (digitize n)))
    (equal? digits (reverse digits))))
(define (digitize n)
  (define (highest-power)
    (define (helper pow)
      (if (= (remainder n (* pow 10)) n)
          pow
          (helper (* pow 10))))
    (helper 1))
  (define (iter n pow)
    (let ((value (floor (/ n pow))))
      (cons value
            (if (= pow 1)
                '()
                (iter (remainder n pow) (/ pow 10))))))
  (iter n (highest-power)))

(define (p4)
  (define (helper x y largest)
    (if (= x 900)
        largest
        (if (= y 900)
            (helper (- x 1) 999 largest)
            (let ((test (* x y)))
              (helper x (- y 1)
                      (if (and (> test largest) (palindrome? test)) test largest))))))
  (helper 999 999 0))
(define (3dx3d-all-palindromes)
  (filter palindrome? 
          (flatmap (lambda (x)
                     (map (lambda (y)
                            (* x y))
                          (enumerate-interval 100 999)))
                   (enumerate-interval 100 999))))


;; Problem 5
; What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?
(define (p5)
  (define (divides? a x)
    (= (remainder a x) 0))
  (define (helper a)
    (if (andmap (lambda (x)
                  (divides? a x))
                (enumerate-interval 1 20))
        a
        (helper (+ a 1))))
  (helper 2520))

;; Problem 6
; Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.
(define (p6)
  (- (square (apply + (enumerate-interval 1 100)))
     (apply + (map square (enumerate-interval 1 100)))))