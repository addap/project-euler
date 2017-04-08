#lang racket

(require "auxiliaries.rkt")

;; Problem 14
; Which starting number, under one million, produces the longest chain?

; a collatz chain is a start value cons'd to a collatz chain whose start value results from the equations.
; n -> (/ n 2) | n is even
; n -> (+ (* 3 n) 1) | n is odd
(define (collatz-next n)
  (if (even? n)
      (/ n 2)
      (+ (* 3 n) 1)))
(define (make-chain start)
  (if (= start 1)
      (cons 1 '())
      (cons start (make-chain (collatz-next start)))))
(define (longest-chain thresh)
  (apply max (map length
                  (map make-chain (enumerate-interval 1 thresh)))))

(define (p14)
  (define (iter x result longest)
    (if (>= x 999999)
        result
        (let ((len (length (make-chain x))))
          (if (> len longest)
              (iter (+ x 1) x len)
              (iter (+ x 1) result longest)))))
  (iter 1 0 0))
  