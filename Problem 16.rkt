#lang racket

(require "math-aux.rkt")

;; Problem 16
; What is the sum of the digits of the number 2^1000?

(define (p16)
  (sum-of-digits (expt 2 1000)))