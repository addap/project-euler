;#lang racket
;(provide enumerate-interval)
;(provide square)
;(provide flatmap)
;(provide accumulate)

(define (enumerate-interval a b)
  (if (> a b)
      '()
      (cons a (enumerate-interval (+ a 1) b))))
(define (square x) (* x x))
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flatmap proc set)
  (accumulate append '() (map proc set)))
