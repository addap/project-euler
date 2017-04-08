;#lang racket
;(provide enumerate-interval)
;(provide square)
;(provide flatmap)
;(provide accumulate)

(define (enumerate-interval a b)
  (define (helper b_ result)
    (if (< b_ a)
      result
      (helper (- b_ 1) (cons b_ result))))
  (helper b '()))
(define (square x) (* x x))
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flatmap proc set)
  (accumulate append '() (map proc set)))

(define (timed-procedure-call proc)
  (let ((start-time (runtime)))
    (proc)
    (display "*** ") (display (- (runtime) start-time))))

(define (cross-product set1 set2)
  (flatmap (lambda (elm1)
	     (map (lambda (elm2)
		    (list elm1 elm2))
		  set2))
	   set1))
