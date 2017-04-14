(load "Problem 21.scm")
(load "auxiliaries.scm")

(define (abundant? n)
  (> (sum-of-prop-divs n) n))
; list of abundant numbers <= 28123
(define abdt-<=-28123
  (filter abundant? (enumerate-interval 1 28123)))
; list of all numbers that can be written as the sum of two numbers
(define (possible-sums numbers)
  (define (iter-1 set1 set2 result)
    (define (iter-2 elm1 set2_ result)
      (if (null? set2_)
	  result
	  (let ((new-sum (+ elm1 (car set2_))))
	    (if (or (>= new-sum 28123)
		    (memq new-sum result))
		(iter-2 elm1 (cdr set2_) result)
		(iter-2 elm1 (cdr set2_) (cons new-sum result))))))
    (if (null? set1)
	result
	(iter-1 (cdr set1) (cdr set2)
		(iter-2 (car set1) set2 result))))
  (iter-1 numbers numbers '()))
(define (sum-of-abundants? n set)
  (cond ((or (null? set) (> (car set) (/ n 2))) #f)
	((memq (- n (car set)) set) #t)
	(else (sum-of-abundants? n (cdr set)))))
	 
(define (p23)
  (accumulate + 0
	      (remove (lambda (number)
			(sum-of-abundants? number abdt-<=-28123))
		      (enumerate-interval 1 28123))))
