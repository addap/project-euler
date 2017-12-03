(load "auxiliaries.scm")
(load "math-aux.scm")

(define (p30)
  (apply + (filter-or-apply (lambda (n) (sum-of-powers 5 n))
			    (enumerate-interval 2 100000000))))

(define (sum-of-powers power number)
  (let ((digits (digitize number)))
    (if (= number
	   (apply + (map (lambda (d)
			   (expt d power))
			 digits)))
	number
	#f)))
	   

(define (filter-or-apply proc list)
  (if (null? list)
      '()
      (if (proc (car list))
	  (cons (proc (car list)) (filter-or-apply proc (cdr list)))
	  (filter-or-apply proc (cdr list)))))
