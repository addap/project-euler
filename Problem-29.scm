(load "auxiliaries.scm")
(load "math-aux.scm")

(define upper-bound 100)

(define (p29)
  (length
   (remove-repeats (map append-primes-power (cross-product (prime-factorization (enumerate-interval 2 upper-bound)) 
					      (enumerate-interval 2 upper-bound)))
		   '()))
  )

;; cannot use just gcd
;; e.g. 3^6 = 9^3 = 27^2 but (repeats? 9 3) => #f
;; have to test against all divisors of power
;; OR use prime-factorization and remove all pfs that are equal?
;; OR just calculate each and remove same
(define (remove-repeats list result)
  (if (null? list)
      result
      (if (memq (car list) result)
	  (remove-repeats (cdr list) result)
	  (remove-repeats (cdr list) (cons (car list) result)))))
