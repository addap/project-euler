(load "auxiliaries.scm")

(define (sum-of-prop-divs n)
  (accumulate + 0 (proper-divisors n)))
(define (proper-divisors n)
  (define (helper counter)
    (cond ((> counter (/ n 2)) '())
	  ((= (remainder n counter) 0) (cons counter (helper (+ counter 1))))
	  (else (helper (+ counter 1)))))
  (helper 1))

(define (amicable? n)
  (let ((sopd (sum-of-prop-divs n)))
    (and (not (= n sopd))
	 (= n (sum-of-prop-divs sopd)))))

(define (p21)
  (accumulate + 0
	      (filter amicable? (enumerate-interval 1 9999))))

