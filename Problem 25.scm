(define (fib n)
  (define (fib-iter a b p q count)
    (cond ((= count 0) b)
	  ((even? count)
	   (fib-iter a b
		     (+ (* p p) (* q q))
		     (+ (* 2 p q) (* q q))
		     (/ count 2)))
	  (else (fib-iter (+ (* b q) (* a q) (* a p))
			  (+ (* b p) (* a q))
			  p
			  q
			  (- count 1)))))
  (fib-iter 1 0 0 1 n))
(define (p25)
  (define (helper count)
    (if (>= (string-length
	     (number->string (fib count)))
	    1000)
	count
	(helper (+ count 1))))
  (helper 1))
