; returns the decimal expansion of 1/d as a list
(define (decimal-expansion d)
  ; only add a 0 if switch is true, then set switch to false
  (define (helper n result switch)
    (let* ((int-div (integer-divide n d))
	   (div (car int-div))
	   (rem (cdr int-div)))
      (cond ((= rem 0) (cons div result))
	    ((= div 0) (helper (* 10 n) (cons 0 result)))
	    (else (helper (- n (* d div)) (cons div result))))))
  (helper 1 '()))
