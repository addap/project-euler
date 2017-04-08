(define (factorial n)
  (define (iter n result)
    (if (<= n 1)
	result
	(iter (- n 1) (* n result))))
  (iter n 1))

(define (helper counter target result digits)
  (if (= target 1)
      (append (reverse result) digits)
      (let ((block-size (factorial (- (length digits) 1))))
	(if (and (<= (* counter block-size)
		     target)
		 (<= target
		     (* (+ counter 1) block-size)))
	    (let ((correct-digit (list-ref digits counter)))
	      (delete! correct-digit digits)
	      (helper 0 (- target (* counter block-size)) (cons correct-digit result) digits))
	    (helper (+ counter 1) target result digits)))))
(define (find-nth-permutation digits n)
  (helper 0 n '() digits))

(define (p24)
  (find-nth-permutation '(0 1 2 3 4 5 6 7 8 9) 1000000))
