(load "auxiliaries.scm")

(define (p28)
  (- (accumulate + 0
		 (map (lambda (n)
			(* 4 (+ 1 (* 5 n) (* (max (/ (- (square n) n) 2)
						  0) 8))))
		      (enumerate-interval 0 500)))
     3))

;; First observe that we can average the four corners like so
;; 1 + 3 + 5 + 7 + 9 + 13 + 17 + 21 + 25 + ...
;; 1 + (4 * 6) + (4 * 19) + ...
;; So we will focus on the sequence formed by the elements starting with 1 and going left.
;; The sum of the dagonals is then 4 times the Series at (rounddown (/ length 2)) - 3 (because we count the 1 only once)

;; Apparently one can find the sequence formula by seeing that the upper right corner is (2n+1)² the other one are multiples of n smaller, averagig these (same as I did) and you have you sequence formula.
;; Then finding the differentials of the sequence. Gets constant at d3, so you need a third order polynomial (insert values and solve for coefficients) to get the closed form of the series.
