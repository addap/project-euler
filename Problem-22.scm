(load "auxiliaries.scm")
(define names.txt (open-input-file "p022_names.txt"))
(read-char names.txt)
(define (list-of-names names-port result)
  (let* ((delimiter (char-set #\"))
	 (name (read-string delimiter names-port)))
    (if (eof-object? name)	    
	result
	(begin
	  (read-char names.txt)	
	  (read-char names.txt)
	  (read-char names.txt)
	  (list-of-names names-port (cons name result))))))
(define sorted-names-list
  (sort (list-of-names names.txt '()) string<?))
(define (sum-of-chars str)
  (accumulate + 0 (map (lambda (char) (- (char->integer char) 96))
		       (string->list (string-downcase str)))))
(define (p22)
  (accumulate + 0
	      (map (lambda (name-value place-value) (* name-value place-value))
		   (map sum-of-chars sorted-names-list)
		   (enumerate-interval 1 (length sorted-names-list)))))
			   
