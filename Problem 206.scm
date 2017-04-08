(define (fold-left op initial items)
  (define (loop result rest)
    (if (null? rest)
        result
        (loop (op result (car rest))
              (cdr rest))))
  (loop initial items))

(define (list->num list)
  (fold-left (lambda (value digit)
                     (+ (* value 10) digit))
             0
             list))

