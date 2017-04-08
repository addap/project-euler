#lang racket

(#%require (only racket/base random))

(provide miller-rabin)
(provide miller-rabin23)
(provide miller-rabin-var)
(provide prime?)
(provide trial-division)

(define (prime? n . args)
  (define (dispatch args)
    (car args))
  (define (content args)
    (cadr args))
  (let ((dispatch (dispatch args)))
    (cond ((or (null? dispatch)
               (eq? dispatch 'trial-division))
           (trial-division n))
          ((eq? dispatch 'miller-rabin)
           (miller-rabin n))
          ((eq? dispatch 'miller-rabin-1.5mil)
           (miller-rabin23 n))
          ((eq? dispatch 'miller-rabin-var)
           (miller-rabin-var n
                             (if (content args)
                                 (content args)
                                 10)))
          (else (error "No prime search with dispatch" dispatch)))))

(define (trial-division n)
  (define (next x)
    (if (= x 2)
        3
        (+ x 2)))
  (define (iter x)
    (if (>= (square x) n)
        #t
        (if (= (remainder n x) 0)
            #f
            (iter (next x)))))
  (iter 2))

(define (miller-rabin n) 
  (miller-rabin-test (- n 1) n))

; works faster and reliable for about 1 million
(define (miller-rabin23 n)
  (if (or (= n 2) (= n 3))
      #t
      (miller-rabin-test2 n)))

(define (miller-rabin-var n tries)
  (define (try-it a counter)
    (if (>= counter tries)
        #t
        (if (= (expmod a (- n 1) n) 1)
            (try-it (+ (random (- n 1)) 1) (+ counter 1))
            #f)))
  (try-it 1 0))

(define (miller-rabin-test a n) 
  (cond ((= a 0) true) 
        ; expmod is congruent to 1 modulo n 
        ((= (expmod a (- n 1) n) 1) (miller-rabin-test (- a 1) n)) 
        (else false)))

(define (miller-rabin-test2 n) 
  (and (= (expmod 2 (- n 1) n) 1)
       (= (expmod 3 (- n 1) n) 1)))
       

(define (expmod base exp m) 
  (cond ((= exp 0) 1) 
        ((even? exp) 
         (let ((x (expmod base (/ exp 2) m))) 
           (if (non-trivial-sqrt? x m) 0 (remainder (square x) m)))) 
        (else 
         (remainder (* base (expmod base (- exp 1) m)) 
                    m))))
(define (square x) (* x x))

(define (non-trivial-sqrt? n m) 
  (cond ((= n 1) false) 
        ((= n (- m 1)) false) 
        ; book reads: whose square is equal to 1 modulo n 
        ; however, what was meant is square is congruent 1 modulo n 
        (else (= (remainder (square n) m) 1)))) 
