#lang racket

(require "conversions.rkt")
(require "auxiliaries.rkt")

;; Problem 17
; If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words, how many letters would be used?
(define (p17)
  (apply + (map string-length
                (map (lambda (number-list)
                       (apply string-append (translate number-list)))
                     (map num->list (enumerate-interval 1 1000))))))


; convert a digitized list into human words
; iterate through list, if x mod 3 is 0, look at prev digit eg 13 and skip next
; if x mod 3 is 1, append 0
; es gibt längen 1, 2, 3, 4 theoretisch unendlich
; (1) -> "one"
; (1 2) -> "twelve"
; (1 2 3) -> "one hundred and twenty three"
; (1 2 3 4) -> "one thousand two hundred and thirty four"
; man muss also erkennen ob man in einer einer-, zehner-, hunderter-position ist (tausender verhalten sich wieder wie einer)
; dh x: aktuelle position (start bei 0), len: gesamtlänge, pos: Position
; pos = (len - x) mod 3
; einer/tausender: pos = 1
; zehner: pos = 2
; hunderter: pos = 0
(define (translate seq)
  (define (iter x len and-flag)
    (if (equal? seq '(1 0 0 0))
        (list (numbers 1000))
        (if (>= x len) ; exit condition
            '()
            (let ((pos (remainder (- len x) 3)) 
                  (value (list-ref seq x)))
              (if (not (= value 0))
                  (if and-flag
                      (append (list "and")
                              (iter x len #f))
                      (cond ((= pos 0) (append (list (numbers value) (numbers 100)) ; hunderter pos
                                               (iter (+ x 1) len #t)))
                            ((= pos 1) (append (list (numbers value)) ; einer/tausender pos
                                               (iter (+ x 1) len #f)))
                            ((= pos 2) (let* ((times-ten (* 10 value))  ; zehner pos
                                              (plus-next (+ times-ten (list-ref seq (+ x 1)))))
                                         (if (eq? (numbers plus-next) -1)
                                             (append (list (numbers times-ten))
                                                     (iter (+ x 1) len #f))
                                             (append (list (numbers plus-next))
                                                     (iter (+ x 2) len #f)))))
                            (else "Error, pos should be 0-2")))
                  (iter (+ x 1) len #t))))))                        
  (iter 0 (length seq) #f))

(define (numbers n)
  (cond ((= n 1) "one")
        ((= n 2) "two")
        ((= n 3) "three")
        ((= n 4) "four")
        ((= n 5) "five")
        ((= n 6) "six")
        ((= n 7) "seven")
        ((= n 8) "eight")
        ((= n 9) "nine")
        ((= n 10) "ten")
        ((= n 11) "eleven")
        ((= n 12) "twelve")
        ((= n 13) "thirteen")
        ((= n 14) "fourteen")
        ((= n 15) "fifteen")
        ((= n 16) "sixteen")
        ((= n 17) "seventeen")
        ((= n 18) "eighteen")
        ((= n 19) "nineteen")
        ((= n 20) "twenty")
        ((= n 30) "thirty")
        ((= n 40) "forty")
        ((= n 50) "fifty")
        ((= n 60) "sixty")
        ((= n 70) "seventy")
        ((= n 80) "eighty")
        ((= n 90) "ninety")
        ((= n 100) "hundred")
        ((= n 1000) "onethousand")
        (else -1)))