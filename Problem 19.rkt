;(require "math-aux.rkt")

;; Problem 19
; How many Sundays fell on the first of the month during the twentieth century (1 Jan 1901 to 31 Dec 2000)?

; Wochentagsformel
; d: Tagesdatum 1-31
; m: Monat nach Tabelle (März=1-Februar=12)
; y: zwei letzten Stellen der Jahreszahl, bei Januar&Februar die letzten Stellen des Vorjahres
; c: zwei ersten Stellen der Jahreszahl, bei Januar&Februar die ersten Stellen des Vorjahres
; w: Wochentag (Sonntag=0-Samstag=6)
; Wochentagsformel:
#|
w = (remainder (+ d
                  (floor (- (* 2.6 m)
                            0.2))
                  y
                  (floor (/ y 4))
                  (floor (/ c 4))
                  (- (* 2 c)))
               7)
|#
; if w < 0 -> w += 7
(define (p19)
  (define (iter day month year result)
    (if (>= year 2001)
        result
        (iter 1
              (if (>= month 12) 1 (+ month 1))
              (if (>= month 12) (+ year 1) year)
              (if (= (wochentagsformel day month year) 0)
                  (+ result 1)
                  result))))
  (iter 1 1 1901 0))

(define (wochentagsformel day month year)
  (define (month-table month)
    (+ (remainder (+ month 9) 12) 1))
  (define (get-c year month)
    (if (or (= month 1) (= month 2))
        (get-c (- year 1) 3)
        (subnumber year 0 2)))
  (define (get-y year month)
    (if (or (= month 1) (= month 2))
        (get-y (- year 1) 3)
        (subnumber year 2)))
  (let ((d day)
        (m (month-table month))
        (y (get-y year month))
        (c (get-c year month)))
    (let ((w (remainder (+ d
                          (floor (- (* 2.6 m)
                                    0.2))
                          y
                          (floor (/ y 4))
                          (floor (/ c 4))
                          (- (* 2 c)))
                       7)))
      (if (< w 0)
          (+ w 7)
          w))))
    
        
           
       
