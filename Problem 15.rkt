#lang racket

;; Problem 15
; Starting in the top left corner of a 2×2 grid, and only being able to move to the right and down, there are exactly 6 routes to the bottom right corner.
; How many such routes are there through a 20×20 grid?

; You have to move to the right 20 times and down 20 times, no more, no less
; Im Urnenmodell dargestellt ergeben sich
; n = 40 Kugeln
; k1 = 20 Kugeln "rechts"
; k2 = 20 Kugeln "runter"
; Um die Permutationen zu berechnen benutzt man den Multinomialkoeffizienten, da sich elemente Wiederholen
; (/ (fact n) (* (fact k1) (fact k2)))

(define (p15)
  ; naive factorial implementation
  (define (factorial n)
    (if (= n 1)
        1
        (* n (factorial (- n 1)))))
  (/ (factorial 40)
     (* (factorial 20)
        (factorial 20))))
