(load "auxiliaries.rkt")

;; Problem 18
; Find the maximum total from top to bottom of the triangle below:
#|
; Convert to List
75
95 64
17 47 82
18 35 87 10
20 04 82 47 65
19 01 23 75 03 34
88 02 77 73 07 63 67
99 65 04 28 06 16 70 92
41 41 26 56 83 40 80 70 33
41 48 72 33 47 32 37 16 94 29
53 71 44 65 25 43 91 52 97 51 14
70 11 33 28 77 73 17 78 39 68 17 57
91 71 52 38 17 14 91 43 58 50 27 29 48
63 66 04 68 89 53 67 30 73 16 69 87 40 31
04 62 98 27 23 09 70 98 73 93 38 53 60 04 23
|#

(define grid
  '(75
    95 64
    17 47 82
    18 35 87 10
    20 04 82 47 65
    19 01 23 75 03 34
    88 02 77 73 07 63 67
    99 65 04 28 06 16 70 92
    41 41 26 56 83 40 80 70 33
    41 48 72 33 47 32 37 16 94 29
    53 71 44 65 25 43 91 52 97 51 14
    70 11 33 28 77 73 17 78 39 68 17 57
    91 71 52 38 17 14 91 43 58 50 27 29 48
    63 66 04 68 89 53 67 30 73 16 69 87 40 31
    04 62 98 27 23 09 70 98 73 93 38 53 60 04 23))

; keep track of depth, start at depth 1, each node can reach the nodes at
; indices (+ self_index (triangle-number depth) [1])
; put neighbors in open list, according to their movement-cost+heuristic
; heuristic could be (- max_depth depth)
; read up on A* on Wikipedia
; if I use A* I need to specify an end node, so I will have to do max_depth searches
; I can't really give it a good heuristic, the numbers are more or less random
; If I'd do a heuristic like above, it would be too optimistic and A* couldn't leave out any nodes as it would never find a shorter path
; Djikstra's would be the better choice I think, but would it scale to Problem 60?
; Doesn't matter, it said this problem could still be brute-forced, just do Djikstra
;
; First convert the list to an appropriate graph, or just work with indices
; takes in a list with (triangle-number n) items and produces one with n sublists.
; The sublists have 1, 2, 3, ... items each

; New: convert list to tree, pass to breadth-first search
; breadth-first search, dequeues first element of open-queue
; looks at cost-so-far, if c-s-f + entry > current-value
; set current-value and add children
(define (sub-list list start length)
  (if (= length 0)
      '()
      (if (= start 0)
	  (cons (car list) (sub-list (cdr list) start (- length 1)))
	  (sub-list (cdr list) (- start 1) length))))

(define (triangle-list input-list rows)
  (define (helper input-list current-row)
    (if (< rows current-row)
	'()
	(cons (sub-list input-list (- current-row 1) current-row)
	      (helper (list-ref input-list current-row) (+ current-row 1)))))
  (helper input-list 1))

(define (breadth-first graph)
  (let ((open-list (list (car graph))))
    ))

(define (find-max-to-add set)
  (define (helper set last)
    (if (null? set)
	(cons (list last) '())
	(cons (list last (car set))
	      (helper (cdr set) (car set)))))
  (map (lambda (x) (apply max x))
       (cons (list (car set))
	     (helper (cdr set) (car set)))))

      
	
