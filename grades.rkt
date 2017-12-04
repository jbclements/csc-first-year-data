#lang typed/racket

(provide grade-ordering
         class-ordering
         success-grades
         vref
         qtr-incr)


;; shared knowledge about grades.
;; and classes.
;; and a nice abstraction (uh oh).

;; grades that allow you to take the next class in the first year:
(: success-grades (Listof String))
(define success-grades '("A" "A-" "B+" "B" "B-" "C+" "C" "C-"))

;; grade ordering, best to worst
(: grade-ordering (Listof String))
(define grade-ordering
  (reverse
  '("W" "WU" "AU" "U" "NC" "I" "F" "D-" "D" "D+" "C-" "C" "C+" "B-" "B" "B+" "A-" "A")))

;; class ordering, first to last
(: class-ordering (Listof String))
(define class-ordering
  '("CPE 123" "CPE 101" "CPE 102" "CPE 103"))

(: vref (All (T) (Integer -> ((Vectorof T) -> T))))
(define (vref idx)
  (λ ([v : (Vectorof T)]) (vector-ref v idx)))

;; given a quarter, go to the "next" quarter, skipping
;; the nonexistent 0
(: qtr-incr (Natural -> Natural))
(define (qtr-incr qtr)
  (define incr
    (match (modulo qtr 10)
      [2 2]
      [4 2]
      [6 2]
      [8 4]))
  (+ qtr incr))