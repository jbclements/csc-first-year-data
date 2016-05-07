#lang racket

(require "table-sqlite.rkt"
         "student-data.rkt"
         "grades.rkt")


(define repeaters
  (remove-duplicates
   (map
    (vref 0)
    (filter
     (λ (v) (< 1 (vector-ref v 1)))
     (table-select grade-facts-table '(student (count))
                   #:group-by '(student class))))))


(define 103-passers
  (map
   (vref 0)
   (filter
    (λ (v) (member (vector-ref v 1) success-grades))
    (table-select grade-facts-table '(student grade)
                  #:where '((= class "CPE 103"))))))


(set-count
 (set-intersect (list->set 103-passers)
                (list->set repeaters)))

