#lang racket

(require sqlite-table
         "student-data.rkt"
         "grades.rkt")

(printf "population: students starting 2058 through 2148 inclusive\n")

(printf "# of students: ~v\n"
        (length (in-table-column pre-2152-grade-table 'student)))

(define repeaters
  (remove-duplicates
   (map
    (vref 0)
    (filter
     (λ (v) (< 1 (vector-ref v 1)))
     (table-select pre-2152-grade-table '(student (count))
                   #:group-by '(student class))))))

(printf "repeaters: ~v\n" (length repeaters))


(define 103-passers
  (map
   (vref 0)
   (filter
    (λ (v) (member (vector-ref v 1) success-grades))
    (table-select pre-2152-grade-table '(student grade)
                  #:where '((= class "CPE 103"))))))

(printf "103 passers: ~v\n" (length 103-passers))



(printf "intersection: ~v\n"
        (set-count
         (set-intersect (list->set 103-passers)
                        (list->set repeaters))))
