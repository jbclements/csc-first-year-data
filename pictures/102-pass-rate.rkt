#lang racket

(require csse-scheduling/qtr-math)

(require csv-reading)

;; a list of lists representing the cells in the csv file
(define cells
  (call-with-input-file "/Users/clements/clements/datasets/student-data-2052-2174.csv"
    (λ (port)
      (csv->list port))))

(define labels (first cells))
(define rows (rest cells))

(define idx-lookup
  (for/hash ([label (in-list labels)]
             [i (in-naturals)])
    (values label i)))

(define ((col-ref label) row)
  (list-ref row (hash-ref idx-lookup label)))

(remove-duplicates (map (col-ref "CPE_CLASS") rows))

(remove-duplicates (map (col-ref "GRADE") rows))

(define good-grades (map symbol->string '(A A- B+ B B- C+ C C-)))

(define first-time-102
  (let ()
    (define only-102s
      (filter (λ (row)
                (and (equal? ((col-ref "CPE_CLASS") row) "CPE 102")
                     (>= (string->number ((col-ref "TERM_CODE") row))
                         2052)))
              rows))
    (define student-groups
      (filter (compose not empty?)
              (group-by (col-ref "EMPLID") only-102s)))
    (map (λ (stu) (argmin (compose string->number
                                   (col-ref "TERM_CODE")) stu))
         student-groups)))

(define filtered2
  (for/list ([row (in-list first-time-102)])
    (list ((col-ref "TERM_CODE") row)
          (and (member ((col-ref "GRADE") row) good-grades)
               #t))))

(define filtered3
  (for/list ([row (in-list first-time-102)])
    (list (qtr->fall-year (string->number ((col-ref "TERM_CODE") row)))
          (and (member ((col-ref "GRADE") row) good-grades)
               #t))))

(define (year-grouper str)
  (qtr->fall-year (string->number str)))

(sort
 (map
  (λ (g) (list (string->number (first (first g)))
               (length g)
               (exact->inexact (/ (length (filter second g))
                                  (length g)))))
  (group-by first filtered2))
 <
 #:key first)

(define by-year-pass-rates
  (sort
   (map
    (λ (g) (list (first (first g))
                 (length g)
                 (exact->inexact (/ (length (filter second g))
                                    (length g)))))
    (group-by first filtered3))
   <
   #:key first))

(require plot)

(plot
 (lines
  (sort
   (map
    (λ (g) (list (string->number (first (first g)))
                 (/ (length (filter second g))
                    (length g))))
    (group-by first filtered2))
   <
   #:key first))
 #:y-min 0
 #:y-max 1.0)

(plot ; -file
 (lines
  (sort
   (map
    (λ (pt) (list (first pt) (third pt)))
    by-year-pass-rates)
   <
   #:key first))
 #:y-min 0
 #:y-max 1.0
 #:y-label "pass rate"
 #:x-label "year"
 #:title "pass rate of CPE 102 by first-time-takers in the major by year"
 #;"/tmp/zz.svg")
