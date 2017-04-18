#lang racket

;; computing retention for students taking 123 in 2118 through 2148

(require "student-data.rkt"
         "table-sqlite.rkt"
         "grades.rkt")

(define all-123-grades
  (table-select grade-facts-table '(student qtr grade)
                #:where '((= class "CPE 123")
                          (< 2116 qtr)
                          (< qtr 2158))))

(define female-ids
  (for/hash ([id (file->lines "female-123-2118-2158.csv")])
    (values id #t)))

(unless
    (=
     (length (table-select grade-facts-table '(student qtr grade)
                           #:where '((= class "CPE 123")
                                     (< 2116 qtr)
                                     (< qtr 2158))))
     (length (table-select grade-facts-table '(student)
                           #:where '((= class "CPE 123")
                                     (< 2116 qtr)
                                     (< qtr 2158))
                           #:group-by '(student))))
  (error 'student-took-123-twice???))

(define all-passing-102-grades
  (group-by
   (λ (v) (vector-ref v 0))
   (filter
    (λ (v) (member (vector-ref v 2) success-grades))
    (table-select grade-facts-table '(student qtr grade)
                  #:where '((= class "CPE 102")
                            (< 2116 qtr))))))

(define passing-102-grade-qtr-lookup
  (for/hash ([group (in-list all-passing-102-grades)])
    (values (vector-ref (first group) 0)
            (map (λ (v) (vector-ref v 1)) group))))

(define combined
  (for/list ([123-result (in-list all-123-grades)])
    (define id (vector-ref 123-result 0))
    (list id
          (vector-ref 123-result 1)
          (hash-ref passing-102-grade-qtr-lookup id '()))))

(define (retained? r)
  (define 123-qtr (second r))
  (not (empty? (filter (λ (qtr) (<= qtr (qtr-incr (+ 10 123-qtr))))
                       (third r)))))

(exact->inexact
 (/ (length (filter retained? combined))
   (length combined)))

(define (female? c)
  (hash-ref female-ids (first c) #f))

(exact->inexact
 (/ (length (filter retained? (filter female? combined)))
   (length (filter female? combined))))

(length (filter retained? (filter female? combined)))
   (length (filter female? combined))

(exact->inexact
 (/ (length (filter retained? (filter (compose not female?) combined)))
   (length (filter (compose not female?) combined))))

(length (filter retained? (filter (compose not female?) combined)))
   (length (filter (compose not female?) combined))


