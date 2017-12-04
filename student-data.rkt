#lang typed/racket

;; this table provides all of the student data tables without recomputing them

(provide (all-defined-out))

(require "grades.rkt")

(require/typed sqlite-table
               [#:opaque Table table?]
               [find-table (String -> Table)])


(define ap-facts-table
  (find-table "ap_scores"))

;; columns: '(student qtr class grade)
(define grade-facts-table
  (find-table "grades"))

(define student-first-qtr-table
  (find-table "student_first_qtr"))

#;(define pre-2152-grade-table
  (find-table "pre_2152_grades"))

(define pre-123-grade-table
  (find-table "pre_123_grades"))

(define post-123-grade-table
  (find-table "post_123_grades"))

;; first last emplid
(define names-table
  (find-table "names"))

;; instructor student 123_qtr
(define student-123-instructors
  (find-table "student_instructors_123"))



;(table-select grade-facts-table )

