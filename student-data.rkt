#lang typed/racket

;; this table provides all of the student data tables without recomputing them

(provide (all-defined-out))

(require "grades.rkt")

(require/typed sqlite-table
               [#:opaque Table table?]
               [find-table (String -> Table)]
               [table-column-names (Table -> (Listof Symbol))])


;; what did students get on the AP exam?
;; columns: '(student score)
(define ap-facts-table
  (find-table "ap_scores"))

;; what grade did students get in this quarter taking this course?
;; columns: '(student qtr class grade)
(define grade-facts-table
  (find-table "grades"))

;; what's the first quarter in which a student took a course from us?
;; columns: '(student qtr)
(define student-first-qtr-table
  (find-table "student_first_qtr"))

#;(define pre-2152-grade-table
  (find-table "pre_2152_grades"))

;; a sub-table of grade-facts table, those who took a class before 2108
(define pre-123-grade-table
  (find-table "pre_123_grades"))

;; a sub-table of grade-facts table, those who didn't take a class before 2108
(define post-123-grade-table
  (find-table "post_123_grades"))

;; columns: '(first last emplid)
(define names-table
  (find-table "names"))

;; instructor student 123_qtr
;; columns: '(instructor student 123_qtr)
(define student-123-instructors
  (find-table "student_instructors_123"))



