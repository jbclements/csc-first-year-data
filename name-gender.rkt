#lang racket

(require "table-sqlite.rkt"
         "grades.rkt")

(define name-table
  (find-table "names"))

(define names
  (sort
   (table-select name-table '(first (count)) #:group-by '(first))
   >
   #:key (vref 1)))

(table-select name-table '(first last) #:where '((= first "Casey")))