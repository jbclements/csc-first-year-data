#lang racket

;; trying to answer Zoe's questions about 
(require db
         "make-connection.rkt")

(define conn (make-connection))

(define 2158-regular-track
  (query-rows
   conn
   (~a "SELECT a.student"
       " FROM (class_grade a"
       "       INNER JOIN"
       "       class_grade b"
       "       ON a.student = b.student)"
       " WHERE a.subject = 'CPE'"
       " AND a.num = '123'"
       " AND a.qtr = 2158"
       " AND b.subject = 'CPE'"
       " AND b.num = '102'"
       " AND b.qtr = 2164")))