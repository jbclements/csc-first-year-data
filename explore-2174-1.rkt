#lang racket

;; trying to answer Zoe's questions about 
(require db
         "make-connection.rkt")

(define conn (make-connection))

(define started-in-2158
  (query-rows
   conn
   (~a "SELECT id"
       " FROM class_grade"
       " WHERE subject = 'CPE'"
       " AND num = '123'"
       " AND qtr = 2158"
       ";")))

(define 2158-regular-track
  (query-rows
   conn
   (~a "SELECT a.id"
       " FROM (class_grade a"
       "       INNER JOIN"
       "       class_grade b"
       "       ON a.id = b.id)"
       " WHERE a.subject = 'CPE'"
       " AND a.num = '123'"
       " AND a.qtr = 2158"
       " AND b.subject = 'CPE'"
       " AND b.num = '102'"
       " AND b.qtr = 2164")))

(define 2158-103-takers
  (query-rows
   conn
   (~a "SELECT a.id"
       " FROM (class_grade a"
       "       INNER JOIN"
       "       class_grade b"
       "       ON a.id = b.id)"
       " WHERE a.subject = 'CPE'"
       " AND a.num = '123'"
       " AND a.qtr = 2158"
       " AND b.subject = 'CPE'"
       " AND b.num = '103'"
       " AND b.qtr = 2164")))

