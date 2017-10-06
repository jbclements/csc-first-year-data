#lang racket

(require db/base
         "../make-connection.rkt")

(database-name "csseprogress")
(define conn (make-connection))

(define (major-entry-stats major)
  (query-rows
   conn
   (~a "SELECT min,COUNT(*) "
       " FROM entry_qtrs INNER JOIN majors ON entry_qtrs.id=majors.id"
       " WHERE major = $1 "
       " GROUP BY min"
       " ORDER BY min")
   major))

(major-entry-stats "SE")

(require plot)
(plot (discrete-histogram (major-entry-stats "SE")) #:title "SE")
(plot (discrete-histogram (major-entry-stats "CSC")) #:title "CSC")
(plot (discrete-histogram (major-entry-stats "CPE")) #:title "CPE")

;; ONE-OFF SCRIPT:

(query-exec
 conn
(~a
"CREATE TEMPORARY VIEW passed_453"
"  AS SELECT id FROM course_grade WHERE course='csc453' GROUP BY id;"))

(length
 (query-rows
 conn
 (~a "SELECT entry_qtrs.id FROM (entry_qtrs INNER JOIN majors ON entry_qtrs.id = majors.id)
  WHERE entry_qtrs.id NOT IN (SELECT id FROM passed_453)
   AND (major = 'CSC' OR major='CPE')
   AND min <= 2148"
     " GROUP BY entry_qtrs.id"
     ";")))
