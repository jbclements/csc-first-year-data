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
(plot (discrete-histogram (major-entry-stats "SE")))

;; ONE-OFF SCRIPT:

(query-exec
 conn
(~a
"CREATE TEMPORARY VIEW passed_308"
"  AS SELECT id FROM course_grade WHERE course='csc308' GROUP BY id;"))

(query-rows
 conn
"SELECT entry_qtrs.id,min FROM (entry_qtrs INNER JOIN majors ON entry_qtrs.id = majors.id)
  WHERE entry_qtrs.id NOT IN (SELECT id FROM passed_308)
   AND major = 'SE'
   AND min < 2158;")
