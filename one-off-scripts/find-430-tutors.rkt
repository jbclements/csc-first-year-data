#lang racket


(require db
         csv-reading)

(define conn1
    (postgresql-connect #:port 13432
                        #:user "grader"
                        #:password "aoeuidht"
                        #:database "grading"))


(define good-grade-getters
  (query-list
   conn1
   (~a
    "SELECT student FROM finalgrades"
    " WHERE (grade='A' OR grade='A-' OR grade='B+')"
    " AND num='0430'"
    " AND qtr >= 2168;")))

(define student-progress-data
(call-with-input-file
    "/Users/clements/clements/datasets/ad-hoc-student-progress/student-progress-data.csv"
  csv->list))

(first student-progress-data)

(define emails
  (remove-duplicates (map fifth (rest student-progress-data))))

(define ids
  (for/list ([email (in-list emails)])
    (match email
      [(regexp #px"^([^@]+)@" (list _ id)) id])))

(for ([id (in-list (set-intersect good-grade-getters ids))])
  (display (~a id "@calpoly.edu, ")))

