#lang racket

(require csv-reading
         "../make-connection.rkt"
         "../emplid-hasher.rkt"
         db)

(database-name "csseprogress")
(define conn (make-connection))

(define ids
  (map emplid->hashed
       (map tenth
            (rest
             (call-with-input-file "/tmp/Class Listing.csv"
               csv->list)))))

(define se-majors
  (map vector->list
  (query-rows
   conn
   "SELECT id,major FROM majors WHERE major='SE'")))

(define major-ids (list->set (map first se-majors)))

(length
 (filter (λ (id)
           (set-member? major-ids id))
         ids))

