#lang racket

(require csv-reading
         "../emplid-hasher.rkt")

(map emplid->hashed
(map tenth
     (call-with-input-file "/tmp/Class Listing.csv"
       csv->list)))