#lang racket

(require csv-reading)


(define d
  (map (λ (row) (append (take row 2) (drop row 5)))
       (call-with-input-file
           "/Users/clements/clements/datasets/ad-hoc-student-progress/student-progress-data.csv" csv->list)))

(call-with-output-file "/tmp/foo.csv"
  #:exists 'truncate
  (λ (port)
    (for ([row (in-list d)])
      (displayln (apply string-append (add-between (map ~a row) ","))
                 port))))