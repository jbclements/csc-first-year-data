#lang racket

;; does the choice of first instructor affect the outcome from the
;; first time a student takes 103?

(require csv-reading
         "student-data.rkt"
         "table-sqlite.rkt")

(define lines
  (call-with-input-file "/Users/clements/clements/datasets/cpe-123-rosters.csv"
    csv->list))

(first lines)


;; given a number represented as a string, pad with leading
;; zeros to get to length 10
(define (pad-to-10 s)
  (define diff (- 10 (string-length s)))
  (cond [(<= 0 diff)
         (string-append (apply string (for/list ([i (in-range diff)]) #\0))
                        s)]
        [else
         (error 'pad-to-10
                "string of length <= 10"
                0 s)]))

(define student-table
  (make-table '(instructor student)
   (for/list ([l (in-list (rest lines))])
     (vector (list-ref l 6)
           (pad-to-10 (list-ref l 8))))
   ;#:permanent "123_instructor"
   ))


#;(make-table-from-select grade-facts-table '(student (min qtr))
                        #:where '((= class "CPE 103"))
                        #:group-by '(student)
                        #:permanent "first_time_103")
#;(natural-join first-time-103s grade-facts-table #:permanent "ft_103_grades" )

(define first-time-103s
  (find-table "ft_103_grades"))

(define results
  (table-select first-time-103s '(student grade)))

(take (sequence->list results) 10)

(define 123-takers
  (sequence->list
   (table-select grade-facts-table '(student (min qtr))
                 #:where '((= class "CPE 123"))
                 #:group-by '(student))))


