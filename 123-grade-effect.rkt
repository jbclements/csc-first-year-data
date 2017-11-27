#lang racket


;;...oog, giving up now?
;; extracting pass-fail data to run a chi-squared test in R
(require "grades.rkt"
         racket/runtime-path)

(require csv-reading)


;; find the index of an element in a list
(define (find-pos elt l)
  (let loop ([remaining l] [i : Natural 0])
    (cond [(empty? remaining) (error 'find-pos "couldn't find element")]
          [else (cond [(equal? (first remaining) elt) i]
                      [else (loop (cdr remaining) (add1 i))])])))

;; strip a leading byte order mark from an input port
(: discard-bom (Input-Port -> Void))
(define (discard-bom p)
  (void (regexp-try-match #rx"^\uFEFF" p)))

(define-runtime-path HERE ".")

;; build the female 123 student database
#;(: cells (Listof (Listof String)))
#;(define cells
  (call-with-input-file (build-path HERE "female-123-students-clean.csv")
    (λ (port)
      (csv->list port))))

  

;; rebuild the student databases

;; a list of lists representing the cells in the csv file
(: cells (Listof (Listof String)))
(define cells
  (call-with-input-file "/Users/clements/clements/datasets/student-data-2052-2174.csv"
    (λ (port)
      (csv->list port))))

;; the first row (presumably column labels)
(: label-row (Listof String))
(define label-row (first cells))

;; the rest of the rows
(: non-label-rows (Listof (Listof String)))
(define non-label-rows (rest cells))

(define idx-tmp 10)
(equal? (take label-row idx-tmp)
        (take '("FIRST_NAME"
           "LAST_NAME"
           "EMPLID"
           "FIRST_TERM"
           "FIRST_MAJOR"
           "FIRST_CLASS_LEVEL"
           "LAST_TERM"
           "LAST_MAJOR"
           "LAST_TERM"
           "LAST_CLASS_LEVEL"
           "TERM_CODE"
           "CPE_CLASS"
           "GRADE"
           "TERM_CODE"
           "CPE_CLASS"
           "GRADE"
           "TERM_CODE"
           "CPE_CLASS"
           "GRADE"
           "TERM_CODE"
           "CPE_CLASS"
           "GRADE"
           "TEST"
           "SCORE")
              idx-tmp ))
;; sanity check on the labels:
;; yikes... just insist that the labels be these:
(unless (equal?
         label-row
         '("FIRST_NAME"
  "LAST_NAME"
  "EMPLID"
  "FIRST_TERM"
  "FIRST_MAJOR"
  "FIRST_CLASS_LEVEL"
  "LAST_TERM"
  "LAST_MAJOR"
  "LAST_TERM"
  "LAST_CLASS_LEVEL"
  "TERM_CODE"
  "CPE_CLASS"
  "GRADE"
  "TEST"
  "SCORE")
         #;'("FIRST_NAME"
           "LAST_NAME"
           "EMPLID"
           "FIRST_TERM"
           "FIRST_MAJOR"
           "FIRST_CLASS_LEVEL"
           "LAST_TERM"
           "LAST_MAJOR"
           "LAST_TERM"
           "LAST_CLASS_LEVEL"
           "TERM_CODE"
           "CPE_CLASS"
           "GRADE"
           "TERM_CODE"
           "CPE_CLASS"
           "GRADE"
           "TERM_CODE"
           "CPE_CLASS"
           "GRADE"
           "TERM_CODE"
           "CPE_CLASS"
           "GRADE"
           "TEST"
           "SCORE"))
  (error 'label-row "unexpected label row, please check code below."))

