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
;; zeros to get to length 9
(define (pad-to-9 s)
  (define diff (- 9 (string-length s)))
  (cond [(<= 0 diff)
         (string-append (apply string (for/list ([i (in-range diff)]) #\0))
                        s)]
        [else
         (error 'pad-to-9
                "string of length <= 9"
                0 s)]))

(define (qtr-text->qtr str)
  (match str
    [(regexp #px"^Fall Quarter ([[:digit:]]+)$" (list _ y))
     (+ 2008 (* 10 (modulo (string->number y) 100)))]))

(define student-123-instructors
  (make-table '(instructor student 123_qtr)
   (for/list ([l (in-list (rest lines))])
     (vector (list-ref l 6)
             (pad-to-9 (list-ref l 8))
             (qtr-text->qtr (list-ref l 0))))
   #:permanent "student_instructors_123"
   #:use-existing #t
   ))



(define first-time-102s
  (make-table '(student qtr)
              (table-select grade-facts-table '(student (min qtr))
                            #:where '((= class "CPE 102"))
                            #:group-by '(student))
              #:permanent "first_time_102"
              #:use-existing #t))

(define first-time-102-grades
  (inner-join first-time-102s
              grade-facts-table
              '(student qtr)
              #:permanent "ft_102_grades"
              #:use-existing #t))



(define pre-2158-123-instructors
  (make-table-from-select
   student-123-instructors '(student instructor)
   #:where '((< 123_qtr 2158))
   #:permanent "pre_2158_123_instructors"
   #:use-existing #t))

(define instructor-grades-102
  (natural-join first-time-102-grades
                pre-2158-123-instructors
                #:permanent "instructor_grades_102"
                #:use-existing #t))


(define results
  (table-select instructor-grades-102 '(instructor grade)))

(sequence-length results)
#;(take (sequence->list results) 10)

(with-output-to-file "/tmp/gg.txt"
  (λ ()
    (for ([v results])
      (apply printf "~v, ~v\n" (vector->list v)))))




