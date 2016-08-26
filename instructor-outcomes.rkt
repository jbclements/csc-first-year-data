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

(define has-ap
  (make-table-from-select ap-facts-table '(student)
                          #:group-by '(student)
                          #:permanent "students_with_ap"
                          #:use-existing #t))

;; hack to extract students without ap:
(define no-ap-students
  (remove* (sequence->list
            (in-table-column has-ap 'student))
           (sequence->list
            (in-table-column grade-facts-table 'student))))

(define no-has-ap
  (make-table '(student)
              (map vector no-ap-students)
              #:permanent "students_without_ap"
              #:use-existing #t))

(define pre-2158-123-instructors
  (make-table-from-select
   student-123-instructors '(student instructor)
   #:where '((< 123_qtr 2158))
   #:permanent "pre_2158_123_instructors"
   #:use-existing #t))

(table-select pre-2158-123-instructors
              '(instructor (count))
              #:group-by '(instructor))

;; in order to count the students that were majors
;; but never made it to 102, we need a count of the students
;; 
(let ()
  (define majors-by-instructor
    (table-select
     (make-table-from-select (inner-join pre-2158-123-instructors
                                         grade-facts-table
                                         '(student)
                                         #:permanent "ijtemp5"
                                         #:use-existing #t)
                             '(instructor student)
                             #:group-by '(student)
                             #:permanent "ijtemp4"
                             #:use-existing #t)
     '(instructor (count))
     #:group-by '(instructor)))

  (with-output-to-file "/tmp/tot-students.txt"
    (λ ()
      (for ([v majors-by-instructor])
        (apply printf "~v, ~v\n" (vector->list v))))
    #:exists 'truncate))


(define pre-2158-123-instructors-with-ap
  (inner-join pre-2158-123-instructors
              has-ap
              '(student)
              #:permanent "pre_5_123_with_ap"
              #:use-existing #t))

(let ()
  (define majors-by-instructor
    (table-select
     (make-table-from-select (inner-join pre-2158-123-instructors-with-ap
                                         grade-facts-table
                                         '(student)
                                         #:permanent "ijtemp5c"
                                         #:use-existing #t)
                             '(instructor student)
                             #:group-by '(student)
                             #:permanent "ijtemp4c"
                             #:use-existing #t)
     '(instructor (count))
     #:group-by '(instructor)))

  (with-output-to-file "/tmp/tot-ap-students.txt"
    (λ ()
      (for ([v majors-by-instructor])
        (apply printf "~v, ~v\n" (vector->list v))))
    #:exists 'truncate))

(define pre-2158-123-instructors-without-ap
  (inner-join pre-2158-123-instructors
              no-has-ap
              '(student)
              #:permanent "pre_5_123_without_ap"
              #:use-existing #t))

(let ()
  (define majors-by-instructor
    (table-select
     (make-table-from-select (inner-join pre-2158-123-instructors-without-ap
                                         grade-facts-table
                                         '(student)
                                         #:permanent "ijtemp5b"
                                         #:use-existing #t)
                             '(instructor student)
                             #:group-by '(student)
                             #:permanent "ijtemp4b"
                             #:use-existing #t)
     '(instructor (count))
     #:group-by '(instructor)))

  (with-output-to-file "/tmp/tot-non-ap-students.txt"
    (λ ()
      (for ([v majors-by-instructor])
        (apply printf "~v, ~v\n" (vector->list v))))
    #:exists 'truncate))

(printf "# of students in population that have an ap score: ~v\n"
        (table-size pre-2158-123-instructors-with-ap))

(define instructor-grades-102
  (natural-join first-time-102-grades
                pre-2158-123-instructors
                #:permanent "instructor_grades_102"
                #:use-existing #t))

(define instructor-grades-102-with-ap
  (natural-join first-time-102-grades
                pre-2158-123-instructors-with-ap
                #:permanent "instructor_grades_102_with_ap"
                #:use-existing #t))

(define instructor-grades-102-without-ap
  (natural-join first-time-102-grades
                pre-2158-123-instructors-without-ap
                #:permanent "instructor_grades_102_without_ap"
                #:use-existing #t))





(let ()
  (define results
    (table-select instructor-grades-102 '(instructor grade (count))
                  #:group-by '(instructor grade)))

  (sequence-length results)
  #;(take (sequence->list results) 10)

  (with-output-to-file "/tmp/gg1.txt"
    (λ ()
      (for ([v results])
        (apply printf "~v, ~v, ~v\n" (vector->list v))))
    #:exists 'truncate))

(let ()
  (define results
    (table-select instructor-grades-102-with-ap '(instructor grade (count))
                  #:group-by '(instructor grade)))
  
  (sequence-length results)
  #;(take (sequence->list results) 10)

  (with-output-to-file "/tmp/gg2.txt"
    (λ ()
      (for ([v results])
        (apply printf "~v, ~v, ~v\n" (vector->list v))))
    #:exists 'truncate))

(let ()
  (define results
    (table-select instructor-grades-102-without-ap '(instructor grade (count))
                  #:group-by '(instructor grade)))

  (sequence-length results)
  #;(take (sequence->list results) 10)

  (with-output-to-file "/tmp/gg3.txt"
    (λ ()
      (for ([v results])
        (apply printf "~v, ~v, ~v\n" (vector->list v))))
    #:exists 'truncate))




