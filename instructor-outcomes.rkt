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

(printf "students in the 123 list: ~v\n" (length (rest lines)))

(define major-student-123-instructors
  (make-table-from-select
   (inner-join student-123-instructors
               grade-facts-table
               '(student)
               #:permanent "57bb"
               #:use-existing #t)
   '(student instructor 123_qtr)
   #:group-by '(student instructor)
   #:permanent "57c0b455"
   #:use-existing #t))

(printf "students in the 123 list also in the grade list: ~v\n"
        (table-size major-student-123-instructors))

(printf "count by instructor of students in 123 in the major\n")
(table-select major-student-123-instructors '(instructor (count))
              #:group-by '(instructor))

(define first-time-101s
  (make-table '(student qtr)
              (table-select grade-facts-table '(student (min qtr))
                            #:where '((= class "CPE 101"))
                            #:group-by '(student))
              #:permanent "first_time_101"
              #:use-existing #t))

(define first-time-102s
  (make-table '(student qtr)
              (table-select grade-facts-table '(student (min qtr))
                            #:where '((= class "CPE 102"))
                            #:group-by '(student))
              #:permanent "first_time_102"
              #:use-existing #t))

(define first-time-101-grades
  (inner-join first-time-101s
              grade-facts-table
              '(student qtr)
              #:permanent "ft_101_grades"
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

;; students and instructors for majors in 2014,2015
(define post-2138-123-instructors
  (make-table-from-select
   major-student-123-instructors '(student instructor)
   #:where '((< 2138 123_qtr))
   #:permanent "post_2138_123_instructors57c0b456"
   #:use-existing #t))

(define pre-2158-123-instructors
  (make-table-from-select
   major-student-123-instructors '(student instructor)
   #:where '((< 123_qtr 2158)
             (< 2108 123_qtr))
   #:permanent "pre_2158_123_instructors57c0b456"
   #:use-existing #t))

(define post-2108-123-instructors
  (make-table-from-select
   major-student-123-instructors '(student instructor)
   #:where '((< 2108 123_qtr))
   #:permanent "post_2108_123_instructors57c0b456"
   #:use-existing #t))

(printf "count by instructor of students in 123 in the major in 2014 and 2015")
(table-select post-2138-123-instructors
              '(instructor (count))
              #:group-by '(instructor))

(printf "count by instructor of students in 123 in the major in 2011-2014")
(table-select pre-2158-123-instructors
              '(instructor (count))
              #:group-by '(instructor))

(printf "count by instructor of students in 123 in the major in 2011-2015")
(table-select post-2108-123-instructors
              '(instructor (count))
              #:group-by '(instructor))

;; in order to count the students that were majors
;; but never made it to 101, we need a count of the students
;;

(define (write-totals tempname source-table file)
  (define majors-by-instructor
    (table-select
     (make-table-from-select
      (inner-join source-table
                  grade-facts-table
                  '(student)
                  #:permanent (string-append tempname "a")
                  #:use-existing #t)
      '(instructor student)
      #:group-by '(student)
      #:permanent (string-append tempname "b")
      #:use-existing #t)
     '(instructor (count))
     #:group-by '(instructor)))
  (csv-write majors-by-instructor file))

;; write a list of vectors to a csv file
(define (csv-write vecs file)
  (with-output-to-file file
    (λ ()
      (for ([v vecs])
        (displayln
         (apply string-append
                (add-between (map ~v (vector->list v)) ",")))))
    #:exists 'truncate))

(write-totals "57c67a60" post-2138-123-instructors
              "/tmp/tot-students-2148+.txt")
(write-totals "57c67a61" pre-2158-123-instructors
              "/tmp/tot-students-2148-.txt")
(write-totals "57c67a62" post-2108-123-instructors
              "/tmp/tot-students-2118+.txt")


(define post-2138-123-instructors-with-ap
  (inner-join post-2138-123-instructors 
              has-ap
              '(student)
              #:permanent "post_3_123_with_ap"
              #:use-existing #t))

(define pre-2158-123-instructors-with-ap
  (inner-join pre-2158-123-instructors 
              has-ap
              '(student)
              #:permanent "pre_5_123_with_ap"
              #:use-existing #t))

(define post-2108-123-instructors-with-ap
  (inner-join post-2108-123-instructors 
              has-ap
              '(student)
              #:permanent "post_0_123_with_ap"
              #:use-existing #t))

(write-totals "57c66792" post-2138-123-instructors-with-ap
              "/tmp/tot-ap-students-2148+.txt")
(write-totals "57c66793" pre-2158-123-instructors-with-ap
              "/tmp/tot-ap-students-2148-.txt")
(write-totals "57c66794" post-2108-123-instructors-with-ap
              "/tmp/tot-ap-students-2118+.txt")



(define post-2138-123-instructors-without-ap
  (inner-join post-2138-123-instructors 
              no-has-ap
              '(student)
              #:permanent "57c61d98"
              #:use-existing #t))

(define pre-2158-123-instructors-without-ap
  (inner-join pre-2158-123-instructors 
              no-has-ap
              '(student)
              #:permanent "57c61d99"
              #:use-existing #t))

(define post-2108-123-instructors-without-ap
  (inner-join post-2108-123-instructors 
              no-has-ap
              '(student)
              #:permanent "57c66795"
              #:use-existing #t))

(write-totals "57c67a63" post-2138-123-instructors-without-ap
              "/tmp/tot-non-ap-students-2148+.txt")
(write-totals "57c67a64" pre-2158-123-instructors-without-ap
              "/tmp/tot-non-ap-students-2148-.txt")
(write-totals "57c67a65" post-2108-123-instructors-without-ap
              "/tmp/tot-non-ap-students-2118+.txt")

(printf "# of students in 2138+ population that have an ap score: ~v\n"
        (table-size post-2138-123-instructors-with-ap))

(printf "# of students in 2158- population that have an ap score: ~v\n"
        (table-size pre-2158-123-instructors-with-ap))

(define post-2108-instructor-grades-101
  (natural-join first-time-101-grades
                post-2108-123-instructors 
                #:permanent "instructor_grades_101_p0"
                #:use-existing #t))

(define post-2108-instructor-grades-101-with-ap
  (natural-join first-time-101-grades
                post-2108-123-instructors-with-ap
                #:permanent "instructor_grades_101_with_ap_p0"
                #:use-existing #t))

(define post-2108-instructor-grades-101-without-ap
  (natural-join first-time-101-grades
                post-2108-123-instructors-without-ap
                #:permanent "instructor_grades_101_without_ap_p0"
                #:use-existing #t))

(define pre-2158-instructor-grades-102
  (natural-join first-time-102-grades
                pre-2158-123-instructors 
                #:permanent "instructor_grades_102_p5"
                #:use-existing #t))

(define pre-2158-instructor-grades-102-with-ap
  (natural-join first-time-102-grades
                pre-2158-123-instructors-with-ap
                #:permanent "instructor_grades_102_with_ap_p5"
                #:use-existing #t))

(define pre-2158-instructor-grades-102-without-ap
  (natural-join first-time-102-grades
                pre-2158-123-instructors-without-ap
                #:permanent "instructor_grades_102_without_ap_p5"
                #:use-existing #t))

(define (write-stats table file)
  (define results
    (table-select table
                  '(instructor grade (count))
                  #:group-by '(instructor grade)))
  (csv-write results file))

(write-stats post-2108-instructor-grades-101
             "/tmp/all-students-101-stats-2118+.txt")
(write-stats post-2108-instructor-grades-101-with-ap
             "/tmp/ap-students-101-stats-2118+.txt")
(write-stats post-2108-instructor-grades-101-without-ap
             "/tmp/non-ap-students-101-stats-2118+.txt")

(write-stats pre-2158-instructor-grades-102
             "/tmp/all-students-102-stats-2148-.txt")
(write-stats pre-2158-instructor-grades-102-with-ap
             "/tmp/ap-students-102-stats-2148-.txt")
(write-stats pre-2158-instructor-grades-102-without-ap
             "/tmp/non-ap-students-102-stats-2148-.txt")

(define post-2108-students-with-123-grades
  (table-select grade-facts-table '(student)
                #:where '((= class "CPE 123")
                          (< 2108 qtr))))

(printf "students with 123 grades that don't have an associated instructor:\n")
(remove* (table-select post-2108-123-instructors  '(student)
                       #:group-by '(student))
         post-2108-students-with-123-grades)

;; cohort adjustment: some students skipped 101 but took 102 or 103
(define (skippers source-table temptable timestr)
  (define got-a-grade-in-103
    (list->set (table-select grade-facts-table '(student)
                             #:where '((= class "CPE 103")))))
  (define got-a-grade-in-102
    (list->set (table-select grade-facts-table '(student)
                             #:where '((= class "CPE 102")))))
  (define got-a-grade-in-101
    (list->set (table-select grade-facts-table '(student)
                             #:where '((= class "CPE 102")))))

  (define got-a-grade-in-102-or-103
    (set-union got-a-grade-in-103 got-a-grade-in-102))
  (define got-a-grade-in-102-or-103-but-not-101
    (set-subtract got-a-grade-in-102-or-103 got-a-grade-in-101))
  (define got-a-grade-in-103-but-not-102
    (set-subtract got-a-grade-in-103 got-a-grade-in-102))
  
  (define instructors-of-skipped-101s
    (inner-join source-table 
                (make-table '(student)
                            got-a-grade-in-102-or-103-but-not-101
                            #:permanent (string-append temptable "a")
                            #:use-existing #t)
                '(student)
                #:permanent (string-append temptable "b")
                #:use-existing #t))

  (define instructors-of-skipped-102s
    (inner-join source-table 
                (make-table '(student)
                            got-a-grade-in-103-but-not-102
                            #:permanent (string-append temptable "c")
                            #:use-existing #t)
                '(student)
                #:permanent (string-append temptable "d")
                #:use-existing #t))
  
  (printf "# of students that skipped 101, by 123 instructor")
  (define skipped-101
    (table-select instructors-of-skipped-101s
                  '(instructor (count))
                  #:group-by '(instructor)))
  (display skipped-101)
  (newline)
  (csv-write skipped-101 (~a "/tmp/skipped-101-"timestr".txt"))

  (define skipped-101-with-ap
    (table-select
     (inner-join instructors-of-skipped-101s
                 has-ap
                 '(student)
                 #:permanent (string-append temptable "e")
                 #:use-existing #t)
     '(instructor (count))
     #:group-by '(instructor)))

  (define skipped-101-without-ap
    (table-select (inner-join instructors-of-skipped-101s
                              no-has-ap
                              '(student)
                              #:permanent (string-append temptable "f")
                              #:use-existing #t)
                  '(instructor (count))
                  #:group-by '(instructor)))

  (displayln skipped-101-with-ap)
  (csv-write skipped-101-with-ap
             (~a "/tmp/skipped-102-ap-" timestr ".txt"))

  (displayln skipped-101-without-ap)
  (csv-write skipped-101-without-ap
             (~a "/tmp/skipped-101-non-ap-" timestr ".txt"))

)

(skippers post-2108-123-instructors "57c67a66" "2118+")
(skippers pre-2158-123-instructors "57c67a67" "2158-")

(table-select
 (inner-join has-ap student-123-instructors
             '(student)
             #:permanent "231442341"
             #:use-existing #t)
 '(123_qtr (count))
 #:group-by '(123_qtr))
(table-select
 student-123-instructors
 '(123_qtr (count))
 #:group-by '(123_qtr))

#;(set-subtract
 (set-intersect
  (list->set (table-select post-2138-123-instructors  '(student)))
  (list->set (table-select grade-facts-table '(student)
                           #:where '((= class "CPE 103")))))
 (list->set (table-select grade-facts-table '(student)
                          #:where '((= class "CPE 102")))))
