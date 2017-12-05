#lang racket

;; does the choice of first instructor affect the outcome from the
;; first time a student takes 103?

(require csv-reading
         sqlite-table
         "student-data.rkt")

(define first-time-123s
  (make-table '(student 123_qtr)
              (table-select grade-facts-table
                            '(student (min qtr))
                            #:where '((= class "CPE 123"))
                            #:group-by '(student))              
              #:permanent "5a263170"
              #:use-existing #t))

(define (check-no-student-dups table)
  (when (check-duplicates (table-select table '(student)))
    (error 'rarararararara)))

(check-no-student-dups first-time-123s)
(printf "all students taking 123: ~v\n"
        (table-select first-time-123s '((count))))


(define major-student-123-instructors
  (inner-join first-time-123s
              student-123-instructors
              '(student 123_qtr)
              #:permanent "5a263171"
              #:use-existing #t))

(check-no-student-dups major-student-123-instructors)
(define 123-student-count
  (vector-ref (first (table-select major-student-123-instructors
                                   '((count))))
              0))
(printf "majors taking 123 for which we have instructors: ~v\n"
        123-student-count)

(printf "count by instructor of students in 123 in the major\n")
(table-select major-student-123-instructors '(instructor (count))
              #:group-by '(instructor))

(define first-time-101s
  (make-table '(student qtr class)
              (table-select grade-facts-table '(student (min qtr) class)
                            #:where '((= class "CPE 101"))
                            #:group-by '(student))
              #:permanent "first_time_101"
              #:use-existing #t))

(check-no-student-dups first-time-101s)

(define first-time-102s
  (make-table '(student qtr class)
              (table-select grade-facts-table '(student (min qtr) class)
                            #:where '((= class "CPE 102"))
                            #:group-by '(student))
              #:permanent "first_time_102"
              #:use-existing #t))

(check-no-student-dups first-time-102s)

(define first-time-103s
  (make-table '(student qtr class)
              (table-select grade-facts-table '(student (min qtr) class)
                            #:where '((= class "CPE 103"))
                            #:group-by '(student))
              #:permanent "first_time_103"
              #:use-existing #t))

(check-no-student-dups first-time-103s)



(define first-time-101-grades
  (inner-join first-time-101s
              grade-facts-table
              '(student qtr class)
              #:permanent "ft_101_grades"
              #:use-existing #t))

(check-no-student-dups first-time-101-grades)


(define first-time-102-grades
  (inner-join first-time-102s
              grade-facts-table
              '(student qtr class)
              #:permanent "ft_102_grades"
              #:use-existing #t))

(check-no-student-dups first-time-102-grades)

(define first-time-103-grades
  (inner-join first-time-103s
              grade-facts-table
              '(student qtr class)
              #:permanent "ft_103_grades"
              #:use-existing #t))

(check-no-student-dups first-time-103-grades)

(define has-ap
  (make-table-from-select ap-facts-table '(student)
                          #:group-by '(student)
                          #:permanent "students_with_ap"
                          #:use-existing #t))

(check-no-student-dups has-ap)


(define big-table-1
  (left-join
   (left-join
    (left-join
     (left-join
      major-student-123-instructors
      first-time-101-grades
      '(student)
      #:permanent "5a261381"
      #:use-existing #t)
     first-time-102-grades
     '(student)
     #:permanent "5a261382"
     #:use-existing #t)
    first-time-103-grades
    '(student)
    #:permanent "5a261383"
    #:use-existing #t)
   ap-facts-table
   '(student)
   #:permanent "5a261385"
   #:use-existing #t))

(check-no-student-dups big-table-1)

;; goofy fixup for spring 2017: 103 should be counted as 102:

(define big-table-2
  (left-join
   big-table-1
   (make-table-from-select big-table-1
                             '(student qtr:2 grade:2)
                             #:where '((= qtr:2 2174))
                             #:permanent "5a261387"
                             #:use-existing #t)
   '(student)
   #:permanent "big_student_table"
   #:use-existing #t))

(check-no-student-dups big-table-2)

;; WOW... I think the rest of this file is ALL unnecessary:
#;(
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

;; students and instructors for majors in 2014,...
(define post-2138-123-instructors
  (make-table-from-select
   major-student-123-instructors '(student instructor)
   #:where '((< 2138 123_qtr))
   #:permanent "post_2138_123_instructors57c0b456"
   #:use-existing #t))

(define pre-2158-123-instructors
  (make-table-from-select
   major-student-123-instructors '(student instructor)
   #:where '((< 123_qtr 2158))
   #:permanent "pre_2158_123_instructors57c0b456"
   #:use-existing #t))

(define pre-2168-123-instructors
  (make-table-from-select
   major-student-123-instructors '(student instructor)
   #:where '((< 123_qtr 2168))
   #:permanent "pre_2158_123_instructors59a9e006"
   #:use-existing #t))


(define post-2108-123-instructors
  (make-table-from-select
   major-student-123-instructors '(student instructor)
   #:where '((< 2108 123_qtr))
   #:permanent "post_2108_123_instructors57c0b456"
   #:use-existing #t))

(printf "count by instructor of students in 123 in the major in 2014 and later")
(table-select post-2138-123-instructors
              '(instructor (count))
              #:group-by '(instructor))

(printf "count by instructor of students in 123 in the major in 2011-2014")
(table-select pre-2158-123-instructors
              '(instructor (count))
              #:group-by '(instructor))

(printf "count by instructor of students in 123 in the major in 2011-")
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
(write-totals "57c67a63" pre-2168-123-instructors
              "/tmp/tot-students-2158-.txt")
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

(define pre-2168-123-instructors-with-ap
  (inner-join pre-2168-123-instructors 
              has-ap
              '(student)
              #:permanent "pre_6_123_with_ap"
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
(write-totals "57c66795" pre-2168-123-instructors-with-ap
              "/tmp/tot-ap-students-2158-.txt")
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

(define pre-2168-123-instructors-without-ap
  (inner-join pre-2168-123-instructors 
              no-has-ap
              '(student)
              #:permanent "59a9e006"
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
(write-totals "59a9e145" pre-2168-123-instructors-without-ap
              "/tmp/tot-non-ap-students-2158-.txt")
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

(define pre-2168-instructor-grades-101
  (natural-join first-time-101-grades
                pre-2168-123-instructors 
                #:permanent "instructor_grades_101_p6"
                #:use-existing #t))

(define pre-2168-instructor-grades-101-with-ap
  (natural-join first-time-101-grades
                pre-2168-123-instructors-with-ap
                #:permanent "instructor_grades_101_with_ap_p6"
                #:use-existing #t))

(define pre-2168-instructor-grades-101-without-ap
  (natural-join first-time-101-grades
                pre-2168-123-instructors-without-ap
                #:permanent "instructor_grades_101_without_ap_p6"
                #:use-existing #t))

(define pre-2168-instructor-grades-102
  (natural-join first-time-102-grades
                pre-2168-123-instructors 
                #:permanent "instructor_grades_102_p6"
                #:use-existing #t))

(define pre-2168-instructor-grades-102-with-ap
  (natural-join first-time-102-grades
                pre-2168-123-instructors-with-ap
                #:permanent "instructor_grades_102_with_ap_p6"
                #:use-existing #t))

(define pre-2168-instructor-grades-102-without-ap
  (natural-join first-time-102-grades
                pre-2168-123-instructors-without-ap
                #:permanent "instructor_grades_102_without_ap_p6"
                #:use-existing #t))

(define (write-stats table file)
  (define results
    (table-select table
                  '(instructor grade (count))
                  #:group-by '(instructor grade)))
  (csv-write results file))

;; post-2108, 101 only
(write-stats post-2108-instructor-grades-101
             "/tmp/all-students-101-stats-2118+.txt")
(write-stats post-2108-instructor-grades-101-with-ap
             "/tmp/ap-students-101-stats-2118+.txt")
(write-stats post-2108-instructor-grades-101-without-ap
             "/tmp/non-ap-students-101-stats-2118+.txt")

;; pre-2158, 102 only
(write-stats pre-2158-instructor-grades-102
             "/tmp/all-students-102-stats-2148-.txt")
(write-stats pre-2158-instructor-grades-102-with-ap
             "/tmp/ap-students-102-stats-2148-.txt")
(write-stats pre-2158-instructor-grades-102-without-ap
             "/tmp/non-ap-students-102-stats-2148-.txt")

;; pre-2168, 101 only
(write-stats pre-2168-instructor-grades-101
             "/tmp/all-students-101-stats-2158-.txt")
(write-stats pre-2168-instructor-grades-101-with-ap
             "/tmp/ap-students-101-stats-2158-.txt")
(write-stats pre-2168-instructor-grades-101-without-ap
             "/tmp/non-ap-students-101-stats-2158-.txt")

;; pre-2168, 102 only
(write-stats pre-2168-instructor-grades-102
             "/tmp/all-students-102-stats-2158-.txt")
(write-stats pre-2168-instructor-grades-102-with-ap
             "/tmp/ap-students-102-stats-2158-.txt")
(write-stats pre-2168-instructor-grades-102-without-ap
             "/tmp/non-ap-students-102-stats-2158-.txt")

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

  (printf "# of students that skipped 101, by 123 instructor\n")
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
  (printf "students that skipped 101 with AP credit:\n")
  (displayln skipped-101-with-ap)
  (csv-write skipped-101-with-ap
             (~a "/tmp/skipped-101-ap-" timestr ".txt"))


  (printf "students that skipped 101 without AP credit:\n")
  (displayln skipped-101-without-ap)
  (csv-write skipped-101-without-ap
             (~a "/tmp/skipped-101-non-ap-" timestr ".txt"))

  (define instructors-of-skipped-102s
    (inner-join source-table 
                (make-table '(student)
                            got-a-grade-in-103-but-not-102
                            #:permanent (string-append temptable "c")
                            #:use-existing #t)
                '(student)
                #:permanent (string-append temptable "d")
                #:use-existing #t))

  (printf "# of students that skipped 102, by 123 instructor\n")
  (define skipped-102
    (table-select instructors-of-skipped-102s
                  '(instructor (count))
                  #:group-by '(instructor)))
  (display skipped-102)
  (newline)
  (csv-write skipped-102 (~a "/tmp/skipped-102-"timestr".txt"))

  (define skipped-102-with-ap
    (table-select
     (inner-join instructors-of-skipped-101s
                 has-ap
                 '(student)
                 #:permanent (string-append temptable "z")
                 #:use-existing #t)
     '(instructor (count))
     #:group-by '(instructor)))

  (define skipped-102-without-ap
    (table-select (inner-join instructors-of-skipped-102s
                              no-has-ap
                              '(student)
                              #:permanent (string-append temptable "f")
                              #:use-existing #t)
                  '(instructor (count))
                  #:group-by '(instructor)))
  (printf "students that skipped 102 with AP credit:\n")
  (displayln skipped-102-with-ap)
  (csv-write skipped-102-with-ap
             (~a "/tmp/skipped-102-ap-" timestr ".txt"))

  (printf "students that skipped 102 without AP credit:\n")
  (displayln skipped-102-without-ap)
  (csv-write skipped-102-without-ap
             (~a "/tmp/skipped-102-non-ap-" timestr ".txt"))
  
  

  

)

(printf "\npost-2108-skippers:\n")
(skippers post-2108-123-instructors "57c67a66" "2118+")
(printf "\npre-2158-skippers:\n")
(skippers pre-2158-123-instructors "57c67a67" "2148-")
(printf "\npre-2168-skippers:\n")
(skippers pre-2168-123-instructors "57c67a67aoeu" "2158-")

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


(define temptemp
  (inner-join pre-2168-123-instructors
              (make-table-from-select
               grade-facts-table
               '(student grade)
               #:where '((= class "CPE 102"))
               #:permanent "59a9ee42abc"
               #:use-existing #t)
              '(student)
              #:permanent "59a9ee43abc"
              #:use-existing #t)))
