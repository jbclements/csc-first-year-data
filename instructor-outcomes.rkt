#lang racket

;; does the choice of first instructor affect the outcome from the
;; first time a student takes 103?

(require csv-reading
         "student-data.rkt"
         "table-sqlite.rkt")

;; strip a leading byte order mark from an input port
(define (discard-bom p)
  (void (regexp-try-match #rx"^\uFEFF" p)))

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


;; copied from rebuild-databases. Should probably be in a library:
;; find the index of an element in a list
(define (find-pos elt l)
  (let loop ([remaining l] [i 0])
    (cond [(empty? remaining) (error 'find-pos "couldn't find element: ~v"
                                     elt)]
          [else (cond [(equal? (first remaining) elt) i]
                      [else (loop (cdr remaining) (add1 i))])])))

;; given csv list (where first element is titles) and a list of
;; desired column headers, return a csv list containing only those
;; columns (in the order specified by colnames
(define (csv-extract-columns lines colnames)
  (define desired-indices
    (for/list ([colname colnames]) (find-pos colname (first lines))))
  (for/list ([line (in-list lines)])
    (for/list ([idx (in-list desired-indices)])
      (list-ref line idx))))

(require rackunit)
(check-equal? (csv-extract-columns
               '(("a" "zz" "b" q9)
                 (1 2 3 4)
                 (5 6 7 8)
                 (9 10 11 12))
               '(q9 "a"))
              '((q9 "a")
                (4 1)
                (8 5)
                (12 9)))

(define desired-columns
  '("Primary Instructor"
    "Emplid"
    "Term"))

(define data-2118-2158
  (csv-extract-columns
   (call-with-input-file "/Users/clements/clements/datasets/cpe-123-rosters-2118-2158.csv"
     csv->list)
   desired-columns))

(first data-2118-2158)

(define data-2168
  (csv-extract-columns
   (call-with-input-file
       "/Users/clements/clements/datasets/cpe-123-rosters-2168.csv"
     (λ (port)
       (begin (discard-bom port)
              (csv->list port))))
   desired-columns))

(define student-instructor-data
  (cons (first data-2118-2158)
        (remove-duplicates
         (append
          (rest data-2118-2158)
          (rest data-2168)))))

(define student-123-instructors
  (make-table '(instructor student 123_qtr)
   (for/list ([l (in-list (rest student-instructor-data))])
     (vector (first l)
             (pad-to-9 (second l))
             (qtr-text->qtr (third l))))
   #:permanent "student_instructors_123"
   #:use-existing #t
   ))

(printf "students in the 123 list: ~v\n" (length (rest student-instructor-data)))

(define major-student-123-instructors
  (make-table-from-select
   (inner-join student-123-instructors
               grade-facts-table
               '(student)
               #:permanent "57bb"
               #:use-existing #t)
   '(student instructor 123_qtr)
   #:group-by '(student instructor 123_qtr)
   #:permanent "57c0b455"
   #:use-existing #t))

;; make sure nobody took 123 twice...
(unless (equal?
         (table-select major-student-123-instructors '((count)))
         (list
          (vector
           (length
            (table-select major-student-123-instructors '((count))
                          #:group-by '(student instructor))))))
  (error 'ono201708happyweddingandybrenna))

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

  (define instructors-of-skipped-102s
    (inner-join source-table 
                (make-table '(student)
                            got-a-grade-in-103-but-not-102
                            #:permanent (string-append temptable "c")
                            #:use-existing #t)
                '(student)
                #:permanent (string-append temptable "d")
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
             (~a "/tmp/skipped-102-ap-" timestr ".txt"))
  ;; weird! why so many in last 2 years? Something strange!


  (printf "students that skipped 101 without AP credit:\n")
  (displayln skipped-101-without-ap)
  (csv-write skipped-101-without-ap
             (~a "/tmp/skipped-101-non-ap-" timestr ".txt"))

)

(printf "\npost-2108-skippers:\n")
(skippers post-2108-123-instructors "57c67a66" "2118+")
(printf "\npre-2158-skippers:\n")
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
