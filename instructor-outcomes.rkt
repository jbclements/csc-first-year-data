#lang racket

;; this file uses the existing data to build a big table where each student
;; has exactly one row, and columns indicating her or his grades in the follow-on
;; courses and whether s/he took the AP. There's also 
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

;; goofy fixup for spring 2017: 103 should be counted as 102, add a column.

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


