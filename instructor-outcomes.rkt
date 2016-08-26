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

(define first-time-101-grades
  (inner-join first-time-101s
              grade-facts-table
              '(student qtr)
              #:permanent "ft_101_grades"
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

(printf "count by instructor of students in 123 in the major in 2014 and 2015")
(table-select post-2138-123-instructors
              '(instructor (count))
              #:group-by '(instructor))

;; in order to count the students that were majors
;; but never made it to 101, we need a count of the students
;; 
(let ()
  (define majors-by-instructor
    (table-select
     (make-table-from-select (inner-join post-2138-123-instructors
                                         grade-facts-table
                                         '(student)
                                         #:permanent "ijtemp557c0"
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


(define 123-instructors-with-ap
  (inner-join post-2138-123-instructors 
              has-ap
              '(student)
              #:permanent "pre_5_123_with_ap"
              #:use-existing #t))

(let ()
  (define majors-by-instructor
    (table-select
     (make-table-from-select (inner-join 123-instructors-with-ap
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

(define 123-instructors-without-ap
  (inner-join post-2138-123-instructors 
              no-has-ap
              '(student)
              #:permanent "pre_5_123_without_ap"
              #:use-existing #t))

(let ()
  (define majors-by-instructor
    (table-select
     (make-table-from-select (inner-join 123-instructors-without-ap
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
        (table-size 123-instructors-with-ap))

(define instructor-grades-101
  (natural-join first-time-101-grades
                post-2138-123-instructors 
                #:permanent "instructor_grades_101"
                #:use-existing #t))

(define instructor-grades-101-with-ap
  (natural-join first-time-101-grades
                123-instructors-with-ap
                #:permanent "instructor_grades_101_with_ap"
                #:use-existing #t))

(define instructor-grades-101-without-ap
  (natural-join first-time-101-grades
                123-instructors-without-ap
                #:permanent "instructor_grades_101_without_ap"
                #:use-existing #t))





(let ()
  (define results
    (table-select instructor-grades-101 '(instructor grade (count))
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
    (table-select instructor-grades-101-with-ap '(instructor grade (count))
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
    (table-select instructor-grades-101-without-ap '(instructor grade (count))
                  #:group-by '(instructor grade)))

  (sequence-length results)
  #;(take (sequence->list results) 10)

  (with-output-to-file "/tmp/gg3.txt"
    (λ ()
      (for ([v results])
        (apply printf "~v, ~v, ~v\n" (vector->list v))))
    #:exists 'truncate))

(table-select grade-facts-table '(class (count))
              #:where '((= qtr 2162))
              #:group-by '(class))


(define students-with-123-grades
  (table-select grade-facts-table '(student)
                #:where '((= class "CPE 123")
                          (< 2138 qtr))))

(remove* (table-select post-2138-123-instructors  '(student)
                       #:group-by '(student))
         students-with-123-grades)

;; cohort adjustment: some students skipped 101 but took 102 or 103
(let ()
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

  (define instructors-of-skipped-101s
    (inner-join post-2138-123-instructors 
                (make-table '(student) got-a-grade-in-102-or-103-but-not-101
                            #:permanent "57c0b458aa"
                            #:use-existing #t)
                '(student)
                #:permanent "57c0b70b"
                #:use-existing #t))
  
  (printf "# of students that took 102 or 103 but not 101, by 123 instructor")
  (define skipped-101
    (table-select instructors-of-skipped-101s
                  '(instructor (count))
                  #:group-by '(instructor)))
  (display skipped-101)
  (newline)
  (call-with-output-file "/tmp/skipped-101.txt"
    (λ (port)
      (map (λ (r) (apply fprintf port "~v,~v\n"
                         (vector->list r)))
           skipped-101))
    #:exists 'truncate)

  (define skipped-101-with-ap
    (table-select (inner-join instructors-of-skipped-101s
                              has-ap
                              '(student)
                              #:permanent "57c0b991"
                              #:use-existing #t)
                  '(instructor (count))
                  #:group-by '(instructor)))

  (define skipped-101-without-ap
    (table-select (inner-join instructors-of-skipped-101s
                              no-has-ap
                              '(student)
                              #:permanent "57c0b992"
                              #:use-existing #t)
                  '(instructor (count))
                  #:group-by '(instructor)))

  (display skipped-101-with-ap)
  (newline)
  (call-with-output-file "/tmp/skipped-101-ap.txt"
    (λ (port)
      (map (λ (r) (apply fprintf port "~v,~v\n"
                         (vector->list r)))
           skipped-101-with-ap))
    #:exists 'truncate)

  (display skipped-101-without-ap)
  (newline)
  (call-with-output-file "/tmp/skipped-101-no-ap.txt"
    (λ (port)
      (map (λ (r) (apply fprintf port "~v,~v\n"
                         (vector->list r)))
           skipped-101-without-ap))
    #:exists 'truncate))

#;(set-subtract
 (set-intersect
  (list->set (table-select post-2138-123-instructors  '(student)))
  (list->set (table-select grade-facts-table '(student)
                           #:where '((= class "CPE 103")))))
 (list->set (table-select grade-facts-table '(student)
                          #:where '((= class "CPE 102")))))
