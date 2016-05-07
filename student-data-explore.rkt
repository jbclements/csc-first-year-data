#lang racket

(require "table-sqlite.rkt"
         "student-data.rkt"
         "grades.rkt"
         plot
         racket/block)

(table-select grade-facts-table '(qtr (count)) #:group-by '(qtr))

(table-select grade-facts-table '(qtr student)
              #:group-by '(qtr))

(define ap-takers (in-table-column ap-facts-table 'student))

(sequence-length ap-takers)

(sequence-length (table-select ap-facts-table '(student) #:group-by '(student)))

(unless
    (equal?
     (list->set (sequence->list (in-table-column grade-facts-table 'grade)))
     (list->set grade-ordering))
  (error 'missing-grades-maybe))
(unless
    (equal?
     (list->set (sequence->list (in-table-column grade-facts-table 'class)))
     (list->set class-ordering))
  (error 'missing-class-maybe))

(define grade-ordering-hash
  (for/hash ([grade (in-list grade-ordering)]
             [i (in-naturals)])
    (values grade i)))

(define class-ordering-hash
  (for/hash ([class (in-list class-ordering)]
             [i (in-naturals)])
    (values class i)))

;; order vectors of the form #(class grade #)
(define (fact-< a b)
  (or (< (dict-ref grade-ordering-hash (vector-ref a 1))
         (dict-ref grade-ordering-hash (vector-ref b 1)))
      (< (dict-ref class-ordering-hash (vector-ref a 0))
         (dict-ref class-ordering-hash (vector-ref b 0)))))

;; order vectors of the form #(grade #)
(define (gradevec-< a b)
  (< (dict-ref grade-ordering-hash (vector-ref a 0))
     (dict-ref grade-ordering-hash (vector-ref b 0))))

(define zzz
  (sort (sequence->list
         (table-select grade-facts-table
                       '(class grade (count))
                       #:group-by '(class grade)))
        fact-<))

(plot3d (discrete-histogram3d
         zzz)
        #:title "all students, # getting grade")


#;(table-select grade-facts-table '(class) )
#;(for/list ([student (in-table-column ap-score-table 'student)])
  (table-select))

#;(table-select grade-facts-table 'aa)

(define test-and-grades (natural-join grade-facts-table
                                      ap-facts-table))

(define 103-grades
  (make-table '(grade students)
  (table-select grade-facts-table '(grade (count))
                #:group-by '(grade)
                #:where `((= class "CPE 103")))))

(define 103-num-takers
  (apply +
         (map (λ (x) (vector-ref x 0))
              (table-select 103-grades '(students)))))

(define 103-ap-grades
  (make-table '(grade students)
  (table-select test-and-grades '(grade (count))
                #:group-by '(grade)
                #:where `((= class "CPE 103")))))

(define 103-ap-num-takers
  (apply +
         (map (λ (x) (vector-ref x 0))
              (table-select 103-ap-grades '(students)))))



#;(plot
 (discrete-histogram
  (sort 
(table-select grade-facts-table '(grade (count))
              #:group-by '(grade)
              #:where `((= class "CPE 103")))
gradevec-<)))


(block
 (define interleaved
   (for/list ([grade (in-list grade-ordering)])
     (define all-earners (table-ref1 103-grades 'grade 'students grade 0))
     (define ap-earners (table-ref1 103-ap-grades 'grade 'students grade 0))
     (list
      (vector
       grade
       (- all-earners ap-earners))
      (vector
       grade
       ap-earners))))

 (plot (list
        (discrete-histogram
         (map first interleaved)
         #:skip 2.5
         #:x-min 0
         #:label "non-AP takers"
         )
        (discrete-histogram
         (map second interleaved)
         #:skip 2.5
         #:x-min 1
         #:color 2
         #:label "AP takers"))
       #:x-label "Grade"
       #:y-label "# of students"
       #:title "Grades in 103"))

(define interleaved
  (for/list ([grade (in-list grade-ordering)])
    (define all-earners (table-ref1 103-grades 'grade 'students grade 0))
    (define ap-earners (table-ref1 103-ap-grades 'grade 'students grade 0))
    (list
     (vector
      grade
      (/ (- all-earners ap-earners)
         (- 103-num-takers 103-ap-num-takers)))
     (vector
      grade
      (/ ap-earners 103-ap-num-takers)))))

(plot (list
       (discrete-histogram
        (map first interleaved)
        #:skip 2.5
        #:x-min 0
        #:label "non-AP takers"
        )
       (discrete-histogram
        (map second interleaved)
        #:skip 2.5
        #:x-min 1
        #:color 2
        #:label "AP takers"))
      #:x-label "Grade"
      #:y-label "% of pool"
      #:title "Grades in 103")

#;(plot
 (discrete-histogram
  (sort 
(table-select test-and-grades '(grade (count))
              #:group-by '(grade)
              #:where `((= class "CPE 103")))
gradevec-<)))

;(table-select grade-facts-table )


#;(back-door/rows
 (~a "SELECT a.student,a.qtr,a.class,a.grade,b.class,b.grade FROM ("grade-facts-table" a, "grade-facts-table" b) WHERE a.student=b.student AND a.qtr=b.qtr AND a.class != b.class;"))

