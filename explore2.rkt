#lang racket

(require "table-sqlite.rkt"
         "student-data.rkt"
         "grades.rkt"
         plot
         racket/block)


(define student-first-quarters
  (map
   first
   (map
    (λ (group) (sort group < #:key (vref 1)))
    (group-by
     (vref 0)
     (table-select grade-facts-table '(student qtr))))))
(make-table '(student qtr) student-first-quarters)

(define entry-data
  (map
   (λ (group) (list (vector-ref (first group) 1)
                    (length group)))
   (group-by (vref 1)
             student-first-quarters)))

(define plot-entry-data
  (for/list ([qtr (qtrs-in-range (apply min (map first entry-data))
                                 (apply max (map first entry-data)))])
    (vector qtr (first (dict-ref entry-data qtr '(0))))))

#;(plot-file
 (discrete-histogram plot-entry-data)
 #:width 600
 #:height 600
 #:x-label "quarter of first class"
 #:y-label "# of students"
 #:title "students entering the major by quarter, 2005—2016"
 "students-entering.png")


(define stu-recs
  (group-by
   (λ (x) (vector-ref x 0))
   (table-select grade-facts-table '(student qtr class grade))))

(define num-students (length stu-recs))
(printf "number of students: ~v\n"
        num-students)

(define classes-to-completion
  (for/list ([stu-rec (in-list stu-recs)])
    (define successes
      (filter (λ (rec)
                (match-define (vector student qtr class grade) rec)
                (and (equal? class "CPE 103")
                     (member grade success-grades)))
              stu-rec))
    (list (length stu-rec)
          (not (empty? successes)))))

(define num-completed
  (length (filter (λ (r) (second r)) classes-to-completion)))
(define num-uncompleted
  (length (filter (λ (r) (not (second r))) classes-to-completion)))

(printf "number of students completing 103 with good grade: ~v\n"
        num-completed)
(printf "number of students not completing 103 with good grade: ~v\n"
        num-uncompleted)

(define categorized
  (group-by
   first
   classes-to-completion))

(define plot-bins
  (sort
   (for/list ([c (in-list categorized)])
     (vector (first (first c))
             (list (* 100
                      (/ (length (filter (λ (r) (second r)) c))
                         num-students))
                   (* 100
                      (/ (length (filter (λ (r) (not (second r))) c))
                         num-students)))))
   <
   #:key (λ (v) (vector-ref v 0))))

(plot
 (stacked-histogram plot-bins
                    #:colors '(2 1)
                    #:labels '("complete" "not complete"))
 #:y-label "pct. of students"
 #:x-label "# of classes taken"
 #:title "distribution of outcomes"
 #:width 600
 #:height 600
 #;"/tmp/outcomes.png")

#;(map
 (λ (summary) (sort summary < #:key first))
 (call-with-values
  (λ () (partition (λ (p)
               (second p)) paths))
  list))



