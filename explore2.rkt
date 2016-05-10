#lang racket

(require "table-sqlite.rkt"
         "student-data.rkt"
         "grades.rkt"
         plot
         racket/block)



(define stu-recs
  (group-by
   (λ (x) (vector-ref x 0))
   (table-select post-123-grade-table '(student qtr class grade))))

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

(plot #;-file
 (stacked-histogram plot-bins
                    #:colors '(2 1)
                    #:labels '("complete" "not complete"))
 #:y-label "pct. of students"
 #:x-label "# of classes taken"
 #:title "distribution of outcomes, since 123" 
 #:width 600
 #:height 600
 #;"/tmp/outcomes.png")

#;(map
 (λ (summary) (sort summary < #:key first))
 (call-with-values
  (λ () (partition (λ (p)
               (second p)) paths))
  list))

;; I need for/accum again!
(define cumulative-plot-bins
  (let loop ([plot-bins plot-bins]
             [fail-so-far 0]
             [success-so-far 0])
    (cond [(empty? plot-bins) empty]
          [else
           (match-define (vector i (list success fail)) (first plot-bins))
           (define new-success (+ success success-so-far))
           (define new-fail (+ fail fail-so-far))
           (cons (vector i (list new-fail new-success))
                 (loop (rest plot-bins)
                       new-fail
                       new-success))])))

(plot
 (stacked-histogram cumulative-plot-bins
                    #:colors '(1 2)
                    #:labels '("not complete" "complete"))
 #:y-label "pct. of students (cumulative)"
 #:x-label "# of classes taken"
 #:title "cumulative distribution of outcomes, since 123"
 #:width 600
 #:height 600
 #;"/tmp/cumulative-outcomes.png")



