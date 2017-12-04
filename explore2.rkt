#lang racket

(require sqlite-table
         "student-data.rkt"
         "grades.rkt"
         plot
         racket/block)

(define (pnum x)
  (/ (round (* 100.0 x)) 100))

;; given a table of student grades, compute the plot-bins and
;; cumulative plot-bins showing the number of classes students
;; take in the first year, and whether they succeed or not
(define (plot-bin-pair grade-table credits-in-sequence)

  (define all-recs
    (table-select grade-table '(student qtr class grade)))

  (define scus-taught (* 4 (length all-recs)))
  (printf "SCUs taught: ~v\n" (pnum scus-taught))
  
  ;; the records in the table
  (define stu-recs
    (group-by
     (λ (x) (vector-ref x 0))
     all-recs))

  
  (define num-students (length stu-recs))
  (printf "number of students: ~v\n"
          num-students)

  

  ;; a list of (List Natural Boolean) indicating how many classes
  ;; a given student took and whether they got a successful grade
  ;; in 103
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

  ;; the number of students that finished the first year
  (define num-completed
    (length (filter (λ (r) (second r)) classes-to-completion)))

  ;; the number of students that didn't finish the first year
  (define num-uncompleted
    (length (filter (λ (r) (not (second r))) classes-to-completion)))

  (printf "number of students completing 103 with good grade: ~v\n"
          num-completed)
  (printf "number of students not completing 103 with good grade: ~v\n"
          num-uncompleted)

  (printf "SCUs taught per successful student: ~v\n"
          (pnum (/ scus-taught num-completed)))
  (define non-wasted-scus (* credits-in-sequence num-completed))
  (define excess-scus (- scus-taught non-wasted-scus))
  (printf "excess SCUs: ~v\n" excess-scus)
  (printf "excess SCUs per successful student: ~v\n"
          (pnum (/ excess-scus num-completed)))
  (printf "excess SCU fraction: ~v%\n"
          (pnum
           (* 100 (/ (/ excess-scus num-completed) credits-in-sequence))))



  ;; (Listof (Listof (List Natural Boolean))). The same list as before,
  ;; grouped by number of classes taken
  (define categorized
    (group-by first classes-to-completion))

  ;; (Listof (Vector Complete-pct Incomplete-pct))
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

  ;; (Listof (Vector Incomplete-pct Complete-pct))
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

  (define success-rate
    (/ (second (vector-ref (last cumulative-plot-bins) 1)) 100))

  (printf "ultimate success rate: ~v\n" (* 1.0 success-rate))

  ;; the number of sections taught per student
  (define classes-taught-per-student
    (apply
     +
     (for/list ([p plot-bins])
       (match-define (vector i (list succeed fail)) p)
       (* i (/ (+ succeed fail) 100)))))

  (define classes-taught-per-successful-student
    (/ classes-taught-per-student
       success-rate))

  (printf "student-classes taught per student: ~v\n"
          (* 1.0 classes-taught-per-student))
  (printf "student-classes taught per successful student: ~v\n"
          (* 1.0 classes-taught-per-successful-student))

  (list plot-bins cumulative-plot-bins)
)

(match-define (list pre-123-plot-bins pre-123-plot-bins-cum)
  (plot-bin-pair pre-123-grade-table 12))
(match-define (list post-123-plot-bins post-123-plot-bins-cum) 
  (plot-bin-pair post-123-grade-table 16))

(plot-file
 (list
  (stacked-histogram pre-123-plot-bins
                     #:colors '(2 1)
                     #:labels '("complete (pre)" "not complete(pre)")
                     #:skip 2.5)
  (stacked-histogram post-123-plot-bins
                     #:colors '(3 4)
                     #:labels '("complete (post)" "not complete (post)")
                     #:skip 2.5
                     #:x-min 1))
 #:y-label "pct. of students"
 #:x-label "# of classes taken"
 #:title "distribution of outcomes, pre- and post-change" 
 #:width 600
 #:height 600
 "/tmp/outcomes.png")

(plot-file
 (list
  (stacked-histogram pre-123-plot-bins-cum
                     #:colors '(1 2)
                     #:labels '("not complete(pre)" "complete (pre)")
                     #:skip 2.5)
  (stacked-histogram post-123-plot-bins-cum
                     #:colors '(4 3)
                     #:labels '("not complete (post)" "complete (post)")
                     #:skip 2.5
                     #:x-min 1))
 #:y-label "pct. of students (cumulative)"
 #:x-label "# of classes taken"
 #:title "cumulative distribution of outcomes, pre- and post-change"
 #:width 600
 #:height 600
 "/tmp/cumulative-outcomes.png")




