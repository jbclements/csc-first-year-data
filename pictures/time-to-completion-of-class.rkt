#lang racket

(require db
         plot
         csse-scheduling/qtr-math
         "../grades.rkt")

;; this file produces pictures showing how long it takes
;; a particular cohort to complete a particular class.

;; I was doing this with CSV files. Now I'm going to try doing it directly with the database.
;; er... okay, now I'm going straight to postgres.

(define conn
  (postgresql-connect #:database "cssegrades"
                      ;#:host "localhost"
                      #:port 5432
                      #:user "clements"))

;; given a class number, return a list of records containing
;; entry quarter and quarter in which the given class was completed.
;; (listof (list entry-qtr completion-qtr))
(define (time-to-completion num)
  (map (λ (pr)
         (list (first pr)
               (cond [(sql-null? (second pr)) #f]
                     [else (second pr)])))
       (map vector->list
            (query-rows
             conn
             (~a
              "SELECT a.qtr,b.qtr"
              " FROM (first_cs_class a LEFT JOIN"
              "       (SELECT id,MIN(qtr) qtr FROM
  raw_grade WHERE num=$1 AND grade IN (SELECT grade FROM passing_grade)
  GROUP BY id) b"
              " ON a.id=b.id);")
             num))))

;; what course are we interested in?
(define coursenum "357")
(define completion-data (time-to-completion coursenum))

;; how many quarters should be shown in the zoomed-in graph?
(define zoomed-qtrs-shown 9)

(define completion-trimmed
  (filter (λ (pr) (<= 2058 (first pr))) completion-data))

(define by-year (group-by first completion-trimmed))

(define year-groupings
  (for/list ([year-group (in-list by-year)])
    (cons (first (first year-group))
          (map (λ (g) (list
                       (cond [(second (first g))
                              (add1
                               (length
                                (qtrs-in-range (first (first year-group))
                                               (second (first g))
                                               #:include-summer? #f)))]
                             [else #f])
                       (length g)))
               (group-by second
                         year-group)))))

;; given a map from how many quarters to how many people,
;; build a cumulative completion dataset
;((Listof (List (U False Natural) Natural))
; -> (Listof (List Natural Real)))
(define (stairstep groupings)
  (define num-students (apply + (map second groupings)))
  (define successes (filter (λ (pr) (not (false? (first pr))))
                            groupings))
  (define-values (points _)
    (for/fold ([points '()]
               [student-accum 0])
              ([grouping (in-list (sort successes
                                        <
                                        #:key first))])
      (match-define (list qtrs n) grouping)
      (define new-accum (+ student-accum n))
      (values (cons (list qtrs (/ (+ n student-accum)
                                  num-students))
                    points)
              new-accum)))
  (reverse points))

(require rackunit)
(check-equal?
 (take
  (stairstep '((4 98) (#f 67) (3 10) (5 21)
                      (6 7) (2 4) (9 1) (1 2) (7 1)))
  5)
 '((1 2/211) (2 6/211) (3 16/211) (4 114/211)
             (5 135/211)))

(define year-points
  (for/list ([year-grouping (in-list year-groupings)])
    (list (first year-grouping)
          (stairstep (rest year-grouping)))))

(define incoming-students
  (sort
   (for/list ([year-grouping (in-list year-groupings)])
     (list (- (first year-grouping) 2008)
           (apply + (map second (rest year-grouping)))))
   <
   #:key first))

(define incoming-plot
  (plot-file
   (discrete-histogram incoming-students)
   #:title "incoming students"
   #:x-label "quarter (50 = fall 2005)"
   #:y-label "students"
   #:width 800
   "/tmp/foo.svg"))

;; executive decision... ignoring off-quarter entries.

(define fall-years
  (sort (filter (λ (yp)
                  (= 8 (modulo (first yp) 10)))
                year-points)
        <
        #:key first))

(plot-file
 #:y-max 1.0
 #:title (~a "students passing "coursenum)
 #:x-label "elapsed qtrs (omitting summer)"
 #:y-label "fraction that have passed the class"
 (for/list ([year (in-list fall-years)]
            [color (in-naturals)]
            [style (in-naturals)])
   (define p (cons (list 0 0) (second year)))
   (lines p
          #:color color
          #:style style
          #:label (~a (qtr->year (first year)))))
 
 "/tmp/full.svg")

(define zoomed-qtrs-only
  (for/list ([yr-data (in-list fall-years)])
    (list (first yr-data)
          (filter (λ (p) (< (first p) (add1 zoomed-qtrs-shown)))
                  (second yr-data)))))

(define (plot-some-years pred #:file [maybe-filename #f])
  (define the-lines
    (for/list ([year (in-list (filter (λ (yr) (pred (first yr))) zoomed-qtrs-only))]
               [color (in-naturals)]
               [style (in-naturals)]
               )
      (define p (cons (list 0 0) (second year)))
      (lines p
             #:color color
             #:style style
             #:label (~a (qtr->year (first year))))))
  (cond [maybe-filename
         (plot-file the-lines
                    #:y-max 1.0
                    #:title (~a "students passing " coursenum", trimmed")
                    #:x-label "elapsed qtrs (omitting summer)"
                    #:y-label "fraction that have passed the class"

                    maybe-filename)]
        [else
         (plot the-lines
               #:title (~a "students passing " coursenum", trimmed")
               #:x-label "elapsed qtrs (omitting summer)"
               #:y-label "fraction that have passed the class"
               #:y-max 1.0)]))

(plot-some-years (λ (x) x))
(plot-some-years (λ (yr) (< yr 2108)))
(plot-some-years (λ (yr) (>= yr 2108)))

(plot
 #:y-min 0
 #:y-max 1
(lines
(for/list ([yr-data zoomed-qtrs-only]
           #:when (= (first (last (second yr-data))) 9))
  (list (first yr-data) (second (last (second yr-data)))))))





