#lang racket

(require "animation-back-end.rkt"
         sqlite-table
         "student-data.rkt"
         "grades.rkt"
         csse-scheduling/qtr-math
         rackunit)


;; a situation is one of
;; - 'pre-start
;; - (progress class grade active?)
;; - 'post-finish
(struct progress (class grade active?) #:transparent)

;; a fact represents a grade awarded
(struct fact (qtr class grade) #:transparent)

;; a trace contains a student and a list of facts sorted by date
(struct trace (student facts) #:transparent)

;; a full-trace is a name and a list of situations representing a
;; map from qtr-idx to situation
;; from earliest-qtr to last-qtr. In other words, *all* quarters
;; are represented, and each quarter contains information about
;; at most one class.
(struct full-trace (student situations) #:transparent)

#;(table-select grade-facts-table '(student qtr class grade)
              #:where '((= student "jkim165@calpoly.edu")))

(define data
  (table-select grade-facts-table '(student qtr class grade)))

(define grouped
  (group-by (λ (d) (vector-ref d 0)) data))



(define traces
  (for/list ([group (in-list grouped)])
    (trace
     (vector-ref (first group) 0)
     (sort
      (for/list ([record (in-list group)])
        (match-define (vector name qtr class grade) record)
        (fact (string->number qtr) class grade))
      <
      #:key fact-qtr))))

(define all-facts (apply append (map trace-facts traces)))

(define cohorts (group-by (λ (t) (fact-qtr (first (trace-facts t))))
                          traces))


(sort
 (for/list ([c (in-list cohorts)])
   (list (fact-qtr (first (trace-facts (first c))))
         (length c)))
 <
 #:key first)

(define class-ordering
  '("CPE 123" "CPE 101" "CPE 102" "CPE 103"))

(define class-ordering-map
  (for/hash ([class (in-list class-ordering)]
             [i (in-naturals)])
    (values class i)))


;; earliest qtr whose students won't already have graduated:
#;(define earliest-qtr 2124 #;(apply min (map fact-qtr all-facts)))
(define latest-qtr (apply max (map fact-qtr all-facts)))


;; for traces that include more than one class in a given
;; quarter, strip out all but the last and print warning
;; messages
(define ((trace-pick earliest-qtr) t)
  ;; is this quarter before the earliest-qtr we care about?
  (define (too-early-qtr? s)
    (< s earliest-qtr))
  (trace
   (trace-student t)
   (let loop ([facts (trace-facts t)])
     (match facts
       ['() '()]
       ;; special case too-early quarters
       [(cons (fact (? too-early-qtr?) _ _) rest)
        (loop rest)]
       [(cons (fact qtr _ _) _)
        (define-values (this-qtr-facts others)
          (partition (λ (fact) (= (fact-qtr fact) qtr)) facts))
        (define reverse-ordered-facts
          (reverse
           (sort this-qtr-facts <
                 #:key (λ (fact)
                         (hash-ref class-ordering-map (fact-class fact))))))
        (when (not (empty? (rest reverse-ordered-facts)))
          (printf "WARNING: ignoring all but the first of this set of classes taken in the same quarter:\n~a\n"
                  reverse-ordered-facts))
        (cons (first reverse-ordered-facts) (loop others))]))))

(define (trim-traces traces earliest-qtr)
  (filter (λ (trace) (not (empty? (trace-facts trace))))
          (map (trace-pick earliest-qtr) traces)))


;; a qtr-index is an integer indicating an element of the all-qtrs
;; so, for instance, if the first quarter is 2094, then index 1
;; would refer to 2098

;; map a trace to a full-trace
(define ((trace->full-trace earliest-qtr latest-qtr) orig-trace)
  ;; I want for/list/accum again!
  (define mutable-accum (list #f (trace-facts orig-trace)))
  (full-trace
   (trace-student orig-trace)
   (for/list ([qtr (in-list (qtrs-in-range earliest-qtr latest-qtr #:include-summer #t))]
              [qtr-idx (in-naturals)])
     (match-define (list last-fact facts) mutable-accum)
     (match facts
       ['()
        (match last-fact
          [#f
           (error 'trace->full-trace
                  "finished with no last-fact? : ~e"
                  orig-trace)]
          [(fact _ class grade)
           (define situation
             (cond [(finished? last-fact)
                    'post-finish]
                   [else
                    (progress class grade #f)]))
           (set! mutable-accum (list last-fact facts))
           situation])]
       [(cons (fact fqtr fclass fgrade) trace-rest)
        (cond [(< fqtr qtr)
               (error 'trace->full-trace
                      "bad quarter sequencing:\n~e\n"
                      orig-trace)]
              [(= fqtr qtr)
               (set! mutable-accum (list (fact fqtr fclass fgrade)
                                         trace-rest))
               (progress fclass fgrade #t)]
              [(> fqtr qtr)
               (set! mutable-accum (list last-fact facts))
               (match last-fact
                 [#f 'pre-start]
                 [(fact lqtr lclass lgrade)
                  (progress lclass lgrade #f)])])]))))



;; did this student successfully complete 103 with a C- or better?
(define (finished? f)
  (match-define (fact qtr class grade) f)
  (and (equal? class "CPE 103")
       (member grade success-grades)
       #t))




;; find traces that move backward...
;; ... actually, these generally look pretty mellow.
#;(for ([t (in-list (apply append full-traces-by-cohort))]
      [i (in-naturals)])
  (match-define (full-trace student situations) t)
  (for ([situation-a (in-list situations)]
        [situation-b (in-list (rest situations))])
    (match (list situation-a situation-b)
      [(list (progress class-a _ _) (progress class-b _ _))
       (when (< (hash-ref class-ordering-map class-b)
                (hash-ref class-ordering-map class-a))
         (printf "YIKES: trace for ~a moves backward:\n ~v\n"
                 student t))]
      [other 'do-nothing])))



;; map a pair of per-qtr records to a transition
(define (2qtrs->transition title a b)
  (unless (= (length a) (length b))
    (raise-argument-error 2qtrs->transition
                          "lists of same length"
                          1 a b))
  (transition
   title
   (for/list ([from (in-list a)]
              [to (in-list b)]
              [id (in-naturals)]
              #:when (onscreen? from to))
     (shapetrans 'dummy
                 (situation-posn from id)
                 (situation-posn to id)))))

;; what position is this situation drawn in?
(define (situation-posn situation id)
  (match situation
    ['pre-start OFF-LEFT-POSN]
    [(progress class grade _)
     (class/grade-posn class grade id)]
    ['post-finish OFF-RIGHT-POSN]))


;; is this transition visible?
;; (specifically, transitions from pre-start to pre-start are
;; not visible, and neither are post-finish to post-finish)
(define (onscreen? from to)
  (match (list from to)
    [(list 'pre-start 'pre-start) #f]
    [(list 'post-finish 'post-finish) #f]
    [other #t]))




;; finish printing before running:

(module+ main
  (define first-qtr 2058)
  (define cohort-qtrs (qtrs-in-range first-qtr 2162)
    #;(map (λ (o) (+ first-qtr o))
           '(0 4 6 10 14 16
               20 24 26 30 34 36)))
  (define cohort-traces
    (filter (λ (t) (member (fact-qtr (first (trace-facts t)))
                           cohort-qtrs))
            traces))
  (define trimmed-traces (trim-traces cohort-traces
                                      first-qtr))
  ;; start the animation early to show them entering:
  (define before-first-qtr (- first-qtr 4))
  (define full-traces
    (map (trace->full-trace before-first-qtr latest-qtr) trimmed-traces))
  ;; transpose the matrix to obtain a per-qtr record:
  (define qtr-situations
    (apply map list (map full-trace-situations
                         full-traces)))
  (define transes
    (for/vector ([a (in-list qtr-situations)]
               [b (in-list (rest qtr-situations))]
               [tgt-qtr (in-list (rest (qtrs-in-range before-first-qtr
                                                 latest-qtr)))])
      (2qtrs->transition (~a "-> "tgt-qtr) a b)))
  (run-transes transes))

(module+ test
(check-equal? (finished? (fact 2154 "CPE 102" "A-")) #f)
(check-equal? (finished? (fact 2154 "CPE 103" "WU")) #f)
(check-equal? (finished? (fact 2154 "CPE 103" "B-")) #t)
  (check-equal?
 (2qtrs->transition "zoomba"
                    (list 'pre-start
                          'pre-start
                          'pre-start
                          (progress "CPE 101" "WU" #t)
                          (progress "CPE 101" "C" #f)
                          'post-finish
                          (progress "CPE 101" "B" #t)
                          (progress "CPE 103" "C" #t))
                    (list 'pre-start
                          'post-finish
                          (progress "CPE 123" "B+" #t)
                          (progress "CPE 101" "C" #t)
                          (progress "CPE 102" "B" #t)
                          'post-finish
                          (progress "CPE 101" "B" #f)
                          'post-finish))
 (transition
  "zoomba"
  (list (shapetrans 'dummy OFF-LEFT-POSN OFF-RIGHT-POSN)
        (shapetrans 'dummy OFF-LEFT-POSN (class/grade-posn "CPE 123" "B+" 2))
        (shapetrans 'dummy
                    (class/grade-posn "CPE 101" "WU" 3)
                    (class/grade-posn "CPE 101" "C" 3))
        (shapetrans 'dummy
                    (class/grade-posn "CPE 101" "C" 4)
                    (class/grade-posn "CPE 102" "B" 4))
        ;; elided here
        (shapetrans 'dummy
                    (class/grade-posn "CPE 101" "B" 6)
                    (class/grade-posn "CPE 101" "B" 6))
        (shapetrans 'dummy
                    (class/grade-posn "CPE 103" "C" 7)
                    OFF-RIGHT-POSN)))))

