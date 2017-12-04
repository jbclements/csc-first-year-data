#lang racket/base

(require sqlite-table
         "student-data.rkt"
         "grades.rkt"
         math/statistics
         racket/contract
         racket/list
         racket/match)

;; provide a list of all student tracks

(provide
 (contract-out
 [student-tracks
  (listof
   (listof
    (list/c natural?
            (listof (vector/c classname? (symbols 'pass 'nopass))))))]))

(define natural? exact-nonnegative-integer?)
(define classname? (symbols 'cpe123 'cpe101 'cpe102 'cpe103))

(define (string->classname str)
  (match str
    ["CPE 123" 'cpe123]
    ["CPE 101" 'cpe101]
    ["CPE 102" 'cpe102]
    ["CPE 103" 'cpe103]))

(define (vref idx)
  (λ (v) (vector-ref v idx)))

(define tracks
  (group-by
   (vref 0)
   (table-select pre-2152-grade-table
                 #;grade-facts-table
                 '(student qtr class grade))))

(define (grade-map g)
  (cond [(member g success-grades)
         'pass]
        [else 'nopass]))

;; yikes, there are some weird things going on with summer quarter.
#;(filter
 (λ (t)
   (ormap (λ (r) (= (modulo ((vref 1) r) 10) 6)) t))
 tracks)

(define (qtr->ord qtr)
  (* 3 (- (floor (/ qtr 10)) 200))
  (match (modulo qtr 10)
    [2 0]
    [4 1]
    [8 2]))

;; dealing with quarters (e.g.: did the student skip a quarter?)
;; in this data is too difficult, because of summer and other
;; factors.

(define student-tracks
  (for/list ([t (in-list tracks)])
    (define grouped-by-qtr (group-by (vref 1) t))
    (define qtr-records (sort grouped-by-qtr < #:key (λ (g) ((vref 1) (first g)))))
    #;(define first-qtr
      (qtr->ord ((vref 1) (first (first qtr-records)))))
    (for/list ([qtr (in-list qtr-records)])
      (list
       ((vref 1) (first qtr))
       (for/list ([r (in-list qtr)])
         (vector (string->classname ((vref 2) r))
                 (grade-map ((vref 3) r))))))))

#;(define h
  (samples->hash g))

#;(define by-last
  (group-by (λ (x) (last x)) g))

#;(sort
   (map (λ (g) (list (length g) (last (first g))))
        by-last)
   > #:key first)

#;(length g)

