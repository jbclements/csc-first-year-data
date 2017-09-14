#lang racket

;; building a database from the chadwell-style data.
;; sadly, it doesn't include failing grades, and thus isn't helpful
;; for first-year statistics.

(require csv-reading
         racket/block
         csse-scheduling/canonicalize
         "emplid-hasher.rkt")

;; we don't have canonical names for classes before this. We would
;; need old catalogs to generate this info.
(define oldest-canonical-class-qtr 2078)

;; the table contains majors, which are presumably current as of
;; the last quarter with a grade. (A bit dangerous here.)
(define major-quarter 2172)

;; strip a leading byte order mark from an input port
;(: discard-bom (Input-Port -> Void))
(define (discard-bom p)
  (void (regexp-try-match #rx"^\uFEFF" p)))

(define d
  (call-with-input-file "/Users/clements/clements/datasets/ad-hoc-student-progress/student-progress-data.csv"
    (λ (port)
      (discard-bom port)
      (csv->list port))))

(define rows (rest d))

(unless (equal? (first d)
                '("MAJOR"
                  "EMPLID"
                  "FIRST_NAME"
                  "LAST_NAME"
                  "EMAIL_ADDRESS"
                  "CONCENTRATION"
                  "TERM"
                  "COURSE"
                  "UNITS_ATTEMPTED"
                  "UNITS_EARNED"
                  "GRADE"))
  (error 'unexpected-columns))

;; GENERATING GRADE TABLE:
(define (create-grade-table)
(define initial-data
  (for/list ([row (in-list rows)])
    (list (emplid->hashed (list-ref row 1))
          (string->number (list-ref row 6))
          (list-ref row 7)
          (list-ref row 10))))

;; very old records:
(define old-records
  (filter (λ (row) (< (second row) oldest-canonical-class-qtr))
          initial-data))

old-records

(define old-ids
  (remove-duplicates (map first old-records)))

(when (not (empty? old-ids))
  (printf "WARNING: discarding ids of students with classes taken before ~v:\n"
          oldest-canonical-class-qtr)
  (pretty-print old-ids))

;; given a string such as "CPE  308", return (list "CPE" "308")
(define (split-course s)
  (match (regexp-split #px" +" s)
    [(list subj num) (list subj num)]
    [_ (raise-argument-error 'split-course
                             "subject-space-num string"
                             0 s)]))

(define not-ancient-rows
  (filter (λ (row) (not (member (first row) old-ids)))
          initial-data))

(define courses
  (remove-duplicates
   (map (λ (l) (list (second l)
                     (split-course (third l))))
        not-ancient-rows)))

#;(call-with-output-file "/tmp/gg.rktd"
  (λ (port) (write courses port)))


(define canonical-map
  (time
   (for/hash ([c (in-list courses)])
     (values c
             (with-handlers ([exn:fail?
                              (λ (exn) #f)])
               (define subj (first (second c)))
               (define coursenum (second (second c)))
               ;; special cases, sigh
               (match (list subj coursenum)
                 [(or (list "CPE" "P400")
                      (list "CPE" "P329")
                      (list "CSC" "S490")
                      (list "CPE" "X105")
                      (list "CPE" "P315")
                      (list "CPE" "P225")
                      (list "CPE" "P349"))
                  (canonicalize/qtr (first c)
                                    subj
                                    (substring coursenum 1))]
                 [_
                  (cond [(subject? subj)
                         (canonicalize/qtr (first c)
                                           subj
                                           coursenum)]
                        [else #f])]))))))

;; those with empty strings for grades are presumably polyplanner data
(define-values (no-grades have-grades)
  (partition (λ (row) (equal? (fourth row) ""))
             not-ancient-rows))

;; polyplanner quarters
(define poly-planner-quarters
  (remove-duplicates (map second no-grades)))
(printf "poly planner quarters: ~v\n" poly-planner-quarters)

(define have-grades-quarters
  (remove-duplicates (map second have-grades)))

(when (not (set-empty?
            (set-intersect
             (list->set poly-planner-quarters)
             (list->set have-grades-quarters))))
  (error 'quarter-overlap
         "these quarters have grade data and also poly planner data:~v"
         (set-intersect
             (list->set poly-planner-quarters)
             (list->set have-grades-quarters))))


(define lookup-fails
  (filter (λ (map-elt)
            (and (not (cdr map-elt))
                 (member (first (second (car map-elt)))
                         '("CSC" "CPE" "DATA"))))
          (hash->list canonical-map)))

;; past records without canonical names:
(define missing-classes
  (filter (λ (fail)
            (member (first (car fail)) have-grades-quarters))
          lookup-fails))

(when (not (empty? missing-classes))
  (error 'missing-mappings
         "missing canonical mappings: ~e"
         missing-classes))

;; use the canonical name map to supply ids for all of the courses
;; that are ours, discard others.
(define out-data
 (for/list ([row (in-list have-grades)]
            #:when (hash-ref canonical-map
                             (list (second row) (split-course (third row)))))
   (list (first row)
         (second row)
         (hash-ref canonical-map
                   (list (second row) (split-course (third row))))
         (fourth row))))

(take out-data 15)

(check-duplicates out-data)

(call-with-output-file "/tmp/progress.csv"
  (λ (port)
    (for ([row (in-list out-data)])
      (fprintf port
               "~a\n" 
               (apply string-append
                      (add-between (map ~a row) "\t")))))))

(define (generate-majors-data)
(define initial-majors-data
  (remove-duplicates
   (for/list ([row (in-list rows)])
     (list (emplid->hashed (list-ref row 1))
           major-quarter
           (list-ref row 0)))))


(block
 (define students-with-more-than-one-major
   (filter (λ (g) (< 1 (length g)))
           (group-by first initial-majors-data)))
 (unless (empty? students-with-more-than-one-major)
   (error 'majors
          "expected major to be the same for every entry for a student in the ad-hoc, query, given:"
          students-with-more-than-one-major)))

(call-with-output-file "/tmp/majors.csv"
  (λ (port)
    (for ([row (in-list initial-majors-data)])
      (fprintf port
               "~a\n" 
               (apply string-append
                      (add-between (map ~a row) "\t")))))))

(define initial-ids-data
  (remove-duplicates
   (for/list ([row (in-list rows)])
     (list (emplid->hashed (list-ref row 1))
           (list-ref row 1)
           (list-ref row 2)
           (list-ref row 3)))))

(block
 (define hash-collisions
   (filter (λ (g) (< 1 (length g)))
           (group-by first initial-ids-data)))
 (unless (empty? hash-collisions)
   (error 'majors
          "anotuhh.h.ntoh.nh ah no! ~v"
          hash-collisions)))


(call-with-output-file "/tmp/ids.csv"
  (λ (port)
    (for ([row (in-list initial-ids-data)])
      (fprintf port
               "~a\n" 
               (apply string-append
                      (add-between (map ~a row) "\t"))))))

;; one-off script to find SE majors that haven't taken 308:

(define uh-ohs
  (map first
       (map vector->list
  '(#("9725b6895d4e080aa059" 2148)
    #("c230b7f86bb774e8345e" 2148)
    #("d32655f75f100cdb7583" 2138)
    #("0726d039d24804b3d6b4" 2148)
    #("6d33d90a2221be24fd7f" 2148)
    #("6d1f0b36bc5713a12c33" 2148)
    #("bebb536c7ab72ed66fdb" 2138)
    #("6a35a4d7438a581a9b16" 2148)
    #("35afb665e1f4e054f344" 2098)
    #("c24667432c9d433e2e95" 2148)
    #("147b35e86da258524cfe" 2148)
    #("90379779561eed9129d9" 2148)
    #("fa6edfa4e7835f027e02" 2154)
    #("6b8d7b9d748714758116" 2148)
    #("b6d43c90166df744082c" 2152)
    #("2f47c425c51c96350b88" 2148)
    #("0870d94608ae12e371d8" 2148)
    #("73353388b9b6040d5a03" 2148)
    #("ec5391831d0ba0062206" 2138)
    #("5ddf0d714dae3abf0f4f" 2138)
    #("f780d04136cbf3a3e2aa" 2148)
    #("26d74fe1ac52a6bffbda" 2148)))))

(filter (λ (row) (member (first row) uh-ohs)) initial-ids-data)

