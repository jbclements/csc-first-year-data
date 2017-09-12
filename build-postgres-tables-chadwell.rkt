#lang racket

;; building a database from the chadwell-style data.
;; sadly, it doesn't include failing grades, and thus isn't helpful
;; for first-year statistics.

(require csv-reading
         csse-scheduling/canonicalize
         "emplid-hasher.rkt")

;; we don't have canonical names for classes before this. We would
;; need old catalogs to generate this info.
(define oldest-canonical-class-qtr 2078)

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
                      (add-between (map ~a row) "\t"))))))

#;((define courses-split
  (map (λ (r)
         (append (take r 2)
                 (split-course (third r))
                 (drop r 3)))
       deduped))

(unless (= 0 (length (filter (λ (r)
                               (ormap (λ (cell) (equal? cell "")) r))
                             coursees-split)))
  (error 'blanks))


(call-with-output-file "/tmp/zz.txt"
  #:exists 'truncate
  (λ (port)
    (for ([d (in-list courses-split)])
    (apply fprintf port "~a\t~a\t~a\t~a\t~a\n"
           d)))))







