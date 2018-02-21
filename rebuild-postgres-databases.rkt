#lang racket

;; build a tsv of the grades from the first-year dataset

(require csv-reading
         racket/block
         csse-scheduling/canonicalize
         csse-scheduling/emplid-hasher
         csse-scheduling/qtr-math
         rackunit)

;; we don't have canonical names for classes before this. We would
;; need old catalogs to generate this info.
(define oldest-canonical-class-qtr 2078)



;; strip a leading byte order mark from an input port
;(: discard-bom (Input-Port -> Void))
(define (discard-bom p)
  (void (regexp-try-match #rx"^\uFEFF" p)))

(define expected-columns
  '("FIRST_NAME"
  "LAST_NAME"
  "EMPLID"
  "FIRST_TERM"
  "FIRST_MAJOR"
  "FIRST_CLASS_LEVEL"
  "LAST_TERM"
  "LAST_MAJOR"
  "LAST_TERM"
  "LAST_CLASS_LEVEL"
  "TERM_CODE"
  "CPE_CLASS"
  "GRADE"
  "TEST"
  "SCORE"))

(define course-key "CPE_CLASS")
(define qtr-key "TERM_CODE")

;; given a row and a column name, return the corresponding value:
(define col-ref
  (let ([table
         (for/hash ([i (in-naturals)]
                    [name (in-list expected-columns)])
           (values name i))])
    (λ (row name)
      (vector-ref row (hash-ref table name)))))

(define row-term (λ (row) (col-ref row qtr-key)))

(define rows
  (let ()
    (define d
      (call-with-input-file
          (build-path 
           "/Users/clements/clements/datasets/"
           "student-data-2052-2174.csv")
        (λ (port)
          (discard-bom port)
          (csv->list port))))

    
    (unless (equal? (first d)
                    expected-columns)
      (error 'unexpected-columns))

    (map list->vector (rest d))))


;; some rows aren't actual classes, but are apparently
;; placeholders for things like "Affiliated Programs Outgoing"
;; and "Department Exchange". These correspond to subject labels
;; like "AP" and "DE". I don't have a complete list of these, sigh.
(define-values (non-class-rows class-rows)
  (partition (λ (row) (regexp-match #px"^(AP|DE) "
                                    (col-ref row course-key)))
             rows))

(when (not (empty? non-class-rows))
  (define non-class-quarters
    (remove-duplicates (map row-term non-class-rows)))

  (when (< 1 (length non-class-quarters))
    (error 'non-classes
           "more than one quarter contains non-class rows:\n~e"
           non-class-rows))
  (fprintf (current-error-port)
           "ignoring these non-class rows from ~v:\n"
           (first non-class-quarters))
  (for ([row (in-list non-class-rows)])
    (fprintf (current-error-port) " ~v\n" row)))

;; split the rows that have grades (presumably completed
;; courses) from the rows that don't (presumably poly planner
;; quarters)
(define-values (poly-planner-rows past-grade-rows)
  (partition (λ (row) (equal? (col-ref row "GRADE") ""))
             class-rows))

;; polyplanner quarters
(define poly-planner-quarters
  (remove-duplicates (map (λ (row) (string->number (col-ref row qtr-key)))
                          poly-planner-rows)))
(printf "poly planner quarters: ~v\n" (sort poly-planner-quarters <))

(define have-grades-quarters
  (remove-duplicates (map (λ (row) (string->number (col-ref row qtr-key)))
                          past-grade-rows)))

(printf "quarters with grades: ~v\n" (sort have-grades-quarters <))


;; the last quarter that has a grade, used for the version number
;; below
(define last-grade-quarter
  (apply max have-grades-quarters))

(define version-str (~a last-grade-quarter "-1"))

(when (not (set-empty?
            (set-intersect
             (list->set poly-planner-quarters)
             (list->set have-grades-quarters))))
  (error 'quarter-overlap
         "these quarters have grade data and also poly planner data:~v"
         (set-intersect
          (list->set poly-planner-quarters)
          (list->set have-grades-quarters))))



;; given a string such as "CPE  308", return (list "CPE" "308")
(define (split-course s)
  (match (regexp-split #px" +" s)
    [(list subj num) (list subj num)]
    [_ (raise-argument-error 'split-course
                             "subject-space-num^ string"
                             0 s)]))

(define (parse-grad-qtr str)
  (match str
    ["" "NONE"]
    [(regexp #px"^([0-9]{4}) (Spr|Fall|Wtr|Sum)" (list _ year season))
     (define season-str
       (match season
         ["Wtr" "Winter"]
         ["Spr" "Spring"]
         ["Sum" "Summer"]
         ["Fall" "Fall"]))
     (number->string (encode-qtr (string->number year) season-str))]))

(check-equal? (parse-grad-qtr "2017 Sum") "2176")
(check-equal? (parse-grad-qtr "") "NONE")

;; given a filename and a list of values formattable with ~a,
;; output a tsv file suitable for import with postgresql
(define (postgresql-format filename rows)
  (call-with-output-file filename
    (λ (port)
      (for ([row (in-list rows)])
        (define row+version (append row (list version-str)))
        (fprintf port
                 "~a\n" 
                 (apply string-append
                        (add-between (map ~a row+version) "\t")))))))


;; GENERATE RAW GRADE TABLE
;; this doesn't use canonical course ids, but it does include
;; courses outside our department, and it includes units_attempted
;; and units_earned
(define (create-raw-grade-table)
  (postgresql-format
   "/tmp/raw_grade.tsv"
   (for/list ([row (in-list past-grade-rows)])
     (append (list (emplid->hashed (col-ref row "EMPLID"))
                   (string->number (col-ref row qtr-key)))
             (split-course (col-ref row course-key))
             (list
              ;; these aren't part of the first-year dataset:
              ;(inexact->exact (round (* 1000 (string->number (col-ref row "UNITS_ATTEMPTED")))))
              ;(inexact->exact (round (* 1000 (string->number (col-ref row "UNITS_EARNED")))))
              (col-ref row "GRADE"))))))

;; GENERATING GRADE TABLE:
;; effect: overwrites /tmp/progress.csv
(define (create-grade-table)
  ;; returns (List Id Qtr Course Grade)
  (define have-grades
    (for/list ([row (in-list past-grade-rows)])
      (list (emplid->hashed (col-ref row "EMPLID"))
            (string->number (col-ref row qtr-key))
            (col-ref row course-key)
            (inexact->exact (round (* 1000 (string->number (col-ref row "UNITS_ATTEMPTED")))))
            (inexact->exact (round (* 1000 (string->number (col-ref row "UNITS_EARNED")))))
            (col-ref row "GRADE"))))

  ;; returns (List Id Qtr Course)
  (define no-grades
    (for/list ([row (in-list poly-planner-rows)])
      (list (emplid->hashed (col-ref row "EMPLID"))
            (string->number (col-ref row qtr-key))
            (col-ref row course-key))))

  ;; very old records:
  (define old-records
    (filter (λ (row) (< (string->number (row-term row)) oldest-canonical-class-qtr))
            past-grade-rows))

  (printf "there are ~v rows older than ~v.\n"
          (length old-records)
          oldest-canonical-class-qtr)

  
  (define old-timer-ids
    (remove-duplicates (map (λ (row) (emplid->hashed
                                      (col-ref row "EMPLID")))
                            old-records)))

  (define old-timers
    (remove-duplicates
     (map (λ (row) (list (col-ref row "FIRST_NAME")
                         (col-ref row "LAST_NAME")))
          old-records)))
  
  (when (not (empty? old-timer-ids))
    (printf "WARNING: discarding ids of \
students with classes taken before ~v:\n"
            oldest-canonical-class-qtr)
    (pretty-print old-timers))

  ;; rows of students that aren't ridiculously old
  (define not-ancient-rows
    (filter (λ (row) (not (member (first row) old-timer-ids)))
            have-grades))

  ;; entry-qtrs.csv
  (block
   (define all-qtrs
     (remove-duplicates
      (map (λ (row) (list (first row) (second row)))
           have-grades)))
   
   ;; the first quarter for which the student has a cal poly grade:
   (define entry-qtrs
     (map (λ (g) (list (first (first g)) (apply min (map second g))))
          (group-by first all-qtrs)))

   (postgresql-format "/tmp/entry-qtrs.tsv" entry-qtrs))

  ;; generate progress.csv (past grades)
  (block
    (define past-courses
    (remove-duplicates
     (map (λ (l) (list (second l)
                       (split-course (third l))))
          not-ancient-rows)))

  ;; this is a map from (list qtr (list subject num)) to course-id.
    (define past-course-map
      (for/hash ([c (in-list past-courses)])
        (values c
                (let ()
                  (define subj (first (second c)))
                  (define coursenum (second (second c)))
                  (define coursenum/t
                    (match coursenum
                      ;; the "P" courses are associated with
                      ;; the Open University program.
                      [(regexp #px"^P\\d{3}" (list _))
                       (substring coursenum 1)]
                      ;; this one I'm not so sure about...
                      [(regexp #px"^S\\d{3}" (list _))
                       (substring coursenum 1)]
                      [else coursenum]))
                  (match (list subj coursenum/t)
                    [(list _ (regexp #px"^[245]IP"))
                     ;; these are study-abroad courses. ignore
                     ;; them for now.
                     (fprintf (current-error-port)
                              "ignoring study-abroad class: ~v\n"
                              c)
                     #f]
                    [(list "CSC" "X171")
                     ;; this is before it appeared in the catalog
                     (canonicalize "2009-2011"
                                   subj
                                   (substring coursenum/t 1))
                     ]
                    ;; for all of these courses, the student receives the same
                    ;; credit that they would for the course with the first
                    ;; character removed from the coursenum.
                    [(or (list "CSC" "S490")
                         (list "CPE" "X105")
                         (list "CSC" "X171"))
                     (canonicalize/qtr (first c)
                                       subj
                                       (substring coursenum/t 1))]
                    [_
                     (define course-id
                       (canonicalize/qtr/noerr
                        (first c)
                        subj
                        coursenum/t))
                     (when (and (not course-id)
                                (member subj '("CPE" "CSC" "DATA")))
                       ;; not signalling an error so you can see all
                       ;; of them at once:
                       (fprintf (current-error-port)
                                "ERROR: no course id found for: ~v\n"
                                c))
                     course-id])))))
    
    ;; use the canonical name map to supply ids for all of the courses
    ;; that are ours, discard others.
    (define out-data
      (for/list ([row (in-list not-ancient-rows)]
                 #:when (hash-ref past-course-map
                                  (list (second row) (split-course (third row)))))
        (list (first row)
              (second row)
              (hash-ref past-course-map
                        (list (second row) (split-course (third row))))
              (fourth row)
              (fifth row)
              (sixth row))))

    (pretty-display (take out-data 15))
    (newline)

    (check-duplicates out-data)

    (postgresql-format "/tmp/progress.tsv" out-data))

  (let ()
    ;; future quarters
    (define poly-planner-course-map
      (let ()
        (define future-courses
          (remove-duplicates
           (map (λ (l) (list (second l)
                             (split-course (third l))))
                no-grades)))
      
        ;; this is a map from (list qtr (list subject num)) to course-id.
        (for/hash ([c (in-list future-courses)])
          (match-define (list qtr (list subj num)) c)
          (values c
                  (match subj
                    ["CSC"
                     (match (canonicalize/qtr/noerr qtr "CSC" num)
                       ;; sigh, try CPE:
                       [#f (match (canonicalize/qtr/noerr qtr "CPE" num)
                             ;; giving up
                             [#f
                              (fprintf
                               (current-error-port)
                               "poly-planner-fail: no course id found for: ~v\n"
                               c)
                              #f]
                             [course-id
                              (fprintf
                               (current-error-port)
                               "warning, using subject CPE for polyplanner ~e\n"
                               c)
                              course-id])]
                       [course-id course-id])]
                    ["CPE"
                     (match (canonicalize/qtr/noerr qtr "CPE" num)
                       ;; sigh, try CPE:
                       [#f (match (canonicalize/qtr/noerr qtr "CSC" num)
                             ;; giving up
                             [#f
                              (fprintf
                               (current-error-port)
                               "poly-planner-fail: no course id found for: ~v\n"
                               c)
                              #f]
                             [course-id
                              (fprintf
                               (current-error-port)
                               "warning, using subject CSC for polyplanner ~e\n"
                               c)
                              course-id])]
                       [course-id course-id])]
                    [(or "AG")
                     ;; ?? these departments don't even exist
                     (fprintf
                      (current-error-port)
                      "poly-planner-fail: nonexistent department for course: ~v\n"
                      c)
                     #f
                     ]
                    [other (canonicalize/qtr/noerr qtr subj num)])))))
    
    (define poly-planner-out-data
      (for/list ([row (in-list no-grades)]
                 #:when (hash-ref poly-planner-course-map
                                  (list (second row)
                                        (split-course (third row)))))
        (list (first row)
              (second row)
              (hash-ref poly-planner-course-map
                        (list (second row) (split-course (third row)))))))

    (postgresql-format "/tmp/poly-planner.tsv" poly-planner-out-data)))

(define (generate-majors-data)

  (define initial-data
    (remove-duplicates
     (for/list ([row (in-list rows)])
       (list (emplid->hashed (col-ref row "EMPLID"))
             (col-ref row "MAJOR")))))


  (block
   (define non-uniform-students
     (filter (λ (g) (< 1 (length g)))
             (group-by first initial-data)))
   (unless (empty? non-uniform-students)
     (error 'majors
            "expected major and expected graduation to be the same for \
every entry for a student in the ad-hoc, query, given:"
            non-uniform-students)))

  (postgresql-format "/tmp/majors.tsv" initial-data))

(define (generate-expected-grad-data)

  (define initial-data
    (remove-duplicates
     (for/list ([row (in-list rows)])
       (list (emplid->hashed (col-ref row "EMPLID"))
             (parse-grad-qtr (col-ref row "EXPECTED_GRAD_TERM"))))))


  (block
   (define non-uniform-students
     (filter (λ (g) (< 1 (length g)))
             (group-by first initial-data)))
   (unless (empty? non-uniform-students)
     (error 'majors
            "expected major and expected graduation to be the same for \
every entry for a student in the ad-hoc, query, given:"
            non-uniform-students)))

  (postgresql-format "/tmp/grad-dates.tsv" initial-data))

(define (generate-ids)
  (define initial-ids-data
    (remove-duplicates
     (for/list ([row (in-list rows)])
       (list (emplid->hashed (col-ref row "EMPLID"))
             (col-ref row "EMPLID")
             (col-ref row "FIRST_NAME")
             (col-ref row "LAST_NAME")
             version-str))))

  (block
   (define hash-collisions
     (filter (λ (g) (< 1 (length g)))
             (group-by first initial-ids-data)))
   (unless (empty? hash-collisions)
     (error 'majors
            "anotuhh.h.ntoh.nh ah no! ~v"
            hash-collisions)))

  (postgresql-format "/tmp/ids.tsv" initial-ids-data))



