#lang racket

;; building a database from the chadwell-style data.
;; sadly, it doesn't include failing grades, and thus isn't helpful
;; for first-year statistics.
7
(require csv-reading
         csse-scheduling/canonicalize
         "emplid-hasher.rkt")

(define d
  (call-with-input-file "/Users/clements/clements/datasets/ad-hoc-student-progress/student-progress-data.csv"
    csv->list))

(define rows (rest d))

(unless (equal? (first d)
                '("\uFEFFMAJOR"
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

(define (split-course s)
  (regexp-split #px" +" s))

(define courses
  (remove-duplicates
   (map (λ (l) (list (second l)
                     (split-course (third l))))
        initial-data)))

#;(call-with-output-file "/tmp/gg.rktd"
  (λ (port) (write courses port)))


(define canonical-map
  (time
   (for/list ([c (in-list courses)])
     (list c
           (with-handlers ([exn:fail?
                            (λ (exn) #f)])
             (define subj (first (second c)))
             (cond [(subject? subj)
                    (canonicalize/qtr (first c)
                                      (first (second c))
                                      (second (second c)))]
                   [else #f]))))))

(filter (λ (map-elt)
          (and (not (second map-elt))
               (member (first (second (first map-elt)))
                       '("CSC"))))
        canonical-map)

;; these are presumably the polyplanner data
(define no-grades
  (filter (λ (row) (equal? (fourth row) ""))
          initial-data))

(remove-duplicates (map second no-grades))





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







