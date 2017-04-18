#lang racket

(require csv-reading
         "emplid-hasher.rkt")

(define d
  (call-with-input-file "/Users/clements/clements/datasets/student-data-2052-2164.csv"
    csv->list))

(define rows (rest d))

(unless (equal? (first d)
                '("\uFEFFFIRST_NAME"
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
                  "TERM_CODE"
                  "CPE_CLASS"
                  "GRADE"
                  "TERM_CODE"
                  "CPE_CLASS"
                  "GRADE"
                  "TERM_CODE"
                  "CPE_CLASS"
                  "GRADE"
                  "TERM_CODE"
                  "CPE_CLASS"
                  "GRADE"
                  "TERM_CODE"
                  "CPE_CLASS"
                  "GRADE"
                  "TERM_CODE"
                  "CPE_CLASS"
                  "GRADE"
                  "TERM_CODE"
                  "CPE_CLASS"
                  "GRADE"
                  "TEST"
                  "SCORE"))
  (error 'unexpected-columns))

(drop (first d) 10)

(define grades-offset 10)
(define grade-bundles 8)
(for ([i (in-range grade-bundles)])
  (unless (equal? (list-ref (first d) (+ grades-offset (* 3 i)))
                  "TERM_CODE")
    (error 'unexpected-column)))
(unless (equal? (list-ref (first d) (+ grades-offset (* 3 grade-bundles)))
                "TEST")
  (error 'unexpected-column))

;; duplication here is truly impressive.
(define initial-data
  (apply
   append
   (for/list ([row (in-list rows)])
     (for/list ([i (in-range grade-bundles)])
       (define col-idx (+ grades-offset (* 3 i)))
       (list (emplid->hashed (list-ref row 2))
             (list-ref row (+ col-idx 0))
             (list-ref row (+ col-idx 1))
             (list-ref row (+ col-idx 2)))))))

(define non-blanks
  (filter (λ (r)
            (not (equal? (drop r 1) '("" "" ""))))
          initial-data))

(length non-blanks)

(define deduped (remove-duplicates non-blanks))

(length deduped)

(define (split-class s)
  (regexp-split #px" " s))

(define classes-split
  (map (λ (r)
         (append (take r 2)
                 (split-class (third r))
                 (drop r 3)))
       deduped))

(unless (= 0 (length (filter (λ (r)
                               (ormap (λ (cell) (equal? cell "")) r))
                             classes-split)))
  (error 'blanks))


(call-with-output-file "/tmp/zz.txt"
  #:exists 'truncate
  (λ (port)
    (for ([d (in-list classes-split)])
    (apply fprintf port "~a\t~a\t~a\t~a\t~a\n"
           d))))







