#lang typed/racket

;; this file builds a table from the CSV data in student-data.csv.

;; EDIT: 2016-05-04 (572a) - looking at new data, going back to 2005.

(require "grades.rkt"
         racket/runtime-path)

(require/typed csv-reading
               [csv->list (Port -> (Listof (Listof String)))])

(require/typed "table-sqlite.rkt"
               [#:opaque Table table?]
               [make-table
                ((Listof Symbol)
                 (Sequenceof (Sequenceof Any))
                 [#:permanent String]
                 -> Table)]
               [make-table-from-select
                (Table
                 (Listof Any)
                 [#:where Any]
                 [#:group-by Any]
                 [#:permanent String]
                 -> Table)]
               [natural-join
                (Table Table
                 [#:permanent String]
                 -> Table)]
               [find-table (String -> Table)]
               [in-table-column (Table Symbol -> (Sequenceof Any))]
               [table-select
                (Table (Listof Any)
                       [#:where Any]
                       [#:group-by Any]
                       -> (Sequenceof (Vectorof Any)))]
               [back-door/rows
                (String Boolean -> (Sequenceof (Vectorof Any)))])


;; find the index of an element in a list
(: find-pos (All (T) (T (Listof T) -> Natural)))
(define (find-pos elt l)
  (let loop ([remaining l] [i : Natural 0])
    (cond [(empty? remaining) (error 'find-pos "couldn't find element")]
          [else (cond [(equal? (first remaining) elt) i]
                      [else (loop (cdr remaining) (add1 i))])])))

(define-runtime-path HERE ".")

;; build the female 123 student database
#;(: cells (Listof (Listof String)))
#;(define cells
  (call-with-input-file (build-path HERE "female-123-students-clean.csv")
    (λ (port)
      (csv->list port))))

  

;; rebuild the student databases

;; a list of lists representing the cells in the csv file
(: cells (Listof (Listof String)))
(define cells
  (call-with-input-file "/Users/clements/clements/datasets/student-data-2005-2016.csv"
    (λ (port)
      (csv->list port))))

;; the first row (presumably column labels)
(: label-row (Listof String))
(define label-row (first cells))

;; the rest of the rows
(: non-label-rows (Listof (Listof String)))
(define non-label-rows (rest cells))

;; sanity check on the labels:
;; yikes... just insist that the labels be these:
(unless (equal?
         label-row
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
  (error 'label-row "unexpected label row, please check code below."))


(define ID-COL (find-pos "EMPLID" label-row))
(define AP-TEST-NAME-COL (find-pos "TEST" label-row))
(define AP-TEST-SCORE-COL (find-pos "SCORE" label-row))

(: GOOD-LABELS-COUNT Natural)
(define GOOD-LABELS-COUNT 24)

(unless (for/and : Boolean ([row (in-list non-label-rows)])
          (not (not (member (list-ref row AP-TEST-NAME-COL)
                            (list "AP 31" "")))))
  (error 'expected-ap31-as-test
         "expected AP 31 or empty string as test, got: ~v\n"))

(unless (for/and : Boolean
          ([label : String
                         (in-list (take label-row GOOD-LABELS-COUNT))])
          (not (equal? (string-trim label) "")))
  (error 'abc))
(unless (for/and : Boolean
          ([label : String
                         (in-list (drop label-row GOOD-LABELS-COUNT))])
          (equal? (string-trim label) ""))
  (error 'def))

(define col-names (map string->symbol (take label-row GOOD-LABELS-COUNT)))

;; lists of name name idnum
(define student-infos
  (remove-duplicates
   (for/list : (Listof (Listof String))
     ([row (in-list non-label-rows)])
     (take row 3))))

  (make-table '(first last emplid) student-infos
              #:permanent "names")

;; ensure ids are as unique as all other student info:
(unless (= (length student-infos)
           (length (remove-duplicates
                    (map (inst third String String String String) student-infos))))
  (error 'non-unique-email-ids))

;; IT'S NOW SAFE TO USE EMPLIDS AS UNIQUE IDS.

;; each row contains up to five distinct facts, ugh.
(define class-info-cols (list 10 13 16 19))
(: row->class-facts ((Listof String) -> (Listof (Listof
                                                 (U String Number)))))
(define (row->class-facts row)
  (map
   (λ ([cols : (Listof String)])
     (list (first cols)
           (string->number/checked (second cols))
           (third cols)
           (fourth cols)))
  (filter
   non-blank-class-fact
   (for/list : (Listof (Listof String))
     ([idx : Natural (in-list class-info-cols)])
     (define cols (take (drop row idx) 3))
     (cons (list-ref row ID-COL)
           cols)))))

  (: string->number/checked (String -> Number))
  (define (string->number/checked str)
    (match (string->number str)
      [#f (raise-argument-error 'string->number/checked
                                "string representing number"
                                0 str)]
      [(? number? str) str]))


  ;; is this class-fact not just empty strings?
(: non-blank-class-fact ((Listof (U String Number)) -> Boolean))
(define (non-blank-class-fact l)
  (not (equal? (drop l 1) '("" "" ""))))

(define all-class-facts
  (remove-duplicates
   (apply append
          (map row->class-facts non-label-rows))))

;; now for the AP scores

(: row->test-facts ((Listof String) -> (Listof String)))
(define (row->test-facts row)
  (cons (list-ref row ID-COL)
        (take (drop row AP-TEST-SCORE-COL) 1)))

(: non-blank-test-fact ((Listof String) -> Boolean))
(define (non-blank-test-fact l)
  (not (equal? (drop l 1) '(""))))

(define all-test-facts
  (remove-duplicates
   (filter
    non-blank-test-fact
    (map row->test-facts non-label-rows))))

  (define all-grades-table
    (make-table '(student qtr class grade)
                all-class-facts
                #:permanent "all_grades"
                ))

  (define ap-scores-table
    (make-table '(student score)
                all-test-facts
                #:permanent "ap_scores"
                ))

;; this code is probably redundant, could use "first-quarter"
;; list defined below.

  ;; create a view without the early students:
  (: early-students (Listof String))
  (define early-students
    (map (λ ([v : Any])
           (vector-ref (cast v (Vector String)) 0))
         (sequence->list
          (table-select all-grades-table '(student)
                        #:where '((< qtr 2058))
                        #:group-by '(student)))))

  (: all-students (Listof String))
  (define all-students
    (cast
     (sequence->list (in-table-column all-grades-table 'student))
     (Listof String)))

(length early-students)
(length all-students)
(define non-early-students
  ((inst map (Vectorof String) String)
   (inst vector String)
       (remove* early-students all-students)))
  
(define non-early-table
  (make-table '(student) non-early-students
              #:permanent "non_early_students"))

  (define grade-facts-table
    (natural-join all-grades-table non-early-table
                  #:permanent "grades"))

(: student-first-quarters (Listof (Vector Any Real)))
(define student-first-quarters
  ((inst map (Vector Any Real) (Listof (Vector Any Real)))
   first
   (map
    (λ ([group : (Listof (Vector Any Real))])
      ((inst sort (Vector Any Real) Real)
       group (ann < (Real Real -> Boolean))
            #:key (cast (vref 1)
                        ((Vector Any Real) -> Real))))
    (cast
     (group-by
      (vref 0)
      (sequence->list
       (table-select grade-facts-table '(student qtr))))
     (Listof (Listof (Vector Any Real)))))))

(define student-first-qtr-table
  (make-table '(student qtr) student-first-quarters
              #:permanent "student_first_qtr"))

(natural-join
 grade-facts-table
 (make-table-from-select student-first-qtr-table '(student)
                         #:where '((< qtr 2152))
                         #:permanent "t1")
 #:permanent "pre_2152_grades")

(natural-join
 grade-facts-table
 (make-table-from-select student-first-qtr-table '(student)
                         #:where '((< qtr 2108))
                         #:permanent "t2")
 #:permanent "pre_123_grades")

(natural-join
 grade-facts-table
 (make-table-from-select student-first-qtr-table '(student)
                         #:where '((<= 2108 qtr)
                                   (< qtr 2152))
                         #:permanent "t3")
 #:permanent "post_123_grades")



  
(printf "done building databases from csv files.\n")

