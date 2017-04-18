#lang racket

(require "grades.rkt"
         racket/runtime-path)

(require csv-reading)

(require "table-sqlite.rkt")

;; first last emplid
(define names-table
  (find-table "names"))

(define-runtime-path HERE ".")

;; build the female 123 student database
(define cells
  (call-with-input-file (build-path HERE "female-123-students-clean.csv")
    (λ (port)
      (csv->list port))))

;; the first row (presumably column labels)
(define label-row (first cells))

;; the rest of the rows
(define non-label-rows (rest cells))

;; sanity check on the labels:
;; yikes... just insist that the labels be these:
(unless (equal?
         label-row
         '("firstname"
           "middlenames"
           "lastname"
           "suffixes"
           "email"
           "instructor"
           "year"))
  (error 'label-row "unexpected label row, please check code below."))

;; find the index of an element in a list
(define (find-pos elt l)
  (let loop ([remaining l] [i 0])
    (cond [(empty? remaining) (error 'find-pos "couldn't find element")]
          [else (cond [(equal? (first remaining) elt) i]
                      [else (loop (cdr remaining) (add1 i))])])))

(define majorsonly
  (filter
   (λ (x) (not (member (fifth x)
                       '("eohanlon@calpoly.edu"
                         "crmitche@calpoly.edu"
                         "rlignitz@calpoly.edu"
                         "tseto@calpoly.edu"
                         "ahsiddiq@calpoly.edu"7
                         ;; baffling: did she change from cpe to pols or
                         ;; vice versa?
                         "chdickso@calpoly.edu"
                         "ykim44@calpoly.edu"
                         "kyoung29@calpoly.edu"
                         "ajlor@calpoly.edu"
                         "agerught@calpoly.edu"
                         "ecguerra@calpoly.edu"
                         "ajaaksi@calpoly.edu"
                         "porekhov@calpoly.edu"
                         "krago@calpoly.edu"
                         "htran15@calpoly.edu"
                         "smoza@calpoly.edu"))))
   (rest cells)))

(check-duplicates (map third majorsonly))

;; changed last names
(define (lastname-rewrite l)
  (match l
    ["Guzman" "Duldulao"]
    ["Galvan" "Galvan-Bahnuk"]
    ["Ramano" "Romano"]
    [other other]))

;; manual associations for those whose names don't quite match
(define (manual-assoc id)
  (match id
    ["jjbaer@calpoly.edu" "008389377"]
    ["acompani@calpoly.edu" "006881637"]
    ["kkeim@calpoly.edu" "006982556"]
    ["mboyken@calpoly.edu" "008602954"]
    ["heseo@calpoly.edu" "009908583"]
    ["zgutierr@calpoly.edu" "011830360"]
    ["jkim271@calpoly.edu" "011287363"]
    ["ekolokow@calpoly.edu" "007735360"]
    ["amvelasq@calpoly.edu" "006932844"]
    ["ejohns72@calpoly.edu" "011009215"]
    ["bgalvanb@calpoly.edu" "010843725"]
    ["hhwang02@calpoly.edu" "006640045"]
    ["kcruz03@calpoly.edu" "006898108"]
    ["astoytce@calpoly.edu" "006736661"]
    ["iguzmanl@calpoly.edu" "011177864"]
    ["smkoski@calpoly.edu" "011065323"]
    [other #f]))

(call-with-output-file "/tmp/female-123-2118-2158.csv"
  (λ (port)
    (for ([row (in-list majorsonly)])
      (define lastname (lastname-rewrite (third row)))
      (define id
        (match (manual-assoc (fifth row))
          [#f
           (define matches
             (table-select names-table '(first emplid)
                           #:where `((= last ,lastname))))

           (define matches-assoc (map vector->list matches))
           (define ids (dict-ref matches-assoc (first row)))
           (match ids
             [(list id) id]
             [other (error 'badbad)])]
          [(? string? s)
           s]))
      (fprintf port "~a\n" id)
      )))






