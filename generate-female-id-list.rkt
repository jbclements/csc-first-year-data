#lang racket

(require "grades.rkt"
         racket/runtime-path
         "emplid-hasher.rkt")

(require csv-reading)

#;(require "table-sqlite.rkt")

;; first last emplid
(define names-table
  (call-with-input-file
      (build-path "/Users/clements/"
                  "OneDrive - California Polytechnic State University"
                  "student-data-hash/names-to-ids.csv")
    csv->list)
  #;(find-table "names"))

(define-runtime-path HERE ".")

;; build the female 123 student database
(define cells
  (call-with-input-file "/tmp/female-123-students-cleaned.csv"
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
                         "ahsiddiq@calpoly.edu"
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
   (filter ;; temporarily omit 2016
    (λ (row) (not (equal? (seventh row) "2016")))
    ;; somehow Olson got duplicated...
    (remove-duplicates
     (rest cells)))))

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
    ("jjbaer@calpoly.edu" "d86f0636b371e9776862")
    ("acompani@calpoly.edu" "ffa9434f89df49896fad")
    ("kkeim@calpoly.edu" "33037c7c9a4962936303")
    ("mboyken@calpoly.edu" "f9845959e6b7fcd3ad67")
    ("heseo@calpoly.edu" "f815f55d831660d968be")
    ("zgutierr@calpoly.edu" "a02c020b78504a59e70f")
    ("jkim271@calpoly.edu" "66053f482daf91d44ce9")
    ("ekolokow@calpoly.edu" "5eb9440d487ffbded2bd")
    ("amvelasq@calpoly.edu" "ce2904c190967677c855")
    ("ejohns72@calpoly.edu" "7a93103be466d1956778")
    ("bgalvanb@calpoly.edu" "39248415da5a2c8916d4")
    ("hhwang02@calpoly.edu" "7705dc011f80190a9c83")
    ("kcruz03@calpoly.edu" "85ab670f471ce9b7924f")
    ("astoytce@calpoly.edu" "40300e5361c28ab621e2")
    ("iguzmanl@calpoly.edu" "a3ae3b242c2cbe0406d0")
    ("smkoski@calpoly.edu" "5768a74fad25849d6f94")
    ("symendoz@calpoly.edu" "4467eb048b6aa374a02c")
    ("smendo12@calpoly.edu" "346b9cff5b6b1dd14113")
    [other #f]))

(call-with-output-file "/tmp/female-123-2118-2158.csv"
  #:exists 'truncate
  (λ (port)
    (for ([row (in-list majorsonly)])
      (define lastname (lastname-rewrite (third row)))
      (define firstname (first row))
      (define id
        (match (manual-assoc (fifth row))
          [#f
           (define matches-1
             (filter (λ (r) (equal? (take r 2)
                                    (list firstname lastname)))
                     names-table))
           (match matches-1
             [(list (list _ _ id)) id]
             [other
              (define lastname-matches
                (filter (λ (r) (equal? (second r) lastname))
                        names-table))
              (error 'matching
                     "no exact match for ~e with id ~e, maybe there's one in:\n ~e"
                     (list firstname lastname)
                     (fifth row)
                     lastname-matches)])]
          [(? string? s)
           s]))
      (fprintf port "~a\n" id)
      )))






