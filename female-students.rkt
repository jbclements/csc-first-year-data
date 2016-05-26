#lang racket

(require csv-reading)
#;(require/typed csv-reading
               [csv->list (Port -> (Listof (Listof String)))])

#;(require/typed "table-sqlite.rkt"
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

;; rebuild the student databases



(define EMAIL-CELL-REGEXP
  #px"^ *[^ @]+@[^ ]+ *$")
(define EMPLID-CELL-REGEXP
  #px"[0-9]{5}")
(define NAME-CELL-REGEXP
  #px"[a-zA-Z ,|]{5}")

;; does this cell have a cell containing an email address?
(define (has-email-cell? row)
  (for/or ([cell (in-list row)])
    (regexp-match EMAIL-CELL-REGEXP cell)))

;; find contiguous blocks of student records, separated by headers
(define (block-split-search rows headers-so-far)
  (match rows
    ['() '()]
    [(cons (? has-email-cell? fst) rst)
     (block-split-consume (reverse headers-so-far) rows '())]
    [(cons header-line rst)
     (block-split-search rst (cons header-line headers-so-far))]))

;; read a contiguous block of student records, make a list of it
;; and continue reading.
(define (block-split-consume header rows so-far)
  (match rows
    ['() (list (list header (reverse so-far)))]
    [(cons (? has-email-cell? fst) rst)
     (block-split-consume header rst (cons fst so-far))]
    [(cons _ _)
     (cons (list header (reverse so-far))
           (block-split-search rows '()))]))

;; given a predicate and a row, return the index of the cell
;; satisfying the predicate
(define ((find-idx-of-cell pred) row)
  (match (filter (λ (pr) (pred (first pr)))
                (for/list ([r row] [i (in-naturals)]) (list r i)))
    [(list (list cell idx)) idx]
    [(and l (cons a (cons b rst)))
     (error 'find-idx-of-cell
            "multiple matches: ~v\nfor predicate: ~v\nfor row: ~v"
            l pred row)]
    [other
     (error 'find-idx-of-cell
            "failed to find cell. regexp: ~v\nrow: ~v\n"
            pred row)]))

(define email-idx (find-idx-of-cell (λ (c) (regexp-match EMAIL-CELL-REGEXP c))))
(define name-idx (find-idx-of-cell
                  (λ (c) (and (regexp-match NAME-CELL-REGEXP c)
                              (not (regexp-match EMAIL-CELL-REGEXP c))))))

;(: cells (Listof (Listof String)))
(define rows
  ;; dump the header row:
  (rest
   (call-with-input-file "/Users/clements/clements/datasets/female-123-students.csv" 
    (λ (port)
      (csv->list port)))))

;; a bundle contains a list of header rows and a list of
;; student rows
(define bundles (block-split-search rows '()))
(length bundles)
(apply + (map length (map second bundles)))

;; for each bundle, identify the field containing the email address
(define email-bundles
  (for/list ([b (in-list (map second bundles))])
    (define candidates (remove-duplicates (map email-idx b)))
    (match candidates
      [(list c) (map (λ (r) (list-ref r c)) b)]
      [other (error 'find-email "more than one looks like an email: ~e"
                    b)])))


;; for each bundle, return the name fields (based on observed
;; properties of the bundles)
(define name-bundles
  (for/list ([b (in-list (map second bundles))])
    (for/list ([row (in-list b)])
      (cond [(regexp-match EMAIL-CELL-REGEXP (second row))
             (list 'a (first row))]
            [(regexp-match EMPLID-CELL-REGEXP (second row))
             (list 'b (first row))]
            [else
             (cons 'c (take row 2))]))))

;; given names separated by vertical bars, return tagged
;; first-last name pairs
(define (bars-style r)
  (match r
    [(list 'a " Aquino     | Erica     |") '((f "Erica")
                                             (l "Aquino"))]
    [(list 'a (regexp #px"^\\| +([a-zA-Z]+) +\\| ([a-zA-Z']+)"
                      (list _ last first))) `((f ,first)
                                              (l ,last))]
    [other other]))

(define (paren-style r)
  (match r
    [(list 'a (regexp #px"\\(\"([^\"]*)" (list _ str)))
     (lastfirst-style (list 'a str))]))

(define (lastfirst-style r)
  (match r
    [(list _ str)
     (match (regexp-split #px"," str)
       [(list last firstplus)
        (match (regexp-split #px" +" (string-trim firstplus))
          [(list first) `((f ,first) (l ,last))]
          [(list first middle) `((f ,first) (m ,middle) (l ,last))]
          [(list first middle1 middle2)
            `((f ,first) (m ,(~a middle1 " " middle2))
                         (l ,last))])])]))

(define (lastfirst-nocomma-style r)
  (match r
    [(list _ str)
     (match (regexp-split #px" " str)
       [(list last first)
        `((f ,first) (l ,last))])]))

(define (firstlast-style r)
  (match r
    ;; special case for some ending with commas
    [(list 'a (regexp #px"^(.*),$" (list _ str)))
     (firstlast-style (list 'a str))]
    ;; special case for some ending with hyphens
    [(list 'a (regexp #px"^([^-]*)- *$" (list _ str)))
     (firstlast-style (list 'a str))]
    [(list 'a str)
     (match (regexp-split #px" +" (string-trim str))
       ;; special case for vivian fong
       [(list first last "vfong01") `((f ,first) (l ,last))]
       ;; special case for suffix of I
       [(list first last "I") `((f ,first) (l ,last) (s "I"))]
       [(list first last) `((f ,first) (l ,last))]
       )]))

(define (separated-style r)
  (match r
    [(list 'c first last) `((f ,first) (l ,last))]))

(define p-funs
  (list bars-style paren-style
        lastfirst-style lastfirst-style
        firstlast-style firstlast-style
        firstlast-style lastfirst-style
        lastfirst-style lastfirst-style
        firstlast-style firstlast-style
        lastfirst-style lastfirst-style
        firstlast-style lastfirst-nocomma-style
        lastfirst-style separated-style
        lastfirst-style lastfirst-style
        firstlast-style firstlast-style
        lastfirst-style lastfirst-style
        firstlast-style lastfirst-style
        lastfirst-style lastfirst-style
        firstlast-style))

(define parsed-name-bundles
  (for/list ([bundle (in-list name-bundles)]
             [processing-fun (in-list p-funs)])
    (map processing-fun bundle)))

;; given a list of header rows, return advisor and year
(define (process-header rows)
  (match (string-trim (first (last rows)))
    ;; special case weird one
    ["Fall 2011:" '(same 2011)]
    ;; with hyphen
    [(regexp #px"^([A-Za-z]+) *- *([0-9]+)$" (list _ person yearstr))
     (list person (string->number yearstr))]
    ;; no hyphen
    [(regexp #px"^([A-Za-z]+) *([0-9]+)$" (list _ person yearstr))
     (list person (string->number yearstr))]
    ;; year first:
    [(regexp #px"^([0-9]+) *([A-Za-z]+)$" (list _ yearstr person))
     (list person (string->number yearstr))]
    [(regexp #px"^[0-9]+$" (list yearstr))
     (list 'same (string->number yearstr))]
    [other `(blarg ,other)]
    #;["Clements2012" '("Clements" 2012)]
    #;["Haungs - 2012" '("Haungs" 2012)]
    #;["Workman-2012"]))

;; for "same" rows, use previous person
(define (clean-up-sames header-pairs prev-advisor)
  (match header-pairs
    ['() empty]
    [(cons (list 'same year) rst)
     (when (not prev-advisor)
       (error "no previous advisor!"))
     (cons (list prev-advisor year)
           (clean-up-sames rst prev-advisor))]
    [(cons (list advisor year) rst)
     (cons (list advisor year)
           (clean-up-sames rst advisor))]))

(define header-pairs
  (clean-up-sames
   (for/list ([header (in-list (map first bundles))])
     (process-header header))
   #f))

(define (all-equal? . elts)
  (match elts
    ['() #t]
    [(list e) #t]
    [(cons a (cons b rst))
     (and (equal? a b) (all-equal? (cons b rst)))]))

(unless (= (length header-pairs)
           (length bundles)
           (length parsed-name-bundles)
           (length email-bundles))
  (error "# of bundles should all be the same"))
(unless (all-equal?
         (map length (map second bundles))
         (map length parsed-name-bundles)
         (map length email-bundles))
  (error "# of names in each bundle should all be the same"))

(define (justify-name assoc)
  (for/list ([field (in-list '(f m l s))])
    (match (dict-ref assoc field '())
      [(list match) match]
      ['() ""])))

(require rackunit)
(check-equal? (justify-name '((f "boo") (l "woo")))
              '("boo" "" "woo" ""))

(define (csv-write lines)
  (for ([l (in-list lines)])
    (display
     (apply
      string-append
      (add-between
       (map format-cell l)
       ",")))
    (newline)))

(define (format-cell c)
  (match c
    [(? number?) (number->string c)]
    [(regexp #px",") (~a "\"" c "\"")]
    [other c]))

(with-output-to-file "/tmp/foo.csv"
  (λ ()
    (csv-write
     (apply
      append
     (for/list ([b (in-list bundles)]
                [n (in-list parsed-name-bundles)]
                [h (in-list header-pairs)]
                [e (in-list email-bundles)])
       (for/list ([name (in-list n)]
                  [email (in-list e)]
                  [original-line (in-list (second b))])
         (append (justify-name name)
                 (list email)
                 h
                 original-line))
       ))))
  #:exists 'truncate)

