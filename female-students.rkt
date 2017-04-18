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

;(: cells (Listof (Listof String)))
(define rows
  (call-with-input-file "/Users/clements/clements/datasets/female-123-students.csv" 
    (λ (port)
      (csv->list port))))

(define EMAIL-CELL-REGEXP
  #px"^ *[^ @]+@[^ ]+ *$")
(define EMPLID-CELL-REGEXP
  #px"[0-9]{5}")
(define NAME-CELL-REGEXP
  #px"[a-zA-Z ,|]{5}")

;; does 
(define (has-email-cell? row)
  (for/or ([cell (in-list row)])
    (regexp-match EMAIL-CELL-REGEXP cell)))

;; find contiguous blocks of student records, separated by chaff
(define (block-split-search rows)
  (match rows
    ['() '()]
    [(cons (? has-email-cell? fst) rst)
     (block-split-consume rows '())]
    [(cons _ rst)
     (block-split-search rst)]))

;; read a contiguous block of student records, make a list of it
;; and continue reading.
(define (block-split-consume rows so-far)
  (match rows
    ['() (list (reverse so-far))]
    [(cons (? has-email-cell? fst) rst)
     (block-split-consume rst (cons fst so-far))]
    [(cons _ _)
     (cons (reverse so-far) (block-split-search rows))]))

(define bundles (block-split-search rows))
(length bundles)
(apply + (map length bundles))

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

(for/list ([b (in-list bundles)])
  (define candidates (remove-duplicates (map email-idx b)))
  (match candidates
    [(list c) c]
    [other (error 'find-email "more than one looks like an email: ~e"
                  b)]))

(define name-bundles
  (for/list ([b (in-list bundles)])
    (for/list ([row (in-list b)])
      (cond [(regexp-match EMAIL-CELL-REGEXP (second row))
             (list 'a (first row))]
            [(regexp-match EMPLID-CELL-REGEXP (second row))
             (list 'b (first row))]
            [else
             (cons 'c (take row 2))]))))

(define (b1 r)
  (match r
    [(list 'a (regexp #px"^\\| +[a-zA-Z]+ +\\| [a-zA-Z']+" n)) n]
    [other other]))

(map b1 (first name-bundles))
