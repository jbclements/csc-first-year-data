#lang racket

(require "table-sqlite.rkt"
         "student-data.rkt"
         plot)

(define (vref idx)
  (λ (v) (vector-ref v idx)))

(define tracks
  (group-by
   (vref 0)
   (table-select grade-facts-table '(student qtr class grade))))

(define (grade-map g)
  (cond [(member g '("A" "A-" "B+" "B" "B-" "C+" "C" "C-"))
         'pass]
        [else 'nopass]))

(define g
  (for/list ([t (in-list tracks)])
    (map
     (λ (r)
       (for/list ([r (in-list r)])
         (vector ((vref 2) r)
                 (grade-map ((vref 3) r)))))
     (sort (group-by (vref 1) t) < #:key (λ (g) ((vref 1) (first g)))))))

(define h
  (sort
   (map
    (λ (r)
      (list (first r) (length r)))
    (group-by
     (λ (x) x)
     g))
   >
   #:key second
   ))

()

