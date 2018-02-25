#lang racket

(require db
         plot
         "grades.rkt")

;; I was doing this with CSV files. Now I'm going to try doing it directly with the database.
;; er... okay, now I'm going straight to postgres.

(define conn
  (postgresql-connect #:database "cssegrades"
                      ;#:host "localhost"
                      #:port 5432
                      #:user "clements"))

(let ()
  (define time-to-103-completion

    (map (λ (pr)
           (list (first pr)
                 (cond [(sql-null? (second pr)) #f]
                       [else (second pr)])))
    (map vector->list
         (query-rows
          conn
          (~a
           "SELECT a.qtr,b.qtr"
           " FROM (first_cs_class a LEFT JOIN"
           "       passed_103 b ON a.id=b.id);")))))

  (call-with-output-file "/tmp/first-year-qtrs.rktd"
    (λ (port) (write time-to-103-completion port))))

(let ()
  (define 101-data
    (filter
     (λ (x) (not (sql-null? (second x))))
     (map vector->list
          (query-rows
           conn
           (~a
            "SELECT g.grade,c.curriculum FROM "
            " ((\"123_instructor\" i LEFT JOIN grades_after_123 g"
            "  ON i.id=g.id) LEFT JOIN curriculum c ON i.instructor=c.instructor)"
            " WHERE g.num='101'"
            ";")))))

  (call-with-output-file "/tmp/101-data.rktd"
    #:exists 'truncate
    (λ (port) (write 101-data port))))


(let ()
  (define pre-101-droppers
  (map (λ (l) (cons 0.0 l))
  (map vector->list
       (query-rows
        conn
        (~a
         "SELECT c.curriculum FROM"
         " ((\"123_instructor\" i INNER JOIN dropped_before_101 d"
         "   ON i.id=d.id) INNER JOIN curriculum c "
         "  ON i.instructor=c.instructor)"
         ";")))))

(call-with-output-file "/tmp/pre-101-droppers-data.rktd"
  #:exists 'truncate
  (λ (port) (write pre-101-droppers port))))

#;"101 drop rate"
#;(map (λ (g)
       (list (second (first g))
             (* 1.0 (/ (length (filter (compose not first) g))
                       (length g)))))
     (group-by
      second
      (append
       (for/list ([rec (in-list 101-data)])
         (list #t (second rec)))
       (for/list ([rec (in-list pre-101-droppers)])
         (list #f (second rec))))))

(let ()
  (define 102-non-ap-data
    (filter
     (λ (x) (not (sql-null? (second x))))
     (map vector->list
          (query-rows
           conn
           (~a
            "SELECT g.grade,c.curriculum FROM "
            " ((\"123_instructor\" i LEFT JOIN grades_after_123 g"
            "  ON i.id=g.id) LEFT JOIN curriculum c ON i.instructor=c.instructor)"
            " WHERE g.num='102'"
            " AND i.id NOT IN (SELECT id FROM ap_score)"
            ";")))))

  (call-with-output-file "/tmp/102-non-ap-data.rktd"
    #:exists 'truncate
    (λ (port) (write 102-non-ap-data port))))

(let ()
(define pre-102-non-ap-droppers
  (map (λ (l) (cons 0.0 l))
  (map vector->list
       (query-rows
        conn
        (~a
         "SELECT c.curriculum FROM"
         " ((\"123_instructor\" i INNER JOIN dropped_before_101 d"
         "   ON i.id=d.id) INNER JOIN curriculum c "
         "  ON i.instructor=c.instructor)"
         " WHERE i.id NOT IN (SELECT id FROM ap_score)"
         ";")))))

(call-with-output-file "/tmp/pre-102-non-ap-droppers-data.rktd"
  #:exists 'truncate
  (λ (port) (write pre-102-non-ap-droppers port))))

(let ()
  (define 102-ap-data
    (filter
     (λ (x) (not (sql-null? (second x))))
     (map vector->list
          (query-rows
           conn
           (~a
            "SELECT g.grade,c.curriculum FROM "
            " ((\"123_instructor\" i LEFT JOIN grades_after_123 g"
            "  ON i.id=g.id) LEFT JOIN curriculum c ON i.instructor=c.instructor)"
            " WHERE g.num='102'"
            " AND i.id IN (SELECT id FROM ap_score)"
            ";")))))

  (call-with-output-file "/tmp/102-ap-data.rktd"
    #:exists 'truncate
    (λ (port) (write 102-ap-data port))))

(let ()
(define pre-102-ap-droppers
  (map (λ (l) (cons 0.0 l))
  (map vector->list
       (query-rows
        conn
        (~a
         "SELECT c.curriculum FROM"
         " ((\"123_instructor\" i INNER JOIN dropped_before_101 d"
         "   ON i.id=d.id) INNER JOIN curriculum c "
         "  ON i.instructor=c.instructor)"
         " WHERE i.id IN (SELECT id FROM ap_score)"
         ";")))))

(call-with-output-file "/tmp/pre-102-ap-droppers-data.rktd"
  #:exists 'truncate
  (λ (port) (write pre-102-ap-droppers port))))



#;(call-with-output-file "/tmp/zz.csv"
  (λ (port)
    (for ([rec (in-list 101-data)])
      (fprintf port "TRUE,~a\n" (second rec)))
    (for ([rec (in-list 101-droppers)])
      (fprintf port "FALSE,~a\n" (second rec)))))



;; RESULTS FOR 102:

;; results for all students:
;> results = aov(gpa ~ curriculum, data = df)
;> summary(results)
;              Df Sum Sq Mean Sq F value Pr(>F)
;curriculum     4      5   1.247   0.648  0.628
;Residuals   1270   2444   1.924               
;; in other words: absolutely no reason to reject the null hypothesis
;; that all curricula produce the same results.

;; results for AP students:
;> results2 = aov(gpa ~ curriculum, data = apdata)
;> summary(results2)
;             Df Sum Sq Mean Sq F value Pr(>F)
;curriculum    4   2.82  0.7042   0.717  0.581
;Residuals   249 244.51  0.9820               
;; once again: definitely can't reject the null hypothesis

;; results for non-ap students:
;> results3 = aov(gpa ~ curriculum, data = nonapdata)
;> summary(results3)
;              Df Sum Sq Mean Sq F value Pr(>F)
;curriculum     4    3.1  0.7766   0.391  0.815
;Residuals   1016 2020.0  1.9882
;; once again: almost freakishly consistent. definitely no
;; evidence to support rejecting null hypothesis.

