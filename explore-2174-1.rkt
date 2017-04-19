#lang racket

;; trying to answer Zoe's questions about 
(require db
         "make-connection.rkt")

(define conn (make-connection))

(let ()
  (define started-in-2158
  (list->set
   (query-list
    conn
    (~a "SELECT id"
        " FROM class_grade"
        " WHERE subject = 'CPE'"
        " AND num = '123'"
        " AND qtr = 2158"
        ";"))))
(printf "~v majors took 123 in 2158\n"
        (set-count started-in-2158))


(define 2158-regular-track
  (list->set
   (query-list
    conn
    (~a "SELECT a.id"
        " FROM (class_grade a"
        "       INNER JOIN"
        "       class_grade b"
        "       ON a.id = b.id)"
        " WHERE a.subject = 'CPE'"
        " AND a.num = '123'"
        " AND a.qtr = 2158"
        " AND b.subject = 'CPE'"
        " AND b.num = '102'"
        " AND b.qtr = 2164"))))

(define 2158-103-takers
  (list->set
   (query-list
    conn
    (~a "SELECT a.id"
        " FROM (class_grade a"
        "       INNER JOIN"
        "       class_grade b"
        "       ON a.id = b.id)"
        " WHERE a.subject = 'CPE'"
        " AND a.num = '123'"
        " AND a.qtr = 2158"
        " AND b.subject = 'CPE'"
        " AND b.num = '103'"
        " AND b.qtr = 2164"))))
(printf "~v majors took 102 or 103 in 2164\n"
        (set-count (set-union 2158-103-takers
                              2158-regular-track)))
)


(define f-started-in-2158
  (list->set
   (query-list
    conn
    (~a "SELECT c.id"
        " FROM (class_grade c INNER JOIN female f"
        "       ON c.id = f.id)"
        " WHERE c.subject = 'CPE'"
        " AND c.num = '123'"
        " AND c.qtr = 2158"
        ";"))))
(printf "~v female majors took 123 in 2158\n"
        (set-count f-started-in-2158))


(define f-2158-regular-track
  (list->set
   (query-list
    conn
    (~a "SELECT a.id"
        " FROM (" "(class_grade a INNER JOIN female f ON a.id = f.id)"
        "       INNER JOIN"
        "       class_grade b"
        "       ON a.id = b.id)"
        " WHERE a.subject = 'CPE'"
        " AND a.num = '123'"
        " AND a.qtr = 2158"
        " AND b.subject = 'CPE'"
        " AND (b.num = '101' OR b.num = '102' OR b.num = '103')"
        " AND b.qtr = 2164"))))

#;(define f-2158-103-takers
  (list->set
   (query-list
    conn
    (~a "SELECT a.id"
        " FROM (" "(class_grade a INNER JOIN female f ON a.id = f.id)"
        "       INNER JOIN"
        "       class_grade b"
        "       ON a.id = b.id)"
        " WHERE a.subject = 'CPE'"
        " AND a.num = '123'"
        " AND a.qtr = 2158"
        " AND b.subject = 'CPE'"
        " AND b.num = '103'"
        " AND b.qtr = 2164"))))


(define f-2164-102-or-103
  (set-union #;f-2158-103-takers
             f-2158-regular-track))

(printf "~v female majors took 102 or 103 in 2164\n"
        (set-count f-2164-102-or-103))

(set-subtract f-started-in-2158
              f-2164-102-or-103)
#;(
)





