#lang racket

;; trying to answer Zoe's questions about 
(require db
         "make-connection.rkt")

(define conn (make-connection))

(define incoming-qtr 2158)
(define spring-qtr 2164)
(let ()
  (define all-started-in
  (list->set
   (query-list
    conn
    (~a "SELECT c.id"
        " FROM class_grade c"
        " WHERE c.subject = 'CPE'"
        " AND c.num = '123'"
        " AND c.qtr = $1"
        ";")
    incoming-qtr)))
(printf "~v majors took 123 in ~v\n"
        (set-count all-started-in)
        incoming-qtr)


(define all-regular-track
  (list->set
   (query-list
    conn
    (~a "SELECT a.id"
        " FROM (" "class_grade a"
        "       INNER JOIN"
        "       class_grade b"
        "       ON a.id = b.id)"
        " WHERE a.subject = 'CPE'"
        " AND a.num = '123'"
        " AND a.qtr = $1"
        " AND b.subject = 'CPE'"
        " AND b.qtr = $2")
    incoming-qtr
    spring-qtr)))

(printf "~v maj majors a CS class in 2164\n"
        (set-count all-regular-track))

  (printf "retention likelihood: ~v\n"
          (exact->inexact
           (/ (set-count all-regular-track)
              (set-count all-started-in))))
)

(let ()
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
        " AND b.qtr = 2164"))))

(printf "~v female majors a CS class in 2164\n"
        (set-count f-2158-regular-track))

(set-subtract f-started-in-2158
              f-2158-regular-track)

(printf "retention likelihood: ~v\n"
          (exact->inexact
           (/ (set-count f-2158-regular-track)
              (set-count f-started-in-2158)))))







