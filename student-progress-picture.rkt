#lang racket

(require 2htdp/image
         db/base
         db/postgresql
         csse-scheduling/canonicalize)

(define box-x 6)
(define box-y 3)

(define yes-rect (rectangle box-x box-y 'solid 'red))
(define no-rect (rectangle box-x box-y 'solid 'white))

(define conn
  (postgresql-connect
   #:database "csseprogress"
   #:user "scheduler"
   #:port 13432
 ))

(define ((vref idx) v)
  (vector-ref v idx))

(define allofthem
  (query-rows
   conn
   (~a "SELECT id,course,qtr "
       " FROM course_grade"
       "  WHERE grade != 'RP'"
       "  AND grade != 'I'")
   ))

(define courses
  (remove-duplicates (map (vref 1) allofthem)))

(define sorted-courses
  (sort courses #:key course-key string<?))

(define (first-qtr group)
  (apply min (map (vref 2) group)))

(define user-records
  (sort
   (group-by (vref 0) allofthem)
   <
   #:key first-qtr))

(define index-row
  (apply
   beside
   (for/list ([i 10])
     (beside yes-rect
             (apply beside (for/list ([j 9]) no-rect))))))

(define blank-row
  (apply beside (for/list ([i (length sorted-courses)]) no-rect)))

(above
 (above/align
  "left"
  index-row
  blank-row)
(apply
 above
(for/list ([g (in-list user-records)])
  (define user-courses (map (vref 1) g))
  (apply
   beside
   (for/list ([c (in-list sorted-courses)])
    (cond [(member c user-courses) yes-rect]
          [else no-rect]))))))
