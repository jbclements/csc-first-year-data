#lang typed/racket

(require plot
         typed/rackunit)

;; TEMPORARY: import student-track-trees when ready:

(define-type ClassOutcome (U 'pass 'nopass))
(define-type QtrOutcome (U 'pass 'nopass 'mixed))
(define-type ClassName (U 'cpe123 'cpe101 'cpe102 'cpe103))

(: vref (All (T) (Index -> ((Vectorof T) -> T))))
(define ((vref n) t)
  (vector-ref t n))

(: class-name ((Vector Symbol ClassOutcome) -> Symbol))
(define (class-name c) (vector-ref c 0))

(: class-outcome ((Vector Symbol ClassOutcome) -> ClassOutcome))
(define (class-outcome c) (vector-ref c 1))

;; represents the decisions made by students to take or not to take a class
;; (though of course the class might just have been full)
(define-type EnrollT (U Take NoTake))
(struct Take ([class : (Listof Symbol)] [num : Natural]
                                        [nexts : OutcomeT])
  #:transparent)
(struct NoTake ([num : Natural] [nexts : (Listof EnrollT)])
  #:transparent)

;; represents the result of the class (C- or better, allowing the student to go on)
(struct OutcomeT ([npass : Natural] [pass : (Listof EnrollT)]
                  [nnopass : Natural] [nopass : (Listof EnrollT)]
                  [nmixed : Natural] [mixed : (Listof EnrollT)])
  #:transparent)

(define-type Track (Listof QtrTrack))
(define-type QtrTrack
  (List Natural (Listof (Vector Symbol ClassOutcome))))

;; an ordering on classes:
(: classname-ord (ClassName -> Natural))
(define (classname-ord classname)
  (match classname
    ['cpe123 0]
    ['cpe101 1]
    ['cpe102 2]
    ['cpe103 3]))

;; END OF TEMPORARY

;; a RectAccum represents a mapping from a course or set of courses
;; and pass/nopass/mixed grades
;; to a list of rectangles, used to accumulate all of the various
;; bits that will appear in the graph.

(define-type RectAccum
  (HashTable Label (Listof Rect)))
(define-type Rect (Vector ivl ivl))
(define-type Label (U (Listof ClassName) QtrOutcome))

(plot
 (rectangles
  (list (vector (ivl 0.0 1.0)
                (ivl 0.0 120.0))
        (vector (ivl 0.0 1.0)
                (ivl 360.0 365.0)))
  #:label "CPE 101")
 #:x-min 0
 #:x-max 24)

;; given a list of enrollment trees, generate the hash that
;; will be used to construct the plot.
(: tree->rects ((Listof EnrollT) -> RectAccum))
(define (tree->rects trees)
  (hash))

;; given a RectAccum, generate the actual plot
(: rects->plot (RectAccum -> Any))
(define (rects->plot rects)
  (define keys (hash-keys rects))
  (: sorted-keys (Listof Label))
  (define sorted-keys (sort keys label<?))
  (plot
   (for/list : (Listof renderer2d)
     ([k : Label (ann (in-list sorted-keys)
                      (Sequenceof Label))])
     (rectangles (hash-ref rects k)
                 #:label (format "~a" k)
                 #:color (label-color k sorted-keys)))))

(: label-color (All (T) (T (Listof T) -> Natural)))
(define (label-color l keys)
  (cond [(empty? keys) (raise-argument-error
                        'label-color
                        "label in the list"
                        0 l keys)]
        [else (cond [(equal? l (first keys)) 0]
                    [else (add1 (label-color l (rest keys)))])]))

;; sort the keys for labeling order: grades are later
;; than classes, and earlier classes are earlier, and
;; shorter lists are earlier

(: label<? ((U QtrOutcome (Listof ClassName))
            (U QtrOutcome (Listof ClassName))
            -> Boolean))
(define (label<? a b)
  (cond [(and (symbol? a) (symbol? b))
         (< (grade-ord a) (grade-ord b))]
        [(symbol? b) #t]
        [(symbol? a) #f]
        [else (course-list<?
               (sort (map classname-ord a) <)
               (sort (map classname-ord b) <))]))

(: course-list<? ((Listof Natural)
                  (Listof Natural)
                  -> Boolean))
(define (course-list<? a b)
  (cond [(and (empty? a) (empty? b)) #f]
        [(empty? a) #t]
        [(empty? b) #f]
        [else (cond [(< (first a) (first b)) #t]
                    [(= (first a) (first b))
                     (course-list<? (rest a) (rest b))]
                    [else #f])]))

(: grade-ord (QtrOutcome -> Natural))
(define (grade-ord grade)
  (match grade
    ['pass 0]
    ['nopass 1]
    ['mixed 2]))

(rects->plot
 (hash '(cpe101)
       (list (vector (ivl 0.0 1.0)
                     (ivl 0.0 120.0))
             (vector (ivl 0.0 1.0)
                     (ivl 360.0 365.0))
             (vector (ivl 3 4)
                     (ivl 20 30))
             (vector (ivl 3 4)
                     (ivl 80 120))
             (vector (ivl 3 4)
                     (ivl 362 365)))
       'pass
       (list (vector (ivl 1 2)
                     (ivl 0 80))
             (vector (ivl 1 2)
                     (ivl 360 362)))))

