#lang typed/racket

(require typed/rackunit)

(require/typed "student-tracks.rkt"
               [student-tracks (Listof Track)])

(provide all-year-trees)

(define-type ClassOutcome (U 'pass 'nopass))
(define-type QtrOutcome (U 'pass 'nopass 'mixed))

(: vref (All (T) (Index -> ((Vectorof T) -> T))))
(define ((vref n) t)
  (vector-ref t n))

(: class-name ((Vector String ClassOutcome) -> String))
(define (class-name c) (vector-ref c 0))

(: class-outcome ((Vector String ClassOutcome) -> ClassOutcome))
(define (class-outcome c) (vector-ref c 1))

;; represents the decisions made by students to take or not to take a class
;; (though of course the class might just have been full)
(define-type EnrollT (U Take NoTake))
(struct Take ([class : (Listof String)] [num : Natural]
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
  (List Natural (Listof (Vector String ClassOutcome))))

(: next-qtr (Natural -> Natural))
(define (next-qtr qtr)
  (match (modulo qtr 10)
    [(or 2 4 6) (+ 2 qtr)]
    [8 (+ 4 qtr)]))

(check-equal? (next-qtr 2164) 2166)
(check-equal? (next-qtr 2168) 2172)

(: build-enroll-trees
   (Natural (Listof Track) -> (Listof EnrollT)))
(define (build-enroll-trees this-qtr tracks)
  (define nonempty-tracks
    (filter (λ (track) (not (empty? track))) tracks))
  (cond
    [(empty? nonempty-tracks) '()]
    [else
     (when (not (empty? (filter (λ ([t : Track])
                                  (< (caar t) this-qtr))
                                nonempty-tracks)))
       (raise-argument-error
        'build-enroll-trees
        "tracks starting at or after starting qtr"
        1 tracks))
     (define-values (takers notakers)
       (partition (λ ([track : Track])
                    (= (caar track) this-qtr))
                  nonempty-tracks))
     (define by-classes-taken
       (group-by classes-taken-in-first-qtr takers))
     (append (map build-enroll-tree by-classes-taken)
             (list (NoTake (length notakers)
                           (build-enroll-trees
                            (next-qtr this-qtr)
                            notakers))))]))

;; invariant: all of the tracks here are for students who took
;; the same classes in the same qtr, and the list of tracks
;; is not empty
(: build-enroll-tree ((Listof Track) -> EnrollT))
(define (build-enroll-tree tracks)
  (define the-next-qtr (next-qtr (first (first (first tracks)))))
  (define-values (passers somenopassers)
    (partition (λ ([t : Track]) (eq? (qtr-outcome-in-first-qtr t) 'pass))
               tracks))
  (define-values (nopassers mixed)
    (partition (λ ([t : Track]) (eq? (qtr-outcome-in-first-qtr t) 'nopass))
               somenopassers))
  (Take (classes-taken-in-first-qtr (first tracks))
        (length tracks)
        (OutcomeT (length passers)
                  (build-enroll-trees the-next-qtr
                                      (map
                                       (inst rest QtrTrack QtrTrack)
                                       passers))
                  (length nopassers)
                  (build-enroll-trees the-next-qtr
                                      (map
                                       (inst rest QtrTrack QtrTrack)
                                       nopassers))
                  (length mixed)
                  (build-enroll-trees the-next-qtr
                                      (map
                                       (inst rest QtrTrack QtrTrack)
                                       mixed)))))

(: qtr-outcome-in-first-qtr (Track -> QtrOutcome))
(define (qtr-outcome-in-first-qtr track)
  (define grades-in-first-qtr (map class-outcome (second (first track))))
  (grades->outcome grades-in-first-qtr))

(: grades->outcome ((Listof ClassOutcome) -> QtrOutcome))
(define (grades->outcome grades)
  (match grades
    [(list 'pass ...) 'pass]
    [(list 'nopass ...) 'nopass]
    [other 'mixed]))

(check-equal? (grades->outcome '(pass pass pass)) 'pass)
(check-equal? (grades->outcome '(pass nopass pass)) 'mixed)
(check-equal? (grades->outcome '(nopass nopass nopass)) 'nopass)


(: classes-taken-in-first-qtr (Track -> (Listof String)))
(define (classes-taken-in-first-qtr track)
  (define qtr-track (first track))
  (sort (map class-name (second qtr-track)) string<?))

(check-equal?
 (build-enroll-trees
  2158
  '(((2162 (#("CPE 101" pass))))))
 (list (NoTake 1
               (list (Take '("CPE 101")
                           1 (OutcomeT 1 '() 0 '() 0 '()))
                     (NoTake 0 '())))))

(check-equal?
 (build-enroll-trees
  2158
  '(((2158 (#("CPE 101" pass)))
     (2162 (#("CPE 102" nopass))))
    ((2162 (#("CPE 101" pass)))
     (2164 (#("CPE 102" pass))))
    ((2158 (#("CPE 101" pass)))
     (2162 (#("CPE 102" nopass)))
     (2164 (#("CPE 102" pass)
            #("CPE 103" pass))))))
 (list (Take (list "CPE 101") 2
             (OutcomeT
              2 (list
                 (Take (list "CPE 102") 2
                       (OutcomeT
                        0 '()
                        2 (list (Take (list "CPE 102"
                                            "CPE 103")
                                      1
                                      (OutcomeT
                                       1 '()
                                       0 '()
                                       0 '()))
                                (NoTake 0 '()))
                        0 '()))
                 (NoTake 0 '()))
              0 '()
              0 '()))
       (NoTake 1
               (list
                (Take '("CPE 101") 1
                      (OutcomeT
                       1 (list (Take '("CPE 102") 1
                                     (OutcomeT 1 '() 0 '() 0 '()))
                               (NoTake 0 '()))
                       0 '()
                       0 '()))
                (NoTake 0 '())))))

(define earliest-qtr
  (apply min (map (λ ([t : Track]) (car (car t))) student-tracks)))


;; the year whose fall most closely precedes the first quarter
;; this student took a class
(: starting-year (Track -> Natural))
(define (starting-year track)
  (+ 2000 (floor (/ (cast (- (caar track) 2008) Natural) 10))))


(define by-year
  (group-by starting-year student-tracks))

(define earliest-year
  (cast (apply min (map starting-year student-tracks)) Natural))
(define latest-year
  (cast (apply max (map starting-year student-tracks)) Natural))

(: year->fall-qtr (Year -> Natural))
(define (year->fall-qtr year)
  (+ 2000 (* (cast (- year 2000) Natural) 10) 8))

(check-equal? (year->fall-qtr 2005) 2058)

(define-type Year Natural)
(: years-tree (Year -> (Listof EnrollT)))
(define (years-tree year)
  (define found-set (findf
                     (λ ([tracks : (Listof Track)])
                       (= (starting-year (first tracks))
                          year))
                     by-year))
  (cond [(false? found-set)
         (error 'find-reconds
                "no records found for year: ~a" year)]
        [else (build-enroll-trees (year->fall-qtr year) found-set)]))

(define all-year-trees
  (for/list : (Listof (List Year (Listof EnrollT)))
    ([year : Natural
           (ann
            (in-range (ann earliest-year Nonnegative-Integer)
                      (ann (add1 latest-year) Integer))
            (Sequenceof Nonnegative-Integer))])
    (list year (years-tree year))))


(: tree-depth (EnrollT -> Natural))
(define (tree-depth tree)
  (match tree
    [(Take _ _ (OutcomeT _ a _ b _ c))
     (add1 (max (trees-depth a) (trees-depth b) (trees-depth c)))]
    [(NoTake _ a)
     (add1 (trees-depth a))]))

(: trees-depth ((Listof EnrollT) -> Natural))
(define (trees-depth trees)
  (cond [(empty? trees) 0]
        [else (apply max (map tree-depth trees))]))

(map trees-depth (map (inst second Year (Listof EnrollT) Any)
                      all-year-trees))



