#lang racket

(require csv-reading
         "grades.rkt")


;; there are three dimensions here:
;; 1) class (101 or 102)
;; 2) AP vs. non-AP
;; 3) time period

(define TIMESTR "2158-")

;; (instructor, grade, count)
(define all-ap-101-stats
  (call-with-input-file (~a "/tmp/ap-students-101-stats-"TIMESTR".txt")
    csv->list))
;; can't use last year of 102 students, they haven't taken 103 yet...
(define all-ap-102-stats
  (call-with-input-file (~a "/tmp/ap-students-102-stats-"TIMESTR".txt")
    csv->list))

;; all of the students that took 123 (needed in order to compute
;; how many of them dropped out; these should be considered fails)
;; (instructor, count)
(define ap-student-nums
  (call-with-input-file (~a "/tmp/tot-ap-students-"TIMESTR".txt")
    csv->list))

;; (instructor, count)
(define ap-101-skippers
  (call-with-input-file (~a "/tmp/skipped-101-ap-"TIMESTR".txt")
    csv->list))
(define ap-102-skippers
  (call-with-input-file (~a "/tmp/skipped-102-ap-"TIMESTR".txt")
    csv->list))


(define all-101-stats all-ap-101-stats)
(define all-102-stats all-ap-102-stats)

(define student-nums ap-student-nums)

(define 101-skippers ap-101-skippers)
(define 102-skippers ap-102-skippers)


#;(
   ;; AP students in 102 from 2158 and before (prior to 101 change?)
   (define all-students
  (call-with-input-file "/tmp/ap-students-102-stats-2158-.txt"
    csv->list))

(define student-nums
  (call-with-input-file "/tmp/tot-ap-students-2158-.txt"
    csv->list))

(define skippers
  (call-with-input-file "/tmp/skipped-102-ap-2158-.txt"
    csv->list)))

#;(
   ;; NON-AP students from 2158 and before (prior to 101 change?)
   (define all-students
  (call-with-input-file "/tmp/non-ap-students-102-stats-2158-.txt"
    csv->list))

(define student-nums
  (call-with-input-file "/tmp/tot-non-ap-students-2158-.txt"
    csv->list))

(define skippers
  (call-with-input-file "/tmp/skipped-102-non-ap-2158-.txt"
    csv->list)))

#;(;; NON-AP students from 2118 and after
(define all-students
  (call-with-input-file "/tmp/non-ap-students-101-stats-2118+.txt"
    csv->list))

(define student-nums
  (call-with-input-file "/tmp/tot-non-ap-students-2118+.txt"
    csv->list))

(define skippers
  (call-with-input-file "/tmp/skipped-101-non-ap-2118+.txt"
    csv->list)))

student-nums

101-skippers
102-skippers

;; given an instructor, produce the curriculum label
(define (curriculum-label instructor)
  (match instructor
    ("Bellardo, John Michael" 'mobile)
    ("DeBruhl II, Bruce Edward" 'security)
    ("Clements, John B." 'music)
    ("Haungs, Michael L." 'gaming)
    ("Janzen, David S." 'mobile)
    ("Peterson, Zachary N.J." 'security)
    ("Seng, John S." 'robotics)
    ("Smith, Hugh M." 'robotics)
    ("Wood, Zoe J." 'art)
    ("Workman, Julie A." 'art)))

;; given the curriculum label, produce the curriculum name
(define (curriculum-name c)
  (match c
    ['mobile "Mobile"]
    ['music "Music"]
    ['security "Security"]
    ['robotics "Robotics"]
    ['art "Art"]
    ['gaming "Gaming"]))

;; all of the curriculum labels
(define all-curricula
  (remove-duplicates
   (map curriculum-label
        (map first
             (append all-101-stats
                     all-102-stats)))))

all-curricula

;; given a grade such as B-, C+, etc., return a GPA number,
;; using 0.0 for all non-letter grades
(define (grade->gpa g)
  (match g
    [(regexp #px"^([ABCD])\\+$"
             (list _ base-grade))
     (+ (grade->gpa base-grade) 0.3)]
    [(regexp #px"^([ABCD])\\-$"
             (list _ base-grade))
     (- (grade->gpa base-grade) 0.3)]
    [(regexp #px"^[ABCD]$")
     (- 4 (- (char->integer (string-ref g 0)) (char->integer #\A)))]
    [other 0.0]))

(require rackunit)
(check-equal? (grade->gpa "D+") 1.3)
(check-equal? (grade->gpa "B-") 2.7)
(check-equal? (grade->gpa "WU") 0.0)
(check-equal? (grade->gpa "GU") 0.0)



(define (strip-quotes grade)
  (second (regexp-match #px"^ *\"([^\"]+)\"$" grade)))

;; all of the grades that appear:
(define all-gpa-levels
  (cons "GU" (remove-duplicates (map second
                                     (append all-101-stats
                                             all-102-stats)))))

(define dfw-grades
  (remove* success-grades all-gpa-levels))

(printf "non-passing grades: ~v\n" dfw-grades)


;; (label x count#)
(define curriculum-majors
  (map (λ (rs)
         (list (curriculum-label (first (first rs)))
               (apply + (map string->number
                             (map string-trim (map second rs))))))
       (group-by (λ (r) (curriculum-label (first r)))
                 student-nums)))


;; given grade stats and skippers stats, return a function
;; that accepts label and grade and returns the # of people
;; getting that grade.
(define (jujubee stats skippers)


  ;; given an (instructor x grade level x count) table,
  ;; produce a (label x grade level x count#) table
  (define grades-by-curriculum
    (map
     (λ (r) (list (curriculum-label (first r))
                  ;; took out grade->gpa mapping here:
                  (second r)
                  (string->number (string-trim (third r)))))
     stats))


  ;; group by curriculum (necessary because e.g. Workman & Wood both teach art
  (define summed-grades-by-curriculum
    (map
     (λ (rs) (list (first (first rs))
                   (second (first rs))
                   (apply + (map third rs))))
     (group-by
      (λ (r) (take r 2))
      grades-by-curriculum)))


  ;; how many people came from each curriculum?
  (define curriculum-totals
    (map
     (λ (rs) (list (first (first rs))
                   (apply + (map third rs))))
     (group-by first summed-grades-by-curriculum)))
  

  ;; (label x count#)
  (define curriculum-skippers
    (map (λ (rs)
           (list (curriculum-label (first (first rs)))
                 (apply + (map string->number (map string-trim (map second rs))))))
         (group-by (λ (r) (curriculum-label (first r)))
                   skippers)))

  ;; giving up means you took 123 but not this class, unless you skipped it.
  (define gave-ups
    (for/list ([curriculum (in-list all-curricula)])
      (list curriculum
            (- (first (dict-ref curriculum-majors curriculum))
               (first (dict-ref curriculum-totals curriculum))
               (first (dict-ref curriculum-skippers curriculum (list 0)))))))


  ;; ((curriculum x grade) x count#)
  (define summed-grades-by-curriculum-and-grade
    (map (λ (r) (list (take r 2) (third r)))
         summed-grades-by-curriculum))

  ;; given a curriculum and a grade, return the # of people in it.
  ;; use the special grade "GU" for people that elected not to enroll.
  (define (curriculum-grade-count curriculum gpa)
    (cond [(equal? gpa "GU")
           (first (dict-ref gave-ups curriculum))]
          [else
           (first
            (dict-ref summed-grades-by-curriculum-and-grade
                      (list curriculum gpa)
                      (list 0)))]))

  curriculum-grade-count
)

(define juju1 (jujubee all-101-stats 101-skippers))
(define juju2 (jujubee all-102-stats 102-skippers))
(define (curriculum-grade-count curriculum gpa)
  (+ (juju1 curriculum gpa)
     (juju2 curriculum gpa)))

(call-with-output-file "/tmp/grade-samples.csv"
  #:exists 'truncate
  (λ (port)
    (fprintf port "curriculum,gpa\n")
    (for ([curriculum (in-list all-curricula)])
      (for ([grade (in-list all-gpa-levels)])
        (for ([i (in-range (curriculum-grade-count curriculum grade))])
          (fprintf port "~a,~a\n" (curriculum-name curriculum)
                   #;grade
                   (cond [(member grade success-grades) "PASS"]
                         [(equal? grade "GU") "GAVE UP"]
                         [else "FAIL"])))))))

(define curriculum-samples
  (for/list ([curriculum (in-list
                          all-curricula)])
    (list curriculum
          (apply
           append
           (for/list ([grade (in-list all-gpa-levels)])
             (for/list ([i (in-range (curriculum-grade-count curriculum grade))])
               (grade->gpa grade)))))))

(require plot)
(plot-file
 (for/list ([i (in-naturals)]
            [pr (in-list curriculum-samples)]
            [style (in-list (list 0 1 2 3 5 4))])
   (density (second pr)
            #:label (symbol->string (first pr))
            #:style style
            #:color i))
 #:x-label "GPA of non-AP student in 101"
 #:y-label "density of non-AP students with this GPA"
 "/tmp/non-ap-101-grades-2118+.pdf")

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

