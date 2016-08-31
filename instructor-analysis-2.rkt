#lang racket

(require csv-reading)

(define all-students
  (call-with-input-file "/tmp/all-students-101-stats-2148+.txt"
    csv->list))

(define student-nums
  (call-with-input-file "/tmp/tot-students.txt"
    csv->list))

(define skippers
  (call-with-input-file "/tmp/skipped-101.txt"
    csv->list))

student-nums

skippers

(define (curriculum-mapping instructor)
  (match instructor
    ("Bellardo, John Michael" 'mobile)
    ("Clements, John B." 'music)
    ("Haungs, Michael L." 'gaming)
    ("Janzen, David S." 'mobile)
    ("Peterson, Zachary N.J." 'security)
    ("Seng, John S." 'robotics)
    ("Smith, Hugh M." 'robotics)
    ("Wood, Zoe J." 'art)
    ("Workman, Julie A." 'art)))

(define (curriculum-name c)
  (match c
    ['mobile "Mobile"]
    ['music "Music"]
    ['security "Security"]
    ['robotics "Robotics"]
    ['art "Art"]
    ['gaming "Gaming"]))

(define all-curricula
  (remove-duplicates (map curriculum-mapping (map first all-students))))

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

(define passing-grades
  '("A" "A-" "B+" "B" "B-" "C+" "C" "C-"))



(define (strip-quotes grade)
  (second (regexp-match #px"^ *\"([^\"]+)\"$" grade)))

(define grades-by-curriculum
  (map
   (λ (r) (list (curriculum-mapping (first r))
                ;; took out grade->gpa mapping here:
                (second r)
                (string->number (string-trim (third r)))))
   all-students))

(define all-gpa-levels
  (cons "GU" (remove-duplicates (map second grades-by-curriculum))))

(define not-passing-grades
  (remove* passing-grades all-gpa-levels))

(printf "non-passing grades: ~v\n" not-passing-grades)

(define summed-grades-by-curriculum
(map
 (λ (rs) (list (first (first rs))
               (second (first rs))
               (apply + (map third rs))))
 (group-by
 (λ (r) (take r 2))
 grades-by-curriculum)))

(define curriculum-totals-from-101-grades
  (map
   (λ (rs) (list (first (first rs))
                 (apply + (map third rs))))
   (group-by first summed-grades-by-curriculum)))

(define curriculum-majors
  (map (λ (rs)
         (list (curriculum-mapping (first (first rs)))
               (apply + (map string->number (map string-trim (map second rs))))))
       (group-by (λ (r) (curriculum-mapping (first r)))
                 student-nums)))

(define curriculum-skippers
  (map (λ (rs)
         (list (curriculum-mapping (first (first rs)))
               (apply + (map string->number (map string-trim (map second rs))))))
       (group-by (λ (r) (curriculum-mapping (first r)))
                 skippers)))

(define gave-ups
  (for/list ([curriculum (in-list all-curricula)])
    (list curriculum
          (- (first (dict-ref curriculum-majors curriculum))
             (first (dict-ref curriculum-totals-from-101-grades curriculum))
             (first (dict-ref curriculum-skippers curriculum (list 0)))))))

(define summed-grades-by-curriculum-and-grade
  (map (λ (r) (list (take r 2) (third r)))
       summed-grades-by-curriculum))

(define (curriculum-grade-count curriculum gpa)
  (cond [(equal? gpa "GU")
         (first (dict-ref gave-ups curriculum))]
        [else
         (first
          (dict-ref summed-grades-by-curriculum-and-grade
                    (list curriculum gpa)
                    (list 0)))]))

(check-equal? (curriculum-grade-count 'gaming 2) 14)
(check-equal? (curriculum-grade-count 'art 0.0) (+ 14 42))

(call-with-output-file "/tmp/grade-samples.csv"
  #:exists 'truncate
  (λ (port)
    (fprintf port "curriculum,gpa\n")
    (for ([curriculum (in-list all-curricula)])
      (for ([grade (in-list all-gpa-levels)])
        (for ([i (in-range (curriculum-grade-count curriculum grade))])
          (fprintf port "~a,~a\n" (curriculum-name curriculum)
                   #;grade
                   (cond [(member grade passing-grades) "PASS"]
                     [else "FAIL"])))))))

(define curriculum-samples
  (for/list ([curriculum (in-list all-curricula)])
    (list curriculum
          (apply
           append
           (for/list ([grade (in-list all-gpa-levels)])
             (for/list ([i (in-range (curriculum-grade-count curriculum grade))])
               grade))))))

#;(require plot)
#;(plot-file
 (for/list ([i (in-naturals)][pr (in-list curriculum-samples)])
   (density (second pr)
            #:label (symbol->string (first pr))
            #:style i
            #:color i))
 #:x-label "gpa of students in 101"
 #:y-label "density of students with this gpa"
 "/tmp/101-grades.pdf")

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
;; evidence to support null hypothesis.
