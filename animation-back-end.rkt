#lang racket

(require "animation-gl-back-end.rkt")

(provide OFF-LEFT-POSN
         OFF-RIGHT-POSN
         class/grade-posn
         run-transes
         (struct-out shapetrans)
         (struct-out transition))

;; a transition contains a string and a list of shapetrans'es
(struct transition (label transes) #:transparent)

;; a shapetrans contains something, a starting
;; position, and an ending position. It represents
;; a shape that moves from the start-pos to the end-pos
;; through the transition
(struct shapetrans (image start-pos end-pos) #:transparent)

;; a world is a VECTOR of transitions and the time of the beginning
;; of the whole animation (in ms since epoch) and the time since
;; the beginning of the whole animation (in ms)
(struct world (transitions animation-start animation-time) #:transparent)

;; represents a position on the screen
(struct posn (x y) #:transparent)


;; a key-frame is a map from symbols to shape-posns

;; a shape-posn has a shape and a posn
(struct shape-posn (image posn))

;; in milliseconds:
(define TRANSITION-TIME 1000)

(define SCENEWIDTH 1300)
(define SCENEHEIGHT 750)

(define INSET 50)
(define SPACING 5)

(define TITLE-SIZE 25)
(define TITLE-BOTTOM-INSET 8)

;; width and height of blue user icon box
(define IMAGEWIDTH 4)
(define IMAGEHEIGHT 4)

(define GL-X-LFT -1.0)
(define GL-X-RGT 1.0)
(define gl-x-wid (- GL-X-RGT GL-X-LFT))
(define GL-Y-TOP 1.0)
(define GL-Y-BOT -1.0)
(define gl-y-hgt (- GL-Y-BOT GL-Y-TOP))

(define BG-BOX-Z -2.0)
(define STUDENT-Z -1.8)

;; the visual ordering of the grades
(define grade-ordering
  (reverse
  '(("AU" "U" "I" "W" "WU" "NC" "F")
    ("D-") ("D") ("D+") ("C-") ("C")
    ("C+") ("B-") ("B") ("B+") ("A-") ("A"))))
(define grade-count (length grade-ordering))

;; the colors of the grade boxes
(define grade-coloring
  (let ()
   (define f (list 1.0 0.9 0.9))
   (define d (list 1.0 0.95 0.95))
   (define c (list 0.9 0.9 1.0))
   (define b (list 0.9 1.0 0.9))
   (define a (list 1.0 1.0 1.0))
  (reverse
   (list f d d d c c c b b b a a))))

;; the ordering of the classes, left to right
(define class-ordering
  '("CPE 123" "CPE 101" "CPE 102" "CPE 103"))
(define class-count (length class-ordering))


;; the width of the box for one possible class
(define class-width
  (floor (/ (- SCENEWIDTH (* 2 INSET) (* (sub1 class-count) SPACING))
            class-count)))

;; the height of the box for one possible grade (e.g.: A, A-, D, WU, etc.)
(define grade-height
  (floor (/ (- SCENEHEIGHT (* 2 INSET) (* (sub1 grade-count) SPACING))
            grade-count)))

;; the background image with boxes for each class/grade pair
(define (draw-background!)
  (for* ([class (in-range class-count)]
         [grade (in-range grade-count)])
    (define x0 (+ INSET (* class (+ SPACING class-width))))
    (define y0 (+ INSET (* grade (+ SPACING grade-height))))
    (match-define (list r g b) (hash-ref grade-coloring-hash grade))
    (draw-rectangle! r g b
                     (to-gl-x x0) (to-gl-x (+ x0 class-width))
                     (to-gl-y y0) (to-gl-y (+ y0 grade-height))
                     BG-BOX-Z)))

;; add the title to a scene
#;(define (add-title title scene)
  (place-image/align (text title TITLE-SIZE "darkgray")
                     (/ SCENEWIDTH 2)
                     (- SCENEHEIGHT TITLE-BOTTOM-INSET)
                     'center 'bottom
                     scene))

;; map a class & grade & number to a position
;; currently, just use the middle of the corresponding box
(define (class/grade-posn class grade id)
  (define class-idx (hash-ref class-ordering-hash class))
  (define grade-idx (hash-ref grade-ordering-hash grade))
  (define id-offset (id->offset id))
  (posn (+ INSET (* (+ 0.2 (* 0.6 (posn-x id-offset)) class-idx)
                    (+ SPACING class-width)))
        (+ INSET (* (+ 0.2 (* 0.6 (posn-y id-offset)) grade-idx)
                    (+ SPACING grade-height)))))

(define OFF-LEFT-POSN (posn (- INSET) (/ SCENEHEIGHT 2)))
(define OFF-RIGHT-POSN (posn (+ SCENEWIDTH INSET) (/ SCENEHEIGHT 2)))

;; a map from a grade to a grade index (natural number)
(define grade-ordering-hash
  (make-immutable-hash
   (apply
    append
    (for/list ([grades (in-list grade-ordering)]
               [i (in-naturals)])
      (for/list ([g (in-list grades)])
        (cons g i))))))

;; a map from a grade index to a list of 3 real numbers
(define grade-coloring-hash
  (for/hash ([i (in-naturals)]
             [color (in-list grade-coloring)])
    (values i color)))

;; a map from a class name to a class index (natural number)
(define class-ordering-hash
  (for/hash ([class (in-list class-ordering)]
             [i (in-naturals)])
    (values class i)))

;; given an id, return a random posn in ((0..1),(0..1)). Returns
;; the same posn for the same id.
(define id-posn-hash (make-hash))
(define (id->offset id)
  (match (hash-ref id-posn-hash id #f)
    [#f (define new-offset (posn (random) (random)))
        (hash-set! id-posn-hash id new-offset)
        new-offset]
    [(? posn? p) p]))

;; given fraction of transition and shapetrans, compute
;; shape position
(define (trans-posn frac st)
  (define 1-frac (- 1 frac))
  (match-define (shapetrans _ (posn sx sy) (posn ex ey)) st)
  (posn (+ (* 1-frac sx) (* frac ex))
        (+ (* 1-frac sy) (* frac ey))))

;; nonlinear motion function: given t between 0 and 1, return
;; another t between 0 and 1
(define (motion-reshaper t)
  (cond [(< t trans-frac)
         (- 0.5 (* 0.5 (cos (* (/ t trans-frac) pi))))]
        [else 1.0]))
;; motion occurs in this fraction of the period:
(define trans-frac 0.7)


;; given fraction of transition, shapetrans, and scene, overlay
;; shapetrans-image at the correct location
(define (add-image! frac st)
  (match (trans-posn (motion-reshaper frac) st)
    [(posn x y) (draw-student! x y)]))

;; given x and y positions in pixel space, draw a student
(define (draw-student! x y)
  (draw-rectangle! 0.0 0.0 1.0
                   (to-gl-x x) (to-gl-x (+ x IMAGEWIDTH))
                   (to-gl-y y) (to-gl-y (+ y IMAGEHEIGHT))
                   STUDENT-Z))

;; translate a pixel-space x to a gl-space x
(define (to-gl-x x) 
  (+ GL-X-LFT (* (/ x SCENEWIDTH) gl-x-wid)))

;; translate a pixel-space x to a gl-space x
(define (to-gl-y y) 
  (+ GL-Y-TOP (* (/ y SCENEHEIGHT) gl-y-hgt)))

;; draw a world
(define (draw-world! w)
  (match-define (world transitions animation-start animation-time) w)
  ;; expressed in units of transitions:
  (define timepos (/ animation-time TRANSITION-TIME))
  (define step (inexact->exact (floor timepos)))
  (define frac (- timepos step))
  (define prev-steps (min step (vector-length transitions)))
  ;; draw trailing lines for previous steps:
  (for ([i (in-range prev-steps)])
    (add-old-trails! (vector-ref transitions i)))
  (cond [(<= (vector-length transitions) step)
         'do-nothing]
        [else
         (add-current-trails! (vector-ref transitions step) frac)
         (match-define (transition title transes)
           (vector-ref transitions step))
         (for ([st (in-list transes)])
           (add-image! frac st))]))

;; draw transparent lines for old trails in a trans
(define (add-old-trails! transition)
  (define transes (transition-transes transition))
  (for ([st (in-list transes)])
    (match-define (shapetrans _ (posn sx sy) (posn ex ey)) st)
    (draw-line! (list (vector (to-gl-x sx)
                              (to-gl-y sy))
                      (vector (to-gl-x ex)
                              (to-gl-y ey))) -1.2)))

;; draw transparent lines for the current trails in a trans
(define (add-current-trails! transition frac)
  (define transes (transition-transes transition))
  (for ([st (in-list transes)])
    (match-define (shapetrans _ (posn sx sy) _) st)
    (match-define (posn ex ey) (trans-posn (motion-reshaper frac) st))
    (draw-line! (list (vector (to-gl-x sx)
                              (to-gl-y sy))
                      (vector (to-gl-x ex)
                              (to-gl-y ey))) -1.2)))

;; advance the world based on the current time
(define (tick-fun w cur-ms)
  (match-define (world transitions animation-start animation-time) w)
  (world transitions animation-start (- cur-ms animation-start)))

;; run a list of transitions
(define (run-transes transitions)
  (define saved-world (world transitions (current-inexact-milliseconds) 0))
  (gl-go
   (λ ()
     (define updated-world (tick-fun saved-world
                                     (current-inexact-milliseconds)))
     (draw-background!)
     (draw-world! updated-world)
     (set! saved-world updated-world))))

(module+ test
  (require rackunit)

  #;(gl-go
   (λ ()
     (draw-student! 0 0)
     (draw-student! 10 10)
     (draw-student! 100 20)))


  ;; should show students at (255, 115) and (15, 65)
  #;(gl-go
   (λ ()
     (draw-world!
      (world (vector
              (transition
               "-> 2148"
               (list
                (shapetrans 'dummy (posn 10 30) (posn 20 100))
                (shapetrans 'dummy (posn -10 130) (posn 520 100))))
              (transition
               "-> 2162"
               (list
                (shapetrans 'dummy (posn 10 30) (posn 20 100))
                (shapetrans 'dummy (posn -10 130) (posn 520 100)))))
             0
             (* 1/2 TRANSITION-TIME)))))

  ;; now includes randomness...
  #;(check-equal? (class/grade-posn "CPE 102" "B-" 1)
             ;; approximate:
             #;(posn 450 250)
             ;; regression testing:
             (posn 1605/2 487/2))

  #;(gl-go (λ ()
           (draw-blue-rectangle 0.25 0.5 0.25 0.75)))
  
  (check-equal? (to-gl-x 0) GL-X-LFT)
  (check-equal? (to-gl-x SCENEWIDTH) GL-X-RGT)
  (check-equal? (to-gl-x (* 1/2 SCENEWIDTH)) (/ (+ GL-X-LFT GL-X-RGT) 2))
  (check-equal? (to-gl-y 0) GL-Y-TOP)
  (check-equal? (to-gl-y SCENEHEIGHT) GL-Y-BOT)
  (check-equal? (to-gl-y (* 1/2 SCENEHEIGHT)) (/ (+ GL-Y-TOP GL-Y-BOT) 2))

  (check-equal? (trans-posn 0 (shapetrans 'dummy (posn 10 30) (posn -50 100)))
                (posn 10 30))
  (check-equal? (trans-posn 1/2 (shapetrans 'dummy (posn 10 30) (posn -50 100)))
                (posn -20 65))
  (check-equal? (trans-posn 1 (shapetrans 'dummy (posn 10 30) (posn -50 100)))
                (posn -50 100))

  (check-equal? (tick-fun (world '#(a b) 22341 34) 22443)
                (world '#(a b) 22341 102))
  (check-equal? (tick-fun (world '#(a b) 22341 120) 23343)
                (world '#(a b) 22341 1002))
  
  (run-transes (vector
                (transition
                 "-> 2148"
                 (list
                  (shapetrans 'dummy (posn 10 30) (posn 20 100))
                  (shapetrans 'dummy (posn -10 130) (posn 520 100))))
                (transition
                 "-> 2152"
                 (list
                  (shapetrans 'dummy (posn 20 100) (posn 10 30))
                  (shapetrans 'dummy (posn 520 100) (posn 300 200))))))

  (check-= (motion-reshaper 0.0) 0.0 1e-8) 
  (check-= (motion-reshaper 1.0) 1.0 1e-8)

  

  (check-equal? (hash-ref grade-ordering-hash "C-") 7)
  (check-equal? (hash-ref grade-ordering-hash "U") 11))