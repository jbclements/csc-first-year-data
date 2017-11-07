#lang racket

(require csv-reading
         text-table
         shelly/mail)



;; strip a leading byte order mark from an input port
;(: discard-bom (Input-Port -> Void))
(define (discard-bom p)
  (void (regexp-try-match #rx"^\uFEFF" p)))

(define d
  (call-with-input-file
      "/Users/clements/clements/datasets/ad-hoc-student-progress/student-progress-data.csv"
    (λ (port)
      (discard-bom port)
      (csv->list port))))

(unless (equal? (first d)
                '("MAJOR"
                  "EMPLID"
                  "FIRST_NAME"
                  "LAST_NAME"
                  "EMAIL_ADDRESS"
                  "CONCENTRATION"
                  "TERM"
                  "COURSE"
                  "UNITS_ATTEMPTED"
                  "UNITS_EARNED"
                  "GRADE"))
  (error 'unexpected-columns))

(define row-major first)
(define row-firstname third)
(define row-lastname fourth)
(define row-qtr seventh)

(define (major-rows major)
  (filter (λ (row) (equal? (row-major row) major))
          d))

(define (major-names major-rows)
  (map (λ (g) (first (sort g string<? #:key third)))
  (group-by
   (λ (r) (take r 2))
   (for/list ([r (in-list major-rows)])
     (list (row-lastname r) (row-firstname r) (row-qtr r))))))

(define csc-major-names (major-names (major-rows "CSC")))
(define se-major-names (major-names (major-rows "SE")))
(define cpe-major-names (major-names (major-rows "CPE")))

(define csc-assignment-table
  '((bellardo "ab")
    (clements "cd")
    (debruhl "ef")
    (eckhardt "gh")
    (gharibyan "i")
    (haungs "j")
    (keen "k")
    (khosmood "l")
    (kurfess "m")
    (lupo "n")
    (nico "o")
    (pantoja "p")
    (peterson "qr")
    (seng "s")
    (smith "t")
    (staley "uv")
    (stanchev "")
    (vakalis "wx")
    (wood "yz")))

(define se-assignment-table
  '((bahrami "abcde")
    (dasilva "fghij")
    (falessi "klmno")
    (janzen "pqrst")
    (turner "uvwxyz")))

(define cpe-assignment-table
  '((nico "l")))

(define (compute-assignments major-names assignment-table)
  (for/list ([assignment (in-list assignment-table)])
    (list (first assignment)
          (sort
           (sort
            (filter (λ (name) (member (string-ref (string-downcase
                                                   (first name)) 0)
                                      (string->list (second assignment))))
                    major-names)
            string<?
            #:key first)
           string<?
           #:key third))))

(define csc-assignments
  (compute-assignments csc-major-names csc-assignment-table))
(define se-assignments
  (compute-assignments se-major-names se-assignment-table))
(define cpe-assignments
  (compute-assignments cpe-major-names cpe-assignment-table))

(map (λ (g) (list (first g) (length (second g))))
     csc-assignments)

(map (λ (g) (list (first g) (length (second g))))
     se-assignments)

cpe-assignments

(require plot)
(plot
 (density
  (map length (map second (append se-assignments csc-assignments)))
  0.2))


;; maps names to email addresses
(define name->email
  '((dasilva . bcdasilv)
    (turner . csturner)
    (falessi . dfalessi)
    (janzen . djanzen)
    (keen . akeen)
    (bellardo . bellardo)
    (debruhl . bdebruhl)
    (eckhardt . ceckhard)
    (clements . clements)
    (lupo . clupo)
    (staley . cstaley)
    (dekhtyar . dekhtyar)
    (khosmood . foaad)
    (kurfess . fkurfess)
    (gharibyan . hghariby)
    (smith . husmith)
    (seng . jseng)
    (haungs . mhaungs)
    (pantoja . mpanto01)
    (nico . pnico)
    (bahrami . sbahrami)
    (stanchev . lstanche)
    (vakalis . ivakalis)
    (kearns . tkearns)
    (peterson . znjp)
    (wood . zwood)))

(define intro-text
  "This is a list of your assigned advisees, according to the \
list posted outside the office. I was just curious to know whom I \
was advising, and I figured you might be interested, too. I also \
included the quarter in which they first took one of our classes. \
It uses Cal Poly's goofy numbering scheme, so 2178 is Fall 2017. \
Students showing 2148 or 2152 or 2154 are in their fourth year. \
Anything smaller than that means they're past the four-year mark. \
Students showing 2118 are in their seventh year. You may be surprised \
to see that you have some *really old* students. I was! :)

Apologies in advance for tab-damage on this e-mail; if you don't read \
mail with a fixed-width-font, it's going to look nasty. If you use \
Apple Mail, you can always hit cmd-option-u to see message source.

Also note that I have no information on who's a graduate student, so \
some of the people on this list are actually technically Alex Dekhtyar's \
advisees, and not yours.

Finally, our current scheme is a bit arbitrary; some of you have a whole \
lot of advisees, and some have few or none. I'm guessing this would be \
easy to fix, if we cared. Maybe this is a first step toward caring?

Hope this is interesting to you,

John C.

" )

(define (send-the-emails!)
  (for ([assignment (in-list (append csc-assignments
                                     se-assignments))])
    (displayln (first assignment))
    (define email (cdr (assoc (first assignment) name->email)))
    (define table-str
      (cond [(empty? (second assignment)) "NO ADVISEES"]
            [else (table->string (second assignment)
                                 #:row-sep? #f)]))
  
    (send-email "clements@brinckerhoff.org"
                (~a email "@calpoly.edu")
                "list of your advisees by entry year (JFYI)"
                (string-append intro-text table-str "\n\n\n")
                )))




