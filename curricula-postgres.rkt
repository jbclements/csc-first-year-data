#lang racket

;; given a filename and a list of values formattable with ~a,
;; output a tsv file suitable for import with postgresql
(define (postgresql-format filename rows)
  (call-with-output-file filename
    (λ (port)
      (for ([row (in-list rows)])
        (define row+version (append row (list)))
        (fprintf port
                 "~a\n" 
                 (apply string-append
                        (add-between (map ~a row+version) "\t")))))))



(postgresql-format
 "/tmp/curricula.tsv"
'(("Bellardo, John Michael" "Mobile")
  ("DeBruhl II, Bruce Edward" "Security")
  ("Clements, John B." "Music")
  ("Haungs, Michael L." "Gaming")
  ("Janzen, David S." "Mobile")
  ("Peterson, Zachary N.J." "Security")
  ("Seng, John S." "Robotics")
  ("Smith, Hugh M." "Robotics")
  ("Wood, Zoe J." "Art")
  ("Workman, Julie A." "Art")))