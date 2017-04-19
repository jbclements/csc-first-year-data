#lang racket

(require "emplid-hasher.rkt")
(map (λ (r)
       (list (first r)
             (emplid->hashed (second r))))
'(["jjbaer@calpoly.edu" "008389377"]
  ["acompani@calpoly.edu" "006881637"]
  ["kkeim@calpoly.edu" "006982556"]
  ["mboyken@calpoly.edu" "008602954"]
  ["heseo@calpoly.edu" "009908583"]
  ["zgutierr@calpoly.edu" "011830360"]
  ["jkim271@calpoly.edu" "011287363"]
  ["ekolokow@calpoly.edu" "007735360"]
  ["amvelasq@calpoly.edu" "006932844"]
  ["ejohns72@calpoly.edu" "011009215"]
  ["bgalvanb@calpoly.edu" "010843725"]
  ["hhwang02@calpoly.edu" "006640045"]
  ["kcruz03@calpoly.edu" "006898108"]
  ["astoytce@calpoly.edu" "006736661"]
  ["iguzmanl@calpoly.edu" "011177864"]
  ["smkoski@calpoly.edu" "011065323"]
  ["symendoz@calpoly.edu" "006740340"]
  ["smendo12@calpoly.edu" "011113995"]))