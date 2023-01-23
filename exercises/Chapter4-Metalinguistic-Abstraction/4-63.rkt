#lang racket

;'(rule (grandson ?g ?s)
;       (and (father ?g ?f)
;            (father ?f ?s)))
;'(rule (father ?m ?s)
;       (or (son ?m ?s)
;           (and (wife ?w ?m)
;                (son ?w ?s))))

'(rule (step-son ?m ?s) 
       (and (son ?w ?s) 
            (wife ?m ?w))) 
  
'(rule (has-son ?x ?y) 
       (or (son ?x ?y) 
           (step-son ?x ?y))) 
  
'(rule (grandson ?g ?s) 
       (and (has-son ?g ?f) 
            (has-son ?f ?s))) 

