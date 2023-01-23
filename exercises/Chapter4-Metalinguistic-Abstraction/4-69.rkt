#lang racket

'(rule (step-son ?m ?s) 
       (and (son ?w ?s) 
            (wife ?m ?w))) 
  
'(rule (has-son ?x ?y) 
       (or (son ?x ?y) 
           (step-son ?x ?y))) 
  
'(rule (grandson ?g ?s) 
       (and (has-son ?g ?f) 
            (has-son ?f ?s))) 

'(rule ((great grandson) ?x ?y)
       (and (has-son ?x ?v)
            (grandson ?v ?y)))

'(rule ((great . (?last-rel . ())) ?x ?y)
       (and (same ?last-rel grandson)
            ((great grandson ?x ?y))))

'(rule ((great . (?u . ?rest-rel)) ?x ?y)
       (and (same ?u great)
            (has-son ?x ?v)
            ((great ?rest-rel) ?v ?y)))

'(rule (?relationship ?x ?y)
       ((great . ?rel) ?x ?y))

'(rule ((great . ?rel) ?x ?y)
       (or (and ((great grandson) ?x ?y)
                (same ?rel (great grandson)))
           (and (has-son ?x ?v)
                ((great . ?further-rel) ?v ?y)
                (append-to-form (great) ?further-rel ?rel))))
            
