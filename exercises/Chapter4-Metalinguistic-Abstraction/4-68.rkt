#lang racket

;'(rule (reverse (?u . ()) ?u))
;'(rule (reverse (?u . ?x) ?y)
;       (append-to-form ?x ?u ?y))


'(rule (reverse (?a . ()) (?b . ()))
       (same ?a ?b))
'(rule (reverse (?u . ?rest) ?x)
       (and (reverse ?rest ?y)
            (append-to-form ?y ?u ?x)))
'(rule (reverse ?x (?u . ?rest))
       (and (reverse ?rest ?y)
            (append-to-form ?y ?u ?x)))
