#lang racket

; prerequisite primitive operations
;(define (cond-clauses exp) (cdr exp))
;(define (cond-else-clause? clause)
;  (eq? (cond-predicate clause) 'else))
;(define (cond-predicate clause) 
;  (car clause))
;(define (cond-actions clause) 
;  (cdr clause))

'(
  ev-cond
     (assign unev (op cond-clauses) (reg exp))
     (test (op null?) (reg unev))
     (branch (label ev-cond-null))
     (save env)
     (save continue)
     ev-cond-clau-test-loop
     (assign the-clause (op car) (reg unev))
     (test (op cond-else-clause?) (reg the-clause))
     (branch (label ev-cond-do-action))
     (save unev)
     (assign continue (label ev-cond-decide))
     (assign exp (op cond-predicate) (reg the-clause))
     (goto (label eval-dispatch))
     ev-cond-decide
     (restore unev)
     (test (op true?) (reg val))
     (branch (label ev-cond-do-action))
     (assign unev (op cdr) (reg unev))
     (goto (label ev-cond-clau-test-loop))
     ev-cond-null
     (assign exp (const 'false))
     (goto (label eval-dispatch))
     ev-cond-do-action
     (restore continue)
     (restore env)
     (assign exp (op cond-actions) (reg the-clause))
     (assign exp (op sequence->exp) (reg exp))
     (goto (label eval-dispatch)))