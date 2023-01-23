#lang racket

;Input below proc into 4.27 system
(define (return-+-or-custumized-proc key)
  (if (eq? key 'plus)
      +
      (lambda (a b) (+ a b 10000))))

;And try these
((return-+-or-custumized-proc 'plus) 2 3)
((return-+-or-custumized-proc 'custom) 2 3)