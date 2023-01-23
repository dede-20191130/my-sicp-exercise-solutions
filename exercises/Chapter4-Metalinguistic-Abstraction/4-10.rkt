#lang racket

;new ternary operator syntax
;(? predicate consequent alternative)

(define my-eval
  (let ((ns (make-base-namespace)))
    (lambda (expr) (eval expr ns))))

(define (ternary-predicate exp) (cadr exp))
(define (ternary-consequent exp) (caddr exp))
(define (ternary-alternative exp)(cadddr exp))
  
(define (eval-ternary exp)
  (if (my-eval (ternary-predicate exp) )
      (my-eval (ternary-consequent exp) )
      (my-eval (ternary-alternative exp) )))

;;;test

(eval-ternary '(? (> 3 1) (* 7 13) (* 17 19)))
(eval-ternary '(? (> 3 5) (* 7 13) (* 17 19)))