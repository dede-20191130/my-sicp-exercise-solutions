#lang racket

(define (tag exp) (car exp))

(define (eval exp env)
  (cond ((self-evaluating? exp) 
         exp)
        ((variable? exp) 
         (lookup-variable-value exp env))
        (else (let ((tagged-evaler (get 'eval (tag exp))))
                (cond (tagged-evaler (tagged-evaler exp))
                      ((application? exp)
                       (apply (eval (operator exp) env)
                              (list-of-values 
                               (operands exp) 
                               env)))
                      (else
                       (error "Unknown expression 
                 type: EVAL" exp)))))))


(define (install-eval-quoted)
  (define (text-of-quotation exp)
    (cadr exp))
  (put 'eval 'quote text-of-quotation)
  'done)

;...and more installer procedures