#lang racket

(define (let-var-declarations exp) (cadr exp))
(define (let-body exp) (caddr exp))

(define (eval exp env)
  (cond ((self-evaluating? exp) 
         exp)
        ((variable? exp) 
         (lookup-variable-value exp env))
        ((quoted? exp) 
         (text-of-quotation exp))
        ((assignment? exp) 
         (eval-assignment exp env))
        ((definition? exp) 
         (eval-definition exp env))
        ((if? exp) 
         (eval-if exp env))
        ((lambda? exp)
         (make-procedure 
          (lambda-parameters exp)
          (lambda-body exp)
          env))
        ((let? exp)
         (eval (let->combination exp) env))
        ((begin? exp)
         (eval-sequence 
          (begin-actions exp) 
          env))
        ((cond? exp) 
         (eval (cond->if exp) env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values 
                 (operands exp) 
                 env)))
        (else
         (error "Unknown expression 
                 type: EVAL" exp))))

(define (let? exp)
  (tagged-list? exp 'let))

(define (let->combination exp)
  (let ((var-declarations (let-var-declarations exp)))
    (let ((params (map (λ(dec) (car dec))) var-declarations)
          (exps-for-params (map (λ(dec) (cadr dec))) var-declarations))
      (cons (make-lambda params
                         (let-body exp))
            exps-for-params))))