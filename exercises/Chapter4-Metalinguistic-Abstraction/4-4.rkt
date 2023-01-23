#lang racket

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
        ((begin? exp)
         (eval-sequence 
          (begin-actions exp) 
          env))
        ((cond? exp) 
         (eval (cond->if exp) env))
        ((and? exp)
         (eval-and (operands exp) env))
        ((or? exp)
         (eval-or (operands exp) env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values 
                 (operands exp) 
                 env)))
        (else
         (error "Unknown expression 
                 type: EVAL" exp))))

(define (and? exp)
  (tagged-list? exp 'and))
(define (eval-and exps env)
  (if (no-operands? exps)
      'true
      (let ((first-operand-predicting (eval (first-operand exps) env)))
        (if (not first-operand-predicting)
            'false
            (eval-and (rest-operands exps) env)))))

(define (eval-and-as-derived exps env)
  (define (create-if-chain tgt-exps)
    (if (no-operands? tgt-exps)
        'true
        (make-if (first-operand tgt-exps)
                 (create-if-chain (rest-operands tgt-exps))
                 'false)))
  (eval (create-if-chain exps) env))

(define (or? exp)
  (tagged-list? exp 'or))
(define (eval-or exps env)
  (if (no-operands? exps)
      'false
      (let ((first-operand-predicting (eval (first-operand exps) env)))
        (if first-operand-predicting
            'true
            (eval-or (rest-operands exps) env)))))

(define (eval-or-as-derived exps env)
  (define (create-if-chain tgt-exps)
    (if (no-operands? tgt-exps)
        'false
        (make-if (first-operand tgt-exps)
                 'true
                 (create-if-chain (rest-operands tgt-exps)))))
  (eval (create-if-chain exps) env))
