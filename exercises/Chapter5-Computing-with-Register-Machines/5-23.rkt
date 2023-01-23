#lang racket

; dispather
'(
  eval-dispatch
  (test (op self-evaluating?) (reg exp))
  (branch (label ev-self-eval))
  (test (op variable?) (reg exp))
  (branch (label ev-variable))
  (test (op quoted?) (reg exp))
  (branch (label ev-quoted))
  (test (op assignment?) (reg exp))
  (branch (label ev-assignment))
  (test (op definition?) (reg exp))
  (branch (label ev-definition))
  (test (op if?) (reg exp))
  (branch (label ev-if))
  (test (op lambda?) (reg exp))
  (branch (label ev-lambda))
  (test (op begin?) (reg exp))
  (branch (label ev-begin))
  (test (op cond?) (reg exp)) ;
  (branch (label ev-cond)) ;
  (test (op let?) (reg exp)) ;
  (branch (label ev-let)) ;
  (test (op application?) (reg exp))
  (branch (label ev-application))
  (goto (label unknown-expression-type)))
  

; cond evaluation

'(
  ev-cond
  ;  (save env)
  ;  (save continue)
  ;  (assign 
  (assign exp (op cond->if) (reg (exp)))
  (goto (label eval-dispatch)))

; let evaluation

; prerequisite primitive operation
;(define (let-var-declarations exp) (cadr exp))
;(define (let-body exp) (caddr exp))
;(define (let? exp)
; (tagged-list? exp 'let))
;(define (the-side-of-p dec) (car dec))
;(define (the-side-of-e dec) (cadr dec))

'(
  ev-let
  ;  (save exp)
  ;    (save env)
  
  ;  (save continue)
  (assign params (op list))
  (assign exps-for-params (op list))
  (assign unev (op let-var-declarations) (reg exp))
  ;  (test (op null?) (unev))
  ;  (branch (label ev-let-combination)) 
  ev-let-decouple-loop
  ;  (test (op last-pair?) (reg unev))
  ;  (branch (label ev-let-last-pairs))
  (test (op null?) (unev))
  (branch (label ev-let-combination)) 
  (assign the-pair (op car) (reg unev))
  (assign one-side (op the-side-of-p) (reg the-pair))
  (assign params (op cons) (reg one-side) (reg params))
  (assign one-side (op the-side-of-e) (reg the-pair))
  (assign exps-for-params (op cons) (reg one-side) (reg exps-for-params))
  (assign unev (op cdr) (reg unev))
  (goto (label ev-let-decouple-loop))
  ;   ev-let-last-pairs
  ev-let-combination
  (assign exp (op let-body) (reg exp))
  (assign new-lambda (op make-lambda) (reg params) (reg exp))
  (assign exp (op cons) (reg new-lambda) (reg exps-for-params))
  (goto (label eval-dispatch)))
