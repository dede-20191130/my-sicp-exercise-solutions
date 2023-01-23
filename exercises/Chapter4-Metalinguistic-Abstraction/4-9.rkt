#lang racket

(define (make-begin seq) (cons 'begin seq))
(define (make-if predicate 
                 consequent 
                 alternative)
  (list 'if 
        predicate 
        consequent 
        alternative))

;custom do syntax
;(custom-do ((id init-expr step-expr)
;            (id2 init-expr2 step-expr2))
;           (stop? final-prod)
;           body)


  
(define (c-do-id-binds exp) (cadr exp))
(define (c-do-id-bind-id id-bind) (car id-bind))
(define (c-do-id-bind-init-expr id-bind) (cadr id-bind))
(define (c-do-id-bind-step-expr id-bind) (caddr id-bind))
(define (c-do-stop? exp) (caaddr exp))
(define (c-do-final? exp) (not (null? (cdaddr exp))))
(define (c-do-final-prod exp) (car (cdaddr exp)))
(define (c-do-body exp) (cadddr exp))

(define (custom-do->combination exp)
  (let ((id-binds-for-let (map (λ(item) (list (car item) (cadr item))) (c-do-id-binds exp)))
        (id-step-exps-for-named-let (map (λ(item) (caddr item)) (c-do-id-binds exp))))
    (list
     'let
     'loop-$$$$ ;the name to be supposed to avoid confliction
     id-binds-for-let
     (make-if (c-do-stop? exp)
              (if (c-do-final? exp)
                  (c-do-final-prod exp)
                  (void))
              (make-begin
               (list
                (c-do-body exp)
                (cons 'loop-$$$$ id-step-exps-for-named-let)))))))

;;;test

(custom-do->combination '(custom-do ((id init-expr step-expr)
                                     (id2 init-expr2 step-expr2))
                                    (stop? final-prod)
                                    body))
