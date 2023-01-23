#lang racket

(define (make-if predicate 
                 consequent 
                 alternative)
  (list 'if 
        predicate 
        consequent 
        alternative))

(define (unless-predicate exp) (cadr exp))
(define (unless-consequent exp) (caddr exp))
(define (unless-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))
(define (unless->combination exp)
  (make-if (unless-predicate exp)
           (unless-alternative exp)
           (unless-consequent exp)))


;;;test 


(define my-eval
  (let ((ns (make-base-namespace)))
    (lambda (expr) (eval expr ns))))

(define unless-exp '(unless (memq 'some-unnecessary-number '(1 2 6 7 ))
                      'ok
                      'ng))
(define unless-exp2 '(unless (memq 'some-unnecessary-number '(1 2 6 some-unnecessary-number 7 ))
                      'ok
                      'ng))

(my-eval (unless->combination unless-exp))
(my-eval (unless->combination unless-exp2))