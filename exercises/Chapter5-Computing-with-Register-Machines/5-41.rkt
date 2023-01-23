#lang racket
(require rackunit)
(require compatibility/mlist)

(define NOT-FOUND 'not-found)

(define (find-variable var cenv)
  (define (env-loop cenv frame-number)
    (define (scan frm displacement-number)
      (cond ((null? frm)
             (env-loop 
              (enclosing-environment cenv)
              (add1 frame-number)))
            ((eq? var (mcar frm))
             (list frame-number displacement-number))
            (else (scan (mcdr frm) 
                        (add1 displacement-number)))))
    (if (eq? cenv the-empty-environment)
        NOT-FOUND
        (scan (first-frame cenv) 0)))
  (env-loop cenv 0))

;;; module for test
(define the-empty-environment '())
(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))

;;; test code
(define cenv-1 `(,(mlist 'y 'z) ,(mlist 'a 'b 'c 'd 'e) ,(mlist 'x 'y)))

(check-equal?
 (find-variable 
  'y cenv-1)
 '(0 0))

(check-equal?
 (find-variable 
  'a cenv-1)
 '(1 0))

(check-equal?
 (find-variable 
  'c cenv-1)
 '(1 2))

(check-equal?
 (find-variable 
  'e cenv-1)
 '(1 4))

(check-equal?
 (find-variable 
  'x cenv-1)
 '(2 0))

(check-equal?
 (find-variable 
  'w cenv-1)
 NOT-FOUND)