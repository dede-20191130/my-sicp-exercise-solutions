#lang racket
(require compatibility/mlist)

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-frame variables values)
  (define (iter vars vals)
    (if (null? vars)
        (mlist)
        (mcons (mcons (car vars)
                      (car vals))
               (iter (cdr vars)
                     (cdr vals)))))
  (mcons '*frame* (iter variables values)))

(define (frame-pairs frame) (mcdr frame))
(define (frame-first-pair tgt-frame-pairs) (mcar tgt-frame-pairs))
(define (frame-rest-pairs tgt-frame-pairs) (mcdr tgt-frame-pairs))
(define (frame-pair-var tgt-pair) (mcar tgt-pair))
(define (frame-pair-val tgt-pair) (mcdr tgt-pair))
(define (change-frame-pair-val! tgt-pair val) (set-mcdr! tgt-pair val))
(define (add-binding-to-frame! var val frame)
  (set-mcdr! frame (mcons (mcons var val) (frame-pairs frame))))
(define (remove-binding-to-frame! prev-pair-struc rest-pairs)
  (set-mcdr! prev-pair-struc rest-pairs))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" 
                 vars 
                 vals)
          (error "Too few arguments supplied" 
                 vars 
                 vals))))


;                                                                                                  
;                                                                                                  
;                                                                                                  
;                                                                                                  
;                   ;;                                     ;;                                   ;; 
;                    ;                                      ;                                    ; 
;  ;; ;  ;   ;;;;    ; ;;;;  ;;;;           ;;  ;;  ;; ;;   ; ;;;    ;;;;   ;;  ;;  ;; ;;    ;;; ; 
;   ;; ;; ; ;    ;   ;  ;   ;    ;           ;   ;   ;;  ;  ;;   ;  ;    ;   ;   ;   ;;  ;  ;   ;; 
;   ;  ;  ;  ;;;;;   ;;;    ;;;;;;  ;;;;;;   ;   ;   ;   ;  ;    ;  ;    ;   ;   ;   ;   ;  ;    ; 
;   ;  ;  ; ;    ;   ; ;    ;                ;   ;   ;   ;  ;    ;  ;    ;   ;   ;   ;   ;  ;    ; 
;   ;  ;  ; ;   ;;   ;  ;   ;                ;  ;;   ;   ;  ;;   ;  ;    ;   ;  ;;   ;   ;  ;   ;; 
;  ;;; ;; ;  ;;; ;; ;;  ;;;  ;;;;;            ;; ;; ;;; ;;;;; ;;;    ;;;;     ;; ;; ;;; ;;;  ;;; ;;
;                                                                                                  
;                                                                                                  
;                                                                                                  
;                                                                                                  

;;make-unbound!'s specification
;;;;signature:(make-unbound! target-variable)
;;;;action:delete the binding of corresponding in first frame.
;;;;if not exitsts, do nothing else.

(define (unbound? exp)
  (tagged-list? exp 'make-unbound!))

(define (eval-unbound exp env)
  (unbound-variable! (cadr exp env)))

(define (unbound-variable! var env)
  (let ((frame (first-frame env)))
    (define (scan tgt-frame-pairs to-be-changed-cdr)
      (if (null? tgt-frame-pairs)
          'not-found
          (let ((first-pair (frame-first-pair tgt-frame-pairs)))
            (if (eq? var (frame-pair-var first-pair))
                (remove-binding-to-frame! to-be-changed-cdr (frame-rest-pairs tgt-frame-pairs))
                (scan (frame-rest-pairs tgt-frame-pairs) frame-first-pair)))))
    (scan (frame-pairs frame) frame)))



;;;test 

(define env0 (extend-environment '(var0-1 var0-2) '(4 8) the-empty-environment))
(define env1 (extend-environment '(var1-1 var1-2) '(1 2) env0))
env1

(unbound-variable! 'var0-1 env1)
(unbound-variable! 'var1-1 env1)
env1
(unbound-variable! 'var1-2 env1)
env1
(unbound-variable! 'some-var env1)