#lang racket
(require compatibility/mlist)
(require racket/exn)

(define UNASSIGNED-SIGN '*unassigned*)

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (make-let decs body)
  (cons 'let (cons decs body)))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (definition? exp)
  (tagged-list? exp 'define))

;;;scan-out-defines should be installed in make-procedure
;;;because it runs once procedure is constructed, while when it is installed in procedure-body, it runs each time it's called and the performance gets worse.
(define (make-procedure parameters body env)
  (list
   'procedure
   parameters
   (scan-out-defines body)
   env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

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
;     ;;                    ;;                                                                          ;;                         
;      ;                     ;                                                                           ;                         
;      ;     ;;;;    ;;;;    ; ;;;; ;;  ;;  ;; ;;           ;; ;;   ;; ;;;   ;;;;    ;;; ;   ;;;;    ;;; ;  ;;  ;;  ;; ;;;   ;;;;  
;      ;    ;    ;  ;    ;   ;  ;    ;   ;   ;;  ;           ;;  ;   ;;     ;    ;  ;   ;;  ;    ;  ;   ;;   ;   ;   ;;     ;    ; 
;      ;    ;    ;  ;    ;   ;;;     ;   ;   ;   ;           ;   ;   ;      ;    ;  ;       ;;;;;;  ;    ;   ;   ;   ;      ;;;;;; 
;      ;    ;    ;  ;    ;   ; ;     ;   ;   ;   ;           ;   ;   ;      ;    ;  ;       ;       ;    ;   ;   ;   ;      ;      
;      ;    ;    ;  ;    ;   ;  ;    ;  ;;   ;   ;           ;   ;   ;      ;    ;  ;    ;  ;       ;   ;;   ;  ;;   ;      ;      
;    ;;;;;   ;;;;    ;;;;   ;;  ;;;   ;; ;;  ;;;;            ;;;;   ;;;;;    ;;;;    ;;;;    ;;;;;   ;;; ;;   ;; ;; ;;;;;    ;;;;; 
;                                            ;               ;                                                                     
;                                           ;;;             ;;;                                                                    
;                                                                                                                                  
;                                                                                                                                  


(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan tgt-frame-pairs)
      (if (null? tgt-frame-pairs)
          (env-loop (enclosing-environment env))
          (let ((first-pair (frame-first-pair tgt-frame-pairs)))
            (if (eq? var (frame-pair-var first-pair))
                (begin
                  (let ((pulled-val (frame-pair-val first-pair)))
                    (if (eq? pulled-val UNASSIGNED-SIGN)
                        (error "SPECIFIED VARIABLE'S VALUE IS NOT YET ASSIGNED." var)
                        pulled-val)))
                (scan (frame-rest-pairs tgt-frame-pairs))))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (scan (frame-pairs (first-frame env)))))
  (env-loop env))


(define (sort-partitioning pred? lst)
  (call-with-values (lambda () (partition pred? lst)) (lambda(top bottom) (list top bottom))))


;                                                                                                                                  
;                                                                                                                                  
;                                                                                                                                  
;                                                                                                                                  
;                                                                               ;;             ;;;     ;                           
;                                                            ;                   ;            ;                                    
;    ;;;;;   ;;; ;   ;;;;   ;; ;;            ;;;;   ;;  ;;  ;;;;;            ;;; ;   ;;;;   ;;;;;;   ;;;    ;; ;;    ;;;;    ;;;;; 
;   ;    ;  ;   ;;  ;    ;   ;;  ;          ;    ;   ;   ;   ;              ;   ;;  ;    ;    ;        ;     ;;  ;  ;    ;  ;    ; 
;    ;;;;   ;        ;;;;;   ;   ;  ;;;;;;  ;    ;   ;   ;   ;      ;;;;;;  ;    ;  ;;;;;;    ;        ;     ;   ;  ;;;;;;   ;;;;  
;        ;  ;       ;    ;   ;   ;          ;    ;   ;   ;   ;              ;    ;  ;         ;        ;     ;   ;  ;            ; 
;   ;    ;  ;    ;  ;   ;;   ;   ;          ;    ;   ;  ;;   ;   ;          ;   ;;  ;         ;        ;     ;   ;  ;       ;    ; 
;   ;;;;;    ;;;;    ;;; ;; ;;; ;;;          ;;;;     ;; ;;   ;;;            ;;; ;;  ;;;;;  ;;;;;;   ;;;;;  ;;; ;;;  ;;;;;  ;;;;;  
;                                                                                                                                  
;                                                                                                                                  
;                                                                                                                                  
;                                                                                                                                  



(define (scan-out-defines body)
  (let ((partitioned-body (sort-partitioning definition? body)))
    (let ((def-exps (car partitioned-body)))
      (if (null? def-exps)
          body
          (let ((other-exps (cadr partitioned-body)))
            (list (make-let (map (λ(def-exp) (list (cadr def-exp) (quote '*unassigned*))) def-exps)
                            (append
                             (map (λ(def-exp) (list 'set! (cadr def-exp) (caddr def-exp))) def-exps)
                             other-exps))))))))

;;;test 

(define my-eval
  (let ((ns (make-base-namespace)))
    (lambda (expr) (eval expr ns))))

(define env0 (extend-environment '(var0-1 var0-2 var-not-assigned) (append '(4 8) (list UNASSIGNED-SIGN)) the-empty-environment))
(define env1 (extend-environment '(var1-1 var1-2) '(1 2) env0))

(lookup-variable-value 'var1-2 env1)
(with-handlers ([exn:fail?
                 (λ (e) (display "got a error: ")(display (exn->string e)))])
  (lookup-variable-value 'var-not-assigned env1))


(define evaled-proc1 (my-eval (make-lambda '(x y)
                                           (scan-out-defines
                                            '((define inner-x 2)
                                              (println (+ x inner-x))
                                              (println (+ y inner-y))
                                              (println (+ inner-x inner-y inner-z))
                                              (define inner-y 4)
                                              (define inner-z 8))))))
(evaled-proc1 16 32)