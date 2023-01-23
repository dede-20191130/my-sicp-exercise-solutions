#lang racket
(require compatibility/mlist)

;                                  
;                                  
;                                  
;                                  
;                             ;;   
;                              ;   
;    ;;;;  ;;;  ;;;  ;;;;      ;   
;   ;    ;  ;    ;  ;    ;     ;   
;   ;;;;;;   ;  ;    ;;;;;     ;   
;   ;        ;  ;   ;    ;     ;   
;   ;         ;;    ;   ;;     ;   
;    ;;;;;    ;;     ;;; ;;  ;;;;; 
;                                  
;                                  
;                                  
;                                  

(define apply-in-underlying-scheme apply)
(define (eval exp env) ((analyze exp) env))


;                                                                  
;                                                                  
;                                                                  
;                                                                  
;                             ;;                       ;           
;                              ;                                   
;    ;;;;   ;; ;;    ;;;;      ;    ;;; ;;;  ;;;;;   ;;;     ;;;;; 
;   ;    ;   ;;  ;  ;    ;     ;     ;   ;  ;    ;     ;    ;    ; 
;    ;;;;;   ;   ;   ;;;;;     ;     ;   ;   ;;;;      ;     ;;;;  
;   ;    ;   ;   ;  ;    ;     ;      ; ;        ;     ;         ; 
;   ;   ;;   ;   ;  ;   ;;     ;      ; ;   ;    ;     ;    ;    ; 
;    ;;; ;; ;;; ;;;  ;;; ;;  ;;;;;     ;    ;;;;;    ;;;;;  ;;;;;  
;                                      ;                           
;                                    ;;;                           
;                                                                  
;                                                                  


(define (analyze exp)
  (cond ((self-evaluating? exp)
         (analyze-self-evaluating exp))
        ((quoted? exp) 
         (analyze-quoted exp))
        ((variable? exp) 
         (analyze-variable exp))
        ((assignment? exp) 
         (analyze-assignment exp))
        ((definition? exp) 
         (analyze-definition exp))
        ((if? exp) 
         (analyze-if exp))
        ((lambda? exp) 
         (analyze-lambda exp))
        ((let? exp)
         (analyze (let->combination exp)))
        ((begin? exp) 
         (analyze-sequence 
          (begin-actions exp)))
        ((cond? exp) 
         (analyze (cond->if exp)))
        ((application? exp) 
         (analyze-application exp))
        (else
         (error "Unknown expression 
                 type: ANALYZE" 
                exp))))

(define (analyze-self-evaluating exp)
  (lambda (env) exp))

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env) qval)))

(define (analyze-variable exp)
  (lambda (env) 
    (lookup-variable-value exp env)))

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze 
                (assignment-value exp))))
    (lambda (env)
      (set-variable-value! 
       var (vproc env) env)
      'ok)))

(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze 
                (definition-value exp))))
    (lambda (env)
      (define-variable! var (vproc env) env)
      'ok)))

(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env)
      (if (true? (pproc env))
          (cproc env)
          (aproc env)))))

(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence 
                (lambda-body exp))))
    (lambda (env) 
      (make-procedure vars bproc env))))

(define (analyze-sequence exps)
  (define (sequentially proc1 proc2)
    (lambda (env) (proc1 env) (proc2 env)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc 
                            (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence: ANALYZE")
        'false)
    (loop (car procs) (cdr procs))))



(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env)
      (execute-application 
       (fproc env)
       (map (lambda (aproc) (aproc env))
            aprocs)))))

(define (execute-application proc args)
  (cond ((primitive-procedure? proc)
         (apply-primitive-procedure proc args))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment 
           (procedure-parameters proc)
           args
           (procedure-environment proc))))
        (else (error "Unknown procedure type: 
                      EXECUTE-APPLICATION"
                     proc))))




;                                                                                                                                                                                          
;                                                                                                                                                                                          
;                                                                                                                                                                                          
;                                                                                                                                                                                          
;                                                              ;                              ;;                                      ;;                                      ;;           
;                                                    ;                                         ;                                       ;                                       ;           
;    ;;; ;   ;;;;   ;; ;;  ;;;  ;;;  ;;;;   ;; ;;   ;;;;;    ;;;     ;;;;   ;; ;;    ;;;;      ;             ;;;;  ;;;  ;;;  ;;;;      ;             ;;;;   ;; ;;   ;; ;;      ;    ;;; ;;;
;   ;   ;;  ;    ;   ;;  ;  ;    ;  ;    ;   ;;  ;   ;         ;    ;    ;   ;;  ;  ;    ;     ;            ;    ;  ;    ;  ;    ;     ;            ;    ;   ;;  ;   ;;  ;     ;     ;   ; 
;   ;       ;    ;   ;   ;   ;  ;   ;;;;;;   ;   ;   ;         ;    ;    ;   ;   ;   ;;;;;     ;            ;;;;;;   ;  ;    ;;;;;     ;    ;;;;;;   ;;;;;   ;   ;   ;   ;     ;     ;   ; 
;   ;       ;    ;   ;   ;   ;  ;   ;        ;   ;   ;         ;    ;    ;   ;   ;  ;    ;     ;            ;        ;  ;   ;    ;     ;            ;    ;   ;   ;   ;   ;     ;      ; ;  
;   ;    ;  ;    ;   ;   ;    ;;    ;        ;   ;   ;   ;     ;    ;    ;   ;   ;  ;   ;;     ;            ;         ;;    ;   ;;     ;            ;   ;;   ;   ;   ;   ;     ;      ; ;  
;    ;;;;    ;;;;   ;;; ;;;   ;;     ;;;;;  ;;; ;;;   ;;;    ;;;;;   ;;;;   ;;; ;;;  ;;; ;;  ;;;;;           ;;;;;    ;;     ;;; ;;  ;;;;;           ;;; ;;  ;;;;    ;;;;    ;;;;;     ;   
;                                                                                                                                                            ;       ;                 ;   
;                                                                                                                                                           ;;;     ;;;              ;;;   
;                                                                                                                                                                                          
;                                                                                                                                                                                          



(define (eval-convnc exp env)
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
         (eval-convnc (cond->if exp) env))
        ((application? exp)
         (apply-convnc (eval-convnc (operator exp) env)
                (list-of-values 
                 (operands exp) 
                 env)))
        (else
         (error "Unknown expression 
                 type: EVAL" exp))))


(define (apply-convnc procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure 
          procedure 
          arguments))
        ((compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameters 
              procedure)
             arguments
             (procedure-environment 
              procedure))))
        (else
         (error "Unknown procedure 
                 type: APPLY" 
                procedure))))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval-convnc (first-operand exps) env)
            (list-of-values 
             (rest-operands exps) 
             env))))

(define (eval-if exp env)
  (if (true? (eval-convnc (if-predicate exp) env))
      (eval-convnc (if-consequent exp) env)
      (eval-convnc (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) 
         (eval-convnc (first-exp exps) env))
        (else 
         (eval-convnc (first-exp exps) env)
         (eval-sequence (rest-exps exps) 
                        env))))

(define (eval-assignment exp env)
  (set-variable-value! 
   (assignment-variable exp)
   (eval-convnc (assignment-value exp) env)
   env)
  'ok)

(define (eval-definition exp env)
  (define-variable! 
    (definition-variable exp)
    (eval-convnc (definition-value exp) env)
    env)
  'ok)

;                                                                                                                                          
;                                                                                                                                          
;                                                                                                                                          
;                                                                                                                                          
;                                                                                                       ;;                                 
;                            ;                                                                           ;                                 
;    ;;;;;  ;;; ;;; ;; ;;   ;;;;;    ;;;;   ;;  ;;          ;; ;;   ;; ;;;   ;;;;    ;;; ;   ;;;;    ;;; ;  ;;  ;;  ;; ;;;   ;;;;    ;;;;; 
;   ;    ;   ;   ;   ;;  ;   ;      ;    ;   ;  ;            ;;  ;   ;;     ;    ;  ;   ;;  ;    ;  ;   ;;   ;   ;   ;;     ;    ;  ;    ; 
;    ;;;;    ;   ;   ;   ;   ;       ;;;;;    ;;             ;   ;   ;      ;    ;  ;       ;;;;;;  ;    ;   ;   ;   ;      ;;;;;;   ;;;;  
;        ;    ; ;    ;   ;   ;      ;    ;    ;;             ;   ;   ;      ;    ;  ;       ;       ;    ;   ;   ;   ;      ;            ; 
;   ;    ;    ; ;    ;   ;   ;   ;  ;   ;;   ;  ;            ;   ;   ;      ;    ;  ;    ;  ;       ;   ;;   ;  ;;   ;      ;       ;    ; 
;   ;;;;;      ;    ;;; ;;;   ;;;    ;;; ;; ;;  ;;           ;;;;   ;;;;;    ;;;;    ;;;;    ;;;;;   ;;; ;;   ;; ;; ;;;;;    ;;;;;  ;;;;;  
;              ;                                             ;                                                                             
;            ;;;                                            ;;;                                                                            
;                                                                                                                                          
;                                                                                                                                          


(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (variable? exp) (symbol? exp))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp)
  (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp) 
  (cadr exp))

(define (assignment-value exp) (caddr exp))

(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda 
       (cdadr exp)   ; formal parameters
       (cddr exp)))) ; body

(define (lambda? exp) 
  (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate 
                 consequent 
                 alternative)
  (list 'if 
        predicate 
        consequent 
        alternative))

(define (begin? exp) 
  (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define (cond? exp) 
  (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) 
  (car clause))
(define (cond-actions clause) 
  (cdr clause))
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))
(define (expand-clauses clauses)
  (if (null? clauses)
      'false     ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp 
                 (cond-actions first))
                (error "ELSE clause isn't 
                        last: COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp 
                      (cond-actions first))
                     (expand-clauses 
                      rest))))))


(define (let? exp)
  (tagged-list? exp 'let))
(define (named-let? exp)
  (and (let? exp) (symbol? (cadr exp))))
(define (named-let-name exp)
  (cadr exp))
(define (let-var-declarations exp)
  (if (named-let? exp)
      (caddr exp)
      (cadr exp)))
(define (let-body exp)
  (if (named-let? exp)
      (cdddr exp)
      (cddr exp)))


(define (let->combination exp)
  (let ((var-declarations (let-var-declarations exp)))
    (let ((params (map (λ(dec) (car dec)) var-declarations))
          (exps-for-params (map (λ(dec) (cadr dec)) var-declarations)))
      (if (named-let? exp)
          (make-begin
           (list
            (list
             'define
             (named-let-name exp)
             (make-lambda params
                          (let-body exp)))
            (cons (named-let-name exp) exps-for-params)))          
          (cons (make-lambda params
                             (let-body exp))
                exps-for-params)))))
  
;                                                                                                                          
;                                                                                                                          
;                                                                                                                          
;                                                                                                                          
;       ;;                                                                                                                 
;        ;           ;                               ;                               ;                                     
;    ;;; ;   ;;;;   ;;;;;    ;;;;            ;;;;;  ;;;;;   ;; ;;;  ;;  ;;   ;;; ;  ;;;;;   ;;  ;;  ;; ;;;   ;;;;    ;;;;; 
;   ;   ;;  ;    ;   ;      ;    ;          ;    ;   ;       ;;      ;   ;  ;   ;;   ;       ;   ;   ;;     ;    ;  ;    ; 
;   ;    ;   ;;;;;   ;       ;;;;;           ;;;;    ;       ;       ;   ;  ;        ;       ;   ;   ;      ;;;;;;   ;;;;  
;   ;    ;  ;    ;   ;      ;    ;               ;   ;       ;       ;   ;  ;        ;       ;   ;   ;      ;            ; 
;   ;   ;;  ;   ;;   ;   ;  ;   ;;          ;    ;   ;   ;   ;       ;  ;;  ;    ;   ;   ;   ;  ;;   ;      ;       ;    ; 
;    ;;; ;;  ;;; ;;   ;;;    ;;; ;;         ;;;;;     ;;;   ;;;;;     ;; ;;  ;;;;     ;;;     ;; ;; ;;;;;    ;;;;;  ;;;;;  
;                                                                                                                          
;                                                                                                                          
;                                                                                                                          
;                                                                                                                          



(define (true? x)
  (not (eq? x false)))

(define (false? x)
  (eq? x false))


(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-frame variables values)
  (mcons variables values))
(define (frame-variables frame) (mcar frame))
(define (frame-values frame) (mcdr frame))
(define (add-binding-to-frame! var val frame)
  (set-mcar! frame (mcons var (mcar frame)))
  (set-mcdr! frame (mcons val (mcdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame (list->mlist vars) (list->mlist vals)) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" 
                 vars 
                 vals)
          (error "Too few arguments supplied" 
                 vars 
                 vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop 
              (enclosing-environment env)))
            ((eq? var (mcar vars))
             (mcar vals))
            (else (scan (mcdr vars) 
                        (mcdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop 
              (enclosing-environment env)))
            ((eq? var (mcar vars))
             (set-mcar! vals val))
            (else (scan (mcdr vars) 
                        (mcdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable: SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! 
              var val frame))
            ((eq? var (mcar vars))
             (set-mcar! vals val))
            (else (scan (mcdr vars) 
                        (mcdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))


;                                                                                                                                                                                                                          
;                                                                                                                                                                                                                          
;                                                                                                                                                                                                                          
;                                                                                                                                                                                                                          
;                                              ;                                                                                                                                        ;;                                 
;                                    ;                                                                                       ;                                                           ;                                 
;   ;; ;;;  ;;  ;;  ;; ;;           ;;;;;    ;;;   ;; ;  ;   ;;;;            ;;;;;  ;;  ;;  ;; ;;   ;; ;;    ;;;;   ;; ;;;  ;;;;;           ;; ;;   ;; ;;;   ;;;;    ;;; ;   ;;;;    ;;; ;  ;;  ;;  ;; ;;;   ;;;;    ;;;;; 
;    ;;      ;   ;   ;;  ;           ;         ;    ;; ;; ; ;    ;          ;    ;   ;   ;   ;;  ;   ;;  ;  ;    ;   ;;      ;               ;;  ;   ;;     ;    ;  ;   ;;  ;    ;  ;   ;;   ;   ;   ;;     ;    ;  ;    ; 
;    ;       ;   ;   ;   ;  ;;;;;;   ;         ;    ;  ;  ; ;;;;;;           ;;;;    ;   ;   ;   ;   ;   ;  ;    ;   ;       ;               ;   ;   ;      ;    ;  ;       ;;;;;;  ;    ;   ;   ;   ;      ;;;;;;   ;;;;  
;    ;       ;   ;   ;   ;           ;         ;    ;  ;  ; ;                    ;   ;   ;   ;   ;   ;   ;  ;    ;   ;       ;               ;   ;   ;      ;    ;  ;       ;       ;    ;   ;   ;   ;      ;            ; 
;    ;       ;  ;;   ;   ;           ;   ;     ;    ;  ;  ; ;               ;    ;   ;  ;;   ;   ;   ;   ;  ;    ;   ;       ;   ;           ;   ;   ;      ;    ;  ;    ;  ;       ;   ;;   ;  ;;   ;      ;       ;    ; 
;   ;;;;;     ;; ;; ;;; ;;;           ;;;    ;;;;; ;;; ;; ;  ;;;;;          ;;;;;     ;; ;;  ;;;;    ;;;;    ;;;;   ;;;;;     ;;;            ;;;;   ;;;;;    ;;;;    ;;;;    ;;;;;   ;;; ;;   ;; ;; ;;;;;    ;;;;;  ;;;;;  
;                                                                                            ;       ;                                       ;                                                                             
;                                                                                           ;;;     ;;;                                     ;;;                                                                            
;                                                                                                                                                                                                                          
;                                                                                                                                                                                                                          


(define (setup-environment)
  (let ((initial-env
         (extend-environment 
          (primitive-procedure-names)
          (primitive-procedure-objects)
          the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))



(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) 
  (cadr proc))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list '> >)
        (list '< <)
        (list '= =)
        (list 'append append)
        ))

(define (primitive-procedure-names)
  (map car primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) 
         (list 'primitive (cadr proc)))
       primitive-procedures))

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))

(define input-prompt  ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output 
           (eval input 
                 the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(define (prompt-for-input string)
  (newline) (newline) 
  (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display 
       (list 'compound-procedure
             (procedure-parameters object)
             (procedure-body object)
             '<procedure-env>))
      (display object)))


;;;initialization

(define the-global-environment 
  (setup-environment))

(define the-second-global-environment 
  (setup-environment))



;;;time measuring

(define quoted-factorial '((lambda ()
                            (define (factorial n)
                              (if (= n 1) 
                                  1 
                                  (* n (factorial (- n 1)))))
                            (factorial 10))))

(eval quoted-factorial the-global-environment)
(eval quoted-factorial the-second-global-environment)

(time
 (for ((i (in-range 0 100000)))
   (eval quoted-factorial the-global-environment)))
      

(time
 (for ((i (in-range 0 100000)))
   (eval-convnc quoted-factorial the-second-global-environment)))
      

