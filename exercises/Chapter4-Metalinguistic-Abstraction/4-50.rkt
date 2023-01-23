#lang racket
(require compatibility/mlist)

(define (mcadr ml) (mcar (mcdr ml)))

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
(define (ambeval exp env succeed fail)
  ((analyze exp) env succeed fail))

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
        ((amb? exp) (analyze-amb exp))
        ((ramb? exp) (analyze-ramb exp))
        ((application? exp) 
         (analyze-application exp))
        (else
         (error "Unknown expression 
                 type: ANALYZE" 
                exp))))

(define (analyze-self-evaluating exp)
  (lambda (env succeed fail)
    (succeed exp fail)))

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env succeed fail)
      (succeed qval fail))))


(define (analyze-variable exp)
  (lambda (env succeed fail)
    (succeed (lookup-variable-value exp env)
             fail)))

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze 
                (assignment-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)    ; *1*
               (let ((old-value
                      (lookup-variable-value 
                       var 
                       env)))
                 (set-variable-value!
                  var 
                  val 
                  env)
                 (succeed 
                  'ok
                  (lambda ()    ; *2*
                    (set-variable-value! 
                     var
                     old-value
                     env)
                    (fail2)))))
             fail))))

(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze 
                (definition-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)
               (define-variable! var val env)
               (succeed 'ok fail2))
             fail))))

(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env succeed fail)
      (pproc env
             ;; success continuation for evaluating
             ;; the predicate to obtain pred-value
             (lambda (pred-value fail2)
               (if (true? pred-value)
                   (cproc env succeed fail2)
                   (aproc env succeed fail2)))
             ;; failure continuation for
             ;; evaluating the predicate
             fail))))

(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence 
                (lambda-body exp))))
    (lambda (env succeed fail)
      (succeed (make-procedure vars bproc env)
               fail))))

(define (analyze-sequence exps)
  (define (sequentially a b)
    (lambda (env succeed fail)
      (a env
         ;; success continuation for calling a
         (lambda (a-value fail2)
           (b env succeed fail2))
         ;; failure continuation for calling a
         fail)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc 
                            (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence: ANALYZE")
        false)
    (loop (car procs) (cdr procs))))

(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env succeed fail)
      (fproc env
             (lambda (proc fail2)
               (get-args 
                aprocs
                env
                (lambda (args fail3)
                  (execute-application
                   proc args succeed fail3))
                fail2))
             fail))))

(define (get-args aprocs env succeed fail)
  (if (null? aprocs)
      (succeed '() fail)
      ((car aprocs) 
       env
       ;; success continuation for this aproc
       (lambda (arg fail2)
         (get-args 
          (cdr aprocs)
          env
          ;; success continuation for
          ;; recursive call to get-args
          (lambda (args fail3)
            (succeed (cons arg args)
                     fail3))
          fail2))
       fail)))

(define (execute-application 
         proc args succeed fail)
  (cond ((primitive-procedure? proc)
         (succeed 
          (apply-primitive-procedure 
           proc args)
          fail))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment 
           (procedure-parameters proc)
           args
           (procedure-environment proc))
          succeed
          fail))
        (else (error "Unknown procedure type: 
                      EXECUTE-APPLICATION"
                     proc))))

(define (analyze-amb exp)
  (let ((cprocs
         (map analyze (amb-choices exp))))
    (lambda (env succeed fail)
      (define (try-next choices)
        (if (null? choices)
            (fail)
            ((car choices) 
             env
             succeed
             (lambda ()
               (try-next (cdr choices))))))
      (try-next cprocs))))

(define (analyze-ramb exp)
  (let ((rand-cprocs
         (shuffle (map analyze (ramb-choices exp)))))
    (lambda (env succeed fail)
      (define (try-next choices)
        (if (null? choices)
            (fail)
            ((car choices) 
             env
             succeed
             (lambda ()
               (try-next (cdr choices))))))
      (try-next rand-cprocs))))

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

(define (amb? exp) (tagged-list? exp 'amb))
(define (amb-choices exp) (cdr exp))

(define (ramb? exp) (tagged-list? exp 'ramb))
(define ramb-choices amb-choices)

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
        (list 'list list)
        (list 'list-ref list-ref)
        (list 'append append)
        (list 'length length)
        (list 'null? null?)
        (list 'not not)
        (list 'eq? eq?)
        (list 'equal? equal?)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list '> >)
        (list '< <)
        (list '= =)
        (list 'random random)
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

(define input-prompt  ";;; Amb-Eval input:")
(define output-prompt ";;; Amb-Eval value:")

(define (driver-loop)
  (define (internal-loop try-again)
    (prompt-for-input input-prompt)
    (let ((input (read)))
      (cond ((eq? input 'try-again)
             (try-again))
            ((eq? input '___e) 'exit-loop)
            (else (begin
                    (newline)
                    (display 
                     ";;; Starting a new problem ")
                    (ambeval 
                     input
                     the-global-environment
                     ;; ambeval success
                     (lambda (val next-alternative)
                       (announce-output 
                        output-prompt)
                       (user-print val)
                       (internal-loop 
                        next-alternative))
                     ;; ambeval failure
                     (lambda ()
                       (announce-output
                        ";;; There are no 
                 more values of")
                       (user-print input)
                       (driver-loop))))))))
  (internal-loop
   (lambda ()
     (newline)
     (display 
      ";;; There is no current problem")
     (driver-loop))))

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

;;;setting
(define presets
  '(begin
     (define nil '())

     (define ($$require p)
       (if (not p) (amb)))

     (define nouns 
       '(noun student professor cat class))

     (define verbs 
       '(verb studies lectures eats sleeps))

     (define articles '(article the a))

     (define prepositions 
       '(prep for to in by with))

     (define (word-list-rdm tgtlist)
       (list-ref tgtlist (+ 1 (random (- (length tgtlist) 1)))))

     (define (gen-word word-list)
       ($$require (not (= *remaining* 0)))
       (set! *remaining* (- *remaining* 1))
       (list (car word-list) (word-list-rdm word-list)))

     (define (an-integer-starting-from n)
       (amb n (an-integer-starting-from (+ n 1))))

     (define *remaining* 0)
     (define (generate)
       (set! *remaining* (an-integer-starting-from 1))
       (let ((sent (gen-sentence)))
         ($$require (= *remaining* 0))
         sent))

     (define (gen-prepositional-phrase)
       (list 'prep-phrase
             (gen-word prepositions)
             (gen-noun-phrase)))

     (define (gen-sentence)
       (list 'sentence
             (gen-noun-phrase)
             (gen-verb-phrase)))

     (define (gen-verb-phrase)
       (define (maybe-extend verb-phrase)
         (ramb
          verb-phrase
          (maybe-extend 
           (list 'verb-phrase
                 verb-phrase
                 (gen-prepositional-phrase)))))
       (maybe-extend (gen-word verbs)))

     (define (gen-simple-noun-phrase)
       (list 'simple-noun-phrase
             (gen-word articles)
             (gen-word nouns)))

     (define (gen-noun-phrase)
       (define (maybe-extend noun-phrase)
         (ramb
          noun-phrase
          (maybe-extend 
           (list 'noun-phrase
                 noun-phrase
                 (gen-prepositional-phrase)))))
       (maybe-extend (gen-simple-noun-phrase)))
     
     )
  )

(ambeval presets the-global-environment (λ(val fail) 'succeeded) (λ() 'failed))

;;;repl

(driver-loop)