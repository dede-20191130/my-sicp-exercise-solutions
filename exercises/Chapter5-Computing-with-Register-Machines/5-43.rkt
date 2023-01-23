#lang racket
(require compatibility/mlist)


;                                                                                                                                          
;                                                                                                                                          
;                                                                                                                                          
;                                                                                                                                          
;    ;;; ;  ;;; ;;;;;;  ;;; ;;;;;;;   ;;    ;;; ;;;         ;;;;;   ;;;;;     ;;;     ;;;;  ;;;;;;  ;;;;    ;;; ;;; ;;;;;   ;;;;;;   ;;; ; 
;   ;   ;;   ;   ;  ;;   ;  ;  ;  ;    ;     ;   ;           ;   ;   ;   ;   ;   ;   ;   ;   ;   ;   ;  ;    ;   ;   ;   ;   ;   ;  ;   ;; 
;   ;         ; ;   ; ;  ;     ;      ; ;     ; ;            ;   ;   ;   ;  ;     ; ;        ; ;     ;   ;   ;   ;   ;   ;   ; ;    ;      
;    ;;;;     ; ;   ; ;  ;     ;      ; ;      ;             ;   ;   ;   ;  ;     ; ;        ;;;     ;   ;   ;   ;   ;   ;   ;;;     ;;;;  
;        ;     ;    ;  ; ;     ;      ; ;      ;             ;;;;    ;;;;   ;     ; ;        ; ;     ;   ;   ;   ;   ;;;;    ; ;         ; 
;        ;     ;    ;  ; ;     ;      ;;;     ; ;            ;       ;  ;   ;     ; ;        ;       ;   ;   ;   ;   ;  ;    ;           ; 
;   ;;   ;     ;    ;   ;;     ;     ;   ;   ;   ;           ;       ;   ;   ;   ;   ;   ;   ;   ;   ;  ;    ;   ;   ;   ;   ;   ;  ;;   ; 
;   ; ;;;     ;;;  ;;;  ;;    ;;;   ;;; ;;; ;;; ;;;         ;;;     ;;;   ;   ;;;     ;;;   ;;;;;;  ;;;;      ;;;   ;;;   ; ;;;;;;  ; ;;;  
;                                                                                                                                          
;                                                                                                                                          
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

(define (tagged-mlist? exp tag)
  (if (mpair? exp)
      (eq? (mcar exp) tag)
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

(define (make-let decs body)
  (cons 'let (cons decs body)))

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

(define (last-operand? ops) (null? (cdr ops)))

(define (sort-partitioning pred? lst)
  (call-with-values (lambda () (partition pred? lst)) (lambda(top bottom) (list top bottom))))

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

;                                                                                                                          
;                                                                                                                          
;                                                                                                                          
;                                                                                                                          
;   ;;;;      ;;    ;;;;;;;   ;;             ;;; ;  ;;;;;;; ;;;;;   ;;; ;;;   ;;;;  ;;;;;;; ;;; ;;; ;;;;;   ;;;;;;   ;;; ; 
;    ;  ;      ;    ;  ;  ;    ;            ;   ;;  ;  ;  ;  ;   ;   ;   ;   ;   ;  ;  ;  ;  ;   ;   ;   ;   ;   ;  ;   ;; 
;    ;   ;    ; ;      ;      ; ;           ;          ;     ;   ;   ;   ;  ;          ;     ;   ;   ;   ;   ; ;    ;      
;    ;   ;    ; ;      ;      ; ;            ;;;;      ;     ;   ;   ;   ;  ;          ;     ;   ;   ;   ;   ;;;     ;;;;  
;    ;   ;    ; ;      ;      ; ;                ;     ;     ;;;;    ;   ;  ;          ;     ;   ;   ;;;;    ; ;         ; 
;    ;   ;    ;;;      ;      ;;;                ;     ;     ;  ;    ;   ;  ;          ;     ;   ;   ;  ;    ;           ; 
;    ;  ;    ;   ;     ;     ;   ;          ;;   ;     ;     ;   ;   ;   ;   ;   ;     ;     ;   ;   ;   ;   ;   ;  ;;   ; 
;   ;;;;    ;;; ;;;   ;;;   ;;; ;;;         ; ;;;     ;;;   ;;;   ;   ;;;     ;;;     ;;;     ;;;   ;;;   ; ;;;;;;  ; ;;;  
;                                                                                                                          
;                                                                                                                          
;                                                                                                                          
;                                                                                                                          



(define (true? x)
  (not (eq? x false)))

(define (false? x)
  (eq? x false))


;(define (make-procedure parameters body env)
;  (list 'procedure parameters body env))
;(define (compound-procedure? p)
;  (tagged-list? p 'procedure))
;(define (procedure-parameters p) (cadr p))
;(define (procedure-body p) (caddr p))
;(define (procedure-environment p) (cadddr p))

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

(define (empty-arglist) '())
(define (adjoin-arg arg arglist)
  (append arglist (list arg)))


(define (frame-count env) (length env))
(define (var-count frm-vars-or-vals) (mlength frm-vars-or-vals))
(define (frame-number lexical-address) (car lexical-address))
(define (displacement-number lexical-address) (cadr lexical-address))
(define (dig-into-frames rt-env frm-num)   
  (if (> frm-num (frame-count rt-env))
      (error "Uncorrect lexical infomation of frame number:" frm-num)
      (list-ref rt-env frm-num)))
(define (dig-into-var-vals frme-vals dsp-num)   
  (if (> dsp-num (var-count frme-vals))
      (error "Uncorrect lexical infomationof displacement number:" dsp-num)
      (mlist-ref frme-vals dsp-num)))
(define (drop-var-vals frme-vals dsp-num)   
  (if (> dsp-num (var-count frme-vals))
      (error "Uncorrect lexical infomationof displacement number:" dsp-num)
      (mlist-tail frme-vals dsp-num)))
(define UNASSIGNED-SIGN '*unassigned*)

(define (lexical-address-lookup lxc-addr rt-env)
  (let ((tgt-frame (dig-into-frames rt-env (frame-number lxc-addr))))
    (let ((tgt-val (dig-into-var-vals (frame-values tgt-frame) (displacement-number lxc-addr))))
      (if (eq? tgt-val UNASSIGNED-SIGN)
          (error "Unassigned value :" (mlist-ref (frame-variables tgt-frame) (displacement-number lxc-addr)))
          tgt-val))))

(define (lexical-address-set! lxc-addr val rt-env)
  (let ((tgt-frame (dig-into-frames rt-env (frame-number lxc-addr))))
    (let ((dropped-vals (drop-var-vals (frame-values tgt-frame) (displacement-number lxc-addr))))
      (set-mcar! dropped-vals val))))

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



;                                                                                                                                                                                                          
;                                                                                                                                                                                                          
;                                                                                                                                                                                                          
;                                                                                                                                                                                                          
;    ;;; ;  ;;;;;;; ;;;;;   ;;; ;;;   ;;;;  ;;;;;;; ;;; ;;; ;;;;;   ;;;;;;            ;;;   ;;;;;;          ;;;;;;; ;;; ;;; ;;;;;;            ;;;;    ;;;   ;;; ;;; ;;;;;    ;;;;;  ;;;     ;;;;;;  ;;;;;  
;   ;   ;;  ;  ;  ;  ;   ;   ;   ;   ;   ;  ;  ;  ;  ;   ;   ;   ;   ;   ;           ;   ;   ;   ;          ;  ;  ;  ;   ;   ;   ;           ;   ;   ;   ;   ;; ;;   ;   ;     ;     ;       ;   ;   ;   ; 
;   ;          ;     ;   ;   ;   ;  ;          ;     ;   ;   ;   ;   ; ;            ;     ;  ; ;               ;     ;   ;   ; ;            ;       ;     ;  ;; ;;   ;   ;     ;     ;       ; ;     ;   ; 
;    ;;;;      ;     ;   ;   ;   ;  ;          ;     ;   ;   ;   ;   ;;;            ;     ;  ;;;               ;     ;;;;;   ;;;            ;       ;     ;  ; ; ;   ;   ;     ;     ;       ;;;     ;   ; 
;        ;     ;     ;;;;    ;   ;  ;          ;     ;   ;   ;;;;    ; ;            ;     ;  ; ;               ;     ;   ;   ; ;            ;       ;     ;  ; ; ;   ;;;;      ;     ;       ; ;     ;;;;  
;        ;     ;     ;  ;    ;   ;  ;          ;     ;   ;   ;  ;    ;              ;     ;  ;                 ;     ;   ;   ;              ;       ;     ;  ;   ;   ;         ;     ;   ;   ;       ;  ;  
;   ;;   ;     ;     ;   ;   ;   ;   ;   ;     ;     ;   ;   ;   ;   ;   ;           ;   ;   ;                 ;     ;   ;   ;   ;           ;   ;   ;   ;   ;   ;   ;         ;     ;   ;   ;   ;   ;   ; 
;   ; ;;;     ;;;   ;;;   ;   ;;;     ;;;     ;;;     ;;;   ;;;   ; ;;;;;;            ;;;   ;;;               ;;;   ;;; ;;; ;;;;;;            ;;;     ;;;   ;;; ;;; ;;;      ;;;;;  ;;;;;;  ;;;;;;  ;;;   ;
;                                                                                                                                                                                                          
;                                                                                                                                                                                                          
;                                                                                                                                                                                                          
;                                                                                                                                                                                                          

(define (compile exp target linkage cenv)
  (cond ((self-evaluating? exp)
         (compile-self-evaluating 
          exp target linkage))
        ((quoted? exp) 
         (compile-quoted exp target linkage))
        ((variable? exp)
         (compile-variable 
          exp target linkage cenv))
        ((assignment? exp)
         (compile-assignment
          exp target linkage cenv))
        ((definition? exp)
         (compile-definition
          exp target linkage cenv))
        ((if? exp)
         (compile-if exp target linkage cenv))
        ((lambda? exp)
         (compile-lambda exp target linkage cenv))
        ((begin? exp)
         (compile-sequence 
          (begin-actions exp) target linkage cenv))
        ((cond? exp) 
         (compile 
          (cond->if exp) target linkage cenv))
        ((let? exp) 
         (compile 
          (let->combination exp) target linkage cenv))
        ((application? exp)
         (compile-application 
          exp target linkage cenv))
        (else
         (error "Unknown expression type: 
                 COMPILE" 
                exp))))

(define (make-instruction-sequence 
         needs modifies statements)
  (list needs modifies statements))

(define (empty-instruction-sequence)
  (make-instruction-sequence '() '() '()))



;                                                                                                                                                                          
;                                                                                                                                                                          
;                                                                                                                                                                          
;                                                                                                                                                                          
;     ;;;;    ;;;   ;;; ;;; ;;;;;    ;;;;;  ;;;      ;;;;; ;;;  ;;;   ;;;;          ;;;;;;  ;;; ;;; ;;;;;   ;;;;;   ;;;;;;   ;;; ;   ;;; ;   ;;;;;    ;;;  ;;;  ;;;  ;;; ; 
;    ;   ;   ;   ;   ;; ;;   ;   ;     ;     ;         ;    ;;   ;   ;   ;           ;   ;   ;   ;   ;   ;   ;   ;   ;   ;  ;   ;;  ;   ;;     ;     ;   ;  ;;   ;  ;   ;; 
;   ;       ;     ;  ;; ;;   ;   ;     ;     ;         ;    ; ;  ;  ;                ; ;      ; ;    ;   ;   ;   ;   ; ;    ;       ;          ;    ;     ; ; ;  ;  ;      
;   ;       ;     ;  ; ; ;   ;   ;     ;     ;         ;    ; ;  ;  ;                ;;;       ;     ;   ;   ;   ;   ;;;     ;;;;    ;;;;      ;    ;     ; ; ;  ;   ;;;;  
;   ;       ;     ;  ; ; ;   ;;;;      ;     ;         ;    ;  ; ;  ;   ;;;          ; ;       ;     ;;;;    ;;;;    ; ;         ;       ;     ;    ;     ; ;  ; ;       ; 
;   ;       ;     ;  ;   ;   ;         ;     ;   ;     ;    ;  ; ;  ;    ;           ;        ; ;    ;       ;  ;    ;           ;       ;     ;    ;     ; ;  ; ;       ; 
;    ;   ;   ;   ;   ;   ;   ;         ;     ;   ;     ;    ;   ;;   ;   ;           ;   ;   ;   ;   ;       ;   ;   ;   ;  ;;   ;  ;;   ;     ;     ;   ;  ;   ;;  ;;   ; 
;     ;;;     ;;;   ;;; ;;; ;;;      ;;;;;  ;;;;;;   ;;;;; ;;;  ;;    ;;;           ;;;;;;  ;;; ;;; ;;;     ;;;   ; ;;;;;;  ; ;;;   ; ;;;    ;;;;;    ;;;  ;;;  ;;  ; ;;;  
;                                                                                                                                                                          
;                                                                                                                                                                          
;                                                                                                                                                                          
;                                                                                                                                                                          



(define (compile-linkage linkage)
  (cond ((eq? linkage 'return)
         (make-instruction-sequence 
          '(continue)
          '()
          '((goto (reg continue)))))
        ((eq? linkage 'next)
         (empty-instruction-sequence))
        (else
         (make-instruction-sequence '() '()
                                    `((goto (label ,linkage)))))))

(define (end-with-linkage 
         linkage instruction-sequence)
  (preserving '(continue)
              instruction-sequence
              (compile-linkage linkage)))



(define (compile-self-evaluating 
         exp target linkage)
  (end-with-linkage
   linkage (make-instruction-sequence 
            '()
            (list target)
            `((assign ,target (const ,exp))))))

(define (compile-quoted exp target linkage)
  (end-with-linkage
   linkage
   (make-instruction-sequence
    '()
    (list target)
    `((assign 
       ,target
       (const ,(text-of-quotation exp)))))))

(define (compile-variable
         exp target linkage cenv)
  (let ((lxc-addr (find-variable exp cenv)))
    (end-with-linkage 
     linkage
     (make-instruction-sequence 
      '(env)
      (list target)
      (if (eq? lxc-addr NOT-FOUND)
          `((assign ,target
                    (op get-global-environment))
            (assign ,target
                    (op lookup-variable-value)
                    (const ,exp)
                    (reg ,target)))
          `((assign ,target
                    (op lexical-address-lookup)
                    (const ,lxc-addr)
                    (reg env))))))))

(define (compile-assignment 
         exp target linkage cenv)
  (let ((var (assignment-variable exp))
        (get-value-code
         (compile (assignment-value exp) 
                  'val
                  'next
                  cenv)))
    (let ((lxc-addr (find-variable var cenv)))
      (end-with-linkage 
       linkage
       (preserving 
        '(env)
        get-value-code
        (make-instruction-sequence
         '(env val)
         (list target)
         (if (eq? lxc-addr NOT-FOUND)
             `((assign ,target
                       (op get-global-environment))
               (perform (op set-variable-value!)
                        (const ,var)
                        (reg val)
                        (reg ,target))
               (assign ,target (const ok)))
             `((perform (op lexical-address-set!)
                        (const ,lxc-addr)
                        (reg val)
                        (reg env))
               (assign ,target (const ok))))))))))

(define (compile-definition 
         exp target linkage cenv)
  (let ((var (definition-variable exp))
        (get-value-code
         (compile (definition-value exp)
                  'val
                  'next
                  cenv)))
    (end-with-linkage
     linkage
     (preserving 
      '(env)
      get-value-code
      (make-instruction-sequence
       '(env val)
       (list target)
       `((perform (op define-variable!)
                  (const ,var)
                  (reg val)
                  (reg env))
         (assign ,target (const ok))))))))


(define (compile-if exp target linkage cenv)
  (let ((t-branch (make-label 'true-branch))
        (f-branch (make-label 'false-branch))
        (after-if (make-label 'after-if)))
    (let ((consequent-linkage
           (if (eq? linkage 'next) 
               after-if
               linkage)))
      (let ((p-code 
             (compile (if-predicate exp)
                      'val
                      'next
                      cenv))
            (c-code
             (compile (if-consequent exp) 
                      target 
                      consequent-linkage
                      cenv))
            (a-code
             (compile (if-alternative exp)
                      target
                      linkage
                      cenv)))
        (preserving 
         '(env continue)
         p-code
         (append-instruction-sequences
          (make-instruction-sequence 
           '(val) 
           '()
           `((test (op false?) (reg val))
             (branch (label ,f-branch))))
          (parallel-instruction-sequences
           (append-instruction-sequences 
            t-branch c-code)
           (append-instruction-sequences
            f-branch a-code))
          after-if))))))



(define (compile-sequence seq target linkage cenv)
  (if (last-exp? seq)
      (compile (first-exp seq) target linkage cenv)
      (preserving '(env continue)
                  (compile (first-exp seq) target 'next cenv)
                  (compile-sequence (rest-exps seq)
                                    target
                                    linkage
                                    cenv))))


(define (compile-lambda exp target linkage cenv)
  (let ((proc-entry 
         (make-label 'entry))
        (after-lambda 
         (make-label 'after-lambda)))
    (let ((lambda-linkage
           (if (eq? linkage 'next)
               after-lambda
               linkage)))
      (append-instruction-sequences
       (tack-on-instruction-sequence
        (end-with-linkage 
         lambda-linkage
         (make-instruction-sequence 
          '(env)
          (list target)
          `((assign 
             ,target
             (op make-compiled-procedure)
             (label ,proc-entry)
             (reg env)))))
        (compile-lambda-body exp proc-entry cenv))
       after-lambda))))


(define (compile-lambda-body exp proc-entry cenv)
  (let ((formals (lambda-parameters exp)))
    (let ((extended-cenv (cons (list->mlist formals) cenv)))
      (append-instruction-sequences
       (make-instruction-sequence 
        '(env proc argl)
        '(env)
        `(,proc-entry
          (assign env 
                  (op compiled-procedure-env)
                  (reg proc))
          (assign env
                  (op extend-environment)
                  (const ,formals)
                  (reg argl)
                  (reg env))))
       (compile-sequence (scan-out-defines (lambda-body exp))
                         'val
                         'return
                         extended-cenv)))))



;                                                                                                                                                                                  
;                                                                                                                                                                                  
;                                                                                                                                                                                  
;                                                                                                                                                                                  
;     ;;;;    ;;;   ;;; ;;; ;;;;;    ;;;;;  ;;;      ;;;;; ;;;  ;;;   ;;;;            ;;;;    ;;;   ;;; ;;; ;;;;;    ;;;;; ;;;  ;;;   ;;    ;;;;;;;  ;;;;;    ;;;  ;;;  ;;;  ;;; ; 
;    ;   ;   ;   ;   ;; ;;   ;   ;     ;     ;         ;    ;;   ;   ;   ;           ;   ;   ;   ;   ;; ;;   ;   ;     ;    ;;   ;     ;    ;  ;  ;    ;     ;   ;  ;;   ;  ;   ;; 
;   ;       ;     ;  ;; ;;   ;   ;     ;     ;         ;    ; ;  ;  ;               ;       ;     ;  ;; ;;   ;   ;     ;    ; ;  ;    ; ;      ;       ;    ;     ; ; ;  ;  ;      
;   ;       ;     ;  ; ; ;   ;   ;     ;     ;         ;    ; ;  ;  ;               ;       ;     ;  ; ; ;   ;;;;      ;    ; ;  ;    ; ;      ;       ;    ;     ; ; ;  ;   ;;;;  
;   ;       ;     ;  ; ; ;   ;;;;      ;     ;         ;    ;  ; ;  ;   ;;;         ;       ;     ;  ; ; ;   ;   ;     ;    ;  ; ;    ; ;      ;       ;    ;     ; ;  ; ;       ; 
;   ;       ;     ;  ;   ;   ;         ;     ;   ;     ;    ;  ; ;  ;    ;          ;       ;     ;  ;   ;   ;   ;     ;    ;  ; ;    ;;;      ;       ;    ;     ; ;  ; ;       ; 
;    ;   ;   ;   ;   ;   ;   ;         ;     ;   ;     ;    ;   ;;   ;   ;           ;   ;   ;   ;   ;   ;   ;   ;     ;    ;   ;;   ;   ;     ;       ;     ;   ;  ;   ;;  ;;   ; 
;     ;;;     ;;;   ;;; ;;; ;;;      ;;;;;  ;;;;;;   ;;;;; ;;;  ;;    ;;;             ;;;     ;;;   ;;; ;;; ;;;;;    ;;;;; ;;;  ;;  ;;; ;;;   ;;;    ;;;;;    ;;;  ;;;  ;;  ; ;;;  
;                                                                                                                                                                                  
;                                                                                                                                                                                  
;                                                                                                                                                                                  
;                                                                                                                                                                                  


(define (compile-application 
         exp target linkage cenv)
  (let ((proc-code 
         (compile (operator exp) 'proc 'next cenv))
        (operand-codes
         (map (lambda (operand)
                (compile operand 'val 'next cenv))
              (operands exp))))
    (preserving 
     '(env continue)
     proc-code
     (preserving 
      '(proc continue)
      (construct-arglist operand-codes)
      (compile-procedure-call 
       target
       linkage)))))


(define (construct-arglist operand-codes)
  (let ((operand-codes 
         (reverse operand-codes)))
    (if (null? operand-codes)
        (make-instruction-sequence 
         '() 
         '(argl)
         '((assign argl (const ()))))
        (let ((code-to-get-last-arg
               (append-instruction-sequences
                (car operand-codes)
                (make-instruction-sequence 
                 '(val)
                 '(argl)
                 '((assign argl
                           (op list)
                           (reg val)))))))
          (if (null? (cdr operand-codes))
              code-to-get-last-arg
              (preserving 
               '(env)
               code-to-get-last-arg
               (code-to-get-rest-args
                (cdr operand-codes))))))))

(define (code-to-get-rest-args operand-codes)
  (let ((code-for-next-arg
         (preserving 
          '(argl)
          (car operand-codes)
          (make-instruction-sequence 
           '(val argl)
           '(argl)
           '((assign argl
                     (op cons)
                     (reg val)
                     (reg argl)))))))
    (if (null? (cdr operand-codes))
        code-for-next-arg
        (preserving 
         '(env)
         code-for-next-arg
         (code-to-get-rest-args 
          (cdr operand-codes))))))



(define (compile-procedure-call
         target linkage)
  (let ((primitive-branch 
         (make-label 'primitive-branch))
        (compiled-branch 
         (make-label 'compiled-branch))
        (after-call
         (make-label 'after-call)))
    (let ((compiled-linkage
           (if (eq? linkage 'next)
               after-call
               linkage)))
      (append-instruction-sequences
       (make-instruction-sequence 
        '(proc)
        '()
        `((test 
           (op primitive-procedure?)
           (reg proc))
          (branch 
           (label ,primitive-branch))))
       (parallel-instruction-sequences
        (append-instruction-sequences
         compiled-branch
         (compile-proc-appl 
          target
          compiled-linkage))
        (append-instruction-sequences
         primitive-branch
         (end-with-linkage
          linkage
          (make-instruction-sequence
           '(proc argl)
           (list target)
           `((assign 
              ,target
              (op apply-primitive-procedure)
              (reg proc)
              (reg argl)))))))
       after-call))))


(define (compile-proc-appl target linkage)
  (cond ((and (eq? target 'val)
              (not (eq? linkage 'return)))
         (make-instruction-sequence 
          '(proc)
          all-regs
          `((assign continue (label ,linkage))
            (assign 
             val 
             (op compiled-procedure-entry)
             (reg proc))
            (goto (reg val)))))
        ((and (not (eq? target 'val))
              (not (eq? linkage 'return)))
         (let ((proc-return 
                (make-label 'proc-return)))
           (make-instruction-sequence 
            '(proc)
            all-regs
            `((assign continue 
                      (label ,proc-return))
              (assign 
               val 
               (op compiled-procedure-entry)
               (reg proc))
              (goto (reg val))
              ,proc-return
              (assign ,target (reg val))
              (goto (label ,linkage))))))
        ((and (eq? target 'val)
              (eq? linkage 'return))
         (make-instruction-sequence 
          '(proc continue) 
          all-regs
          '((assign 
             val 
             (op compiled-procedure-entry)
             (reg proc))
            (goto (reg val)))))
        ((and (not (eq? target 'val))
              (eq? linkage 'return))
         (error "return linkage, 
                 target not val: COMPILE"
                target))))





;                                                                                                                                                                                                                  
;                                                                                                                                                                                                                  
;                                                                                                                                                                                                                  
;                                                                                                                                                                                                                  
;     ;;;;    ;;;   ;;; ;;; ;;;;;    ;;;;; ;;;  ;;;  ;;;;; ;;;  ;;;   ;;;;           ;;;;; ;;;  ;;;  ;;; ;  ;;;;;;; ;;;;;   ;;; ;;;   ;;;;  ;;;;;;;  ;;;;;    ;;;  ;;;  ;;;          ;;; ;  ;;;;;;    ;;;    ;;; ; 
;    ;   ;   ;   ;   ;; ;;   ;   ;     ;    ;;   ;     ;    ;;   ;   ;   ;             ;    ;;   ;  ;   ;;  ;  ;  ;  ;   ;   ;   ;   ;   ;  ;  ;  ;    ;     ;   ;  ;;   ;          ;   ;;   ;   ;   ;   ;  ;   ;; 
;   ;       ;     ;  ;; ;;   ;   ;     ;    ; ;  ;     ;    ; ;  ;  ;                  ;    ; ;  ;  ;          ;     ;   ;   ;   ;  ;          ;       ;    ;     ; ; ;  ;          ;        ; ;    ;     ; ;      
;   ;       ;     ;  ; ; ;   ;;;;      ;    ; ;  ;     ;    ; ;  ;  ;                  ;    ; ;  ;   ;;;;      ;     ;   ;   ;   ;  ;          ;       ;    ;     ; ; ;  ;           ;;;;    ;;;    ;     ;  ;;;;  
;   ;       ;     ;  ; ; ;   ;   ;     ;    ;  ; ;     ;    ;  ; ;  ;   ;;;            ;    ;  ; ;       ;     ;     ;;;;    ;   ;  ;          ;       ;    ;     ; ;  ; ;               ;   ; ;    ;     ;      ; 
;   ;       ;     ;  ;   ;   ;   ;     ;    ;  ; ;     ;    ;  ; ;  ;    ;             ;    ;  ; ;       ;     ;     ;  ;    ;   ;  ;          ;       ;    ;     ; ;  ; ;               ;   ;      ;     ;      ; 
;    ;   ;   ;   ;   ;   ;   ;   ;     ;    ;   ;;     ;    ;   ;;   ;   ;             ;    ;   ;;  ;;   ;     ;     ;   ;   ;   ;   ;   ;     ;       ;     ;   ;  ;   ;;          ;;   ;   ;   ;   ;   ;  ;;   ; 
;     ;;;     ;;;   ;;; ;;; ;;;;;    ;;;;; ;;;  ;;   ;;;;; ;;;  ;;    ;;;            ;;;;; ;;;  ;;  ; ;;;     ;;;   ;;;   ;   ;;;     ;;;     ;;;    ;;;;;    ;;;  ;;;  ;;          ; ;;;   ;;;;;;    ;;;   ; ;;;  
;                                                                                                                                                                                                     ;;;;;        
;                                                                                                                                                                                                                  
;                                                                                                                                                                                                                  
;                                                                                                                                                                                                                  





(define (registers-needed s)
  (if (symbol? s) '() (car s)))
(define (registers-modified s)
  (if (symbol? s) '() (cadr s)))
(define (statements s)
  (if (symbol? s) (list s) (caddr s)))


(define (needs-register? seq reg)
  (memq reg (registers-needed seq)))
(define (modifies-register? seq reg)
  (memq reg (registers-modified seq)))



(define (append-instruction-sequences . seqs)
  (define (append-2-sequences seq1 seq2)
    (make-instruction-sequence
     (list-union 
      (registers-needed seq1)
      (list-difference 
       (registers-needed seq2)
       (registers-modified seq1)))
     (list-union
      (registers-modified seq1)
      (registers-modified seq2))
     (append (statements seq1)
             (statements seq2))))
  (define (append-seq-list seqs)
    (if (null? seqs)
        (empty-instruction-sequence)
        (append-2-sequences 
         (car seqs)
         (append-seq-list (cdr seqs)))))
  (append-seq-list seqs))




(define (list-union s1 s2)
  (cond ((null? s1) s2)
        ((memq (car s1) s2)
         (list-union (cdr s1) s2))
        (else
         (cons (car s1)
               (list-union (cdr s1) s2)))))

(define (list-difference s1 s2)
  (cond ((null? s1) '())
        ((memq (car s1) s2)
         (list-difference (cdr s1) s2))
        (else 
         (cons (car s1)
               (list-difference (cdr s1)
                                s2)))))


(define (preserving regs seq1 seq2)
  (if (null? regs)
      (append-instruction-sequences seq1 seq2)
      (let ((first-reg (car regs)))
        (if (and 
             (needs-register? seq2 first-reg)
             (modifies-register? seq1 
                                 first-reg))
            (preserving 
             (cdr regs)
             (make-instruction-sequence
              (list-union 
               (list first-reg)
               (registers-needed seq1))
              (list-difference
               (registers-modified seq1)
               (list first-reg))
              (append `((save ,first-reg))
                      (statements seq1)
                      `((restore ,first-reg))))
             seq2)
            (preserving 
             (cdr regs)
             seq1
             seq2)))))



(define (tack-on-instruction-sequence 
         seq body-seq)
  (make-instruction-sequence
   (registers-needed seq)
   (registers-modified seq)
   (append (statements seq)
           (statements body-seq))))




(define (parallel-instruction-sequences 
         seq1 seq2)
  (make-instruction-sequence
   (list-union (registers-needed seq1)
               (registers-needed seq2))
   (list-union (registers-modified seq1)
               (registers-modified seq2))
   (append (statements seq1)
           (statements seq2))))



;                                                  
;                                                  
;                                                  
;                                                  
;     ;;;   ;;;;;;; ;;; ;;; ;;;;;;  ;;;;;    ;;; ; 
;    ;   ;  ;  ;  ;  ;   ;   ;   ;   ;   ;  ;   ;; 
;   ;     ;    ;     ;   ;   ; ;     ;   ;  ;      
;   ;     ;    ;     ;;;;;   ;;;     ;   ;   ;;;;  
;   ;     ;    ;     ;   ;   ; ;     ;;;;        ; 
;   ;     ;    ;     ;   ;   ;       ;  ;        ; 
;    ;   ;     ;     ;   ;   ;   ;   ;   ;  ;;   ; 
;     ;;;     ;;;   ;;; ;;; ;;;;;;  ;;;   ; ; ;;;  
;                                                  
;                                                  
;                                                  
;                                                  



(define label-counter 0)

(define (new-label-number)
  (set! label-counter (+ 1 label-counter))
  label-counter)

(define (make-label name)
  (string->symbol
   (string-append 
    (symbol->string name)
    (number->string (new-label-number)))))



(define (make-compiled-procedure entry env)
  (list 'compiled-procedure entry env))
(define (compiled-procedure? proc)
  (tagged-list? proc 'compiled-procedure))
(define (compiled-procedure-entry c-proc) 
  (cadr c-proc))
(define (compiled-procedure-env c-proc)
  (caddr c-proc))

   

;                                                                  
;                                                                  
;                                                                  
;                                                                  
;    ;;; ;  ;;;;;;  ;;;;;;; ;;;;;;;  ;;;;; ;;;  ;;;   ;;;;   ;;; ; 
;   ;   ;;   ;   ;  ;  ;  ; ;  ;  ;    ;    ;;   ;   ;   ;  ;   ;; 
;   ;        ; ;       ;       ;       ;    ; ;  ;  ;       ;      
;    ;;;;    ;;;       ;       ;       ;    ; ;  ;  ;        ;;;;  
;        ;   ; ;       ;       ;       ;    ;  ; ;  ;   ;;;      ; 
;        ;   ;         ;       ;       ;    ;  ; ;  ;    ;       ; 
;   ;;   ;   ;   ;     ;       ;       ;    ;   ;;   ;   ;  ;;   ; 
;   ; ;;;   ;;;;;;    ;;;     ;;;    ;;;;; ;;;  ;;    ;;;   ; ;;;  
;                                                                  
;                                                                  
;                                                                  
;                                                                  



(define all-regs '(env proc val argl continue))



(define cmpled-fcl (compile
                    '(define (factorial n)
                       (if (= n 1)
                           1
                           (* (factorial (- n 1)) n)))
                    'val
                    'next
                    the-empty-environment))


(define cmpled-proc-has-inner-defs (compile
                                    '(define (proc-has-inner-defs x y)
                                       (define inner-x 2)
                                       (println (+ x inner-x))
                                       (println (+ y inner-y))
                                       (println (+ inner-x inner-y inner-z))
                                       (define inner-y 4)
                                       (define inner-z 8)
                                       'ok)
                                    'val
                                    'next
                                    the-empty-environment))

(define cmpled-proc-has-inner-defs-reordered (compile
                                    '(define (proc-has-inner-defs x y)
                                       (define inner-x 2)
                                       (define inner-y 4)
                                       (define inner-z 8)
                                       (println (+ x inner-x))
                                       (println (+ y inner-y))
                                       (println (+ inner-x inner-y inner-z))
                                       'ok)
                                    'val
                                    'next
                                    the-empty-environment))

 

(statements cmpled-fcl)

(statements cmpled-proc-has-inner-defs)

