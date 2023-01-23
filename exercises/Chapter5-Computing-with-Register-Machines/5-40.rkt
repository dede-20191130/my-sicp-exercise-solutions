#lang racket



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


'(define (compile exp target linkage cenv)
   (cond ((self-evaluating? exp)
          (compile-self-evaluating 
           exp target linkage))
         ((quoted? exp) 
          (compile-quoted exp target linkage))
         ((variable? exp)
          (compile-variable 
           exp target linkage))
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
         ((application? exp)
          (compile-application 
           exp target linkage cenv))
         (else
          (error "Unknown expression type: 
                 COMPILE" 
                 exp))))


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



'(define (compile-assignment 
          exp target linkage cenv)
   (let ((var (assignment-variable exp))
         (get-value-code
          (compile (assignment-value exp) 
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
        `((perform (op set-variable-value!)
                   (const ,var)
                   (reg val)
                   (reg env))
          (assign ,target (const ok))))))))

'(define (compile-definition 
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


'(define (compile-if exp target linkage cenv)
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


'(define (compile-sequence seq target linkage cenv)
   (if (last-exp? seq)
       (compile (first-exp seq) target linkage cenv)
       (preserving '(env continue)
                   (compile (first-exp seq) target 'next cenv)
                   (compile-sequence (rest-exps seq)
                                     target
                                     linkage
                                     cenv))))


'(define (compile-lambda exp target linkage cenv)
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


'(define (compile-lambda-body exp proc-entry cenv)
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
       (compile-sequence (lambda-body exp)
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



'(define (compile-application 
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


