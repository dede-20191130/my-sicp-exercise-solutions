#lang racket
(require compatibility/mlist)



;                                                                                                                                                                          
;                                                                                                                                                                          
;                                                                                                                                                                          
;                                                                                                                                                                          
;   ;;;;;;  ;;;;;     ;;;   ;;; ;;;         ;;;;;   ;;;;;   ;;;;;; ;;;  ;;;  ;;;;;    ;;;   ;;; ;;;  ;;; ;           ;;; ;  ;;;;;;    ;;;;  ;;;;;;;  ;;;;;    ;;;  ;;;  ;;;
;    ;   ;   ;   ;   ;   ;   ;; ;;           ;   ;   ;   ;   ;   ;  ;    ;     ;     ;   ;   ;   ;  ;   ;;          ;   ;;   ;   ;   ;   ;  ;  ;  ;    ;     ;   ;  ;;   ; 
;    ; ;     ;   ;  ;     ;  ;; ;;           ;   ;   ;   ;   ; ;    ;    ;     ;    ;     ;  ;   ;  ;               ;        ; ;    ;          ;       ;    ;     ; ; ;  ; 
;    ;;;     ;   ;  ;     ;  ; ; ;           ;   ;   ;   ;   ;;;     ;  ;      ;    ;     ;  ;   ;   ;;;;            ;;;;    ;;;    ;          ;       ;    ;     ; ; ;  ; 
;    ; ;     ;;;;   ;     ;  ; ; ;           ;;;;    ;;;;    ; ;     ;  ;      ;    ;     ;  ;   ;       ;               ;   ; ;    ;          ;       ;    ;     ; ;  ; ; 
;    ;       ;  ;   ;     ;  ;   ;           ;       ;  ;    ;       ;  ;      ;    ;     ;  ;   ;       ;               ;   ;      ;          ;       ;    ;     ; ;  ; ; 
;    ;       ;   ;   ;   ;   ;   ;           ;       ;   ;   ;   ;    ;;       ;     ;   ;   ;   ;  ;;   ;          ;;   ;   ;   ;   ;   ;     ;       ;     ;   ;  ;   ;; 
;   ;;;     ;;;   ;   ;;;   ;;; ;;;         ;;;     ;;;   ; ;;;;;;    ;;     ;;;;;    ;;;     ;;;   ; ;;;           ; ;;;   ;;;;;;    ;;;     ;;;    ;;;;;    ;;;  ;;;  ;; 
;                                                                                                                                                                          
;                                                                                                                                                                          
;                                                                                                                                                                          
;                                                                                                                                                                          


(define apply-in-underlying-scheme apply)

;                                                          
;                                                          
;                                                          
;                                                          
;   ;;; ;;;   ;;      ;;;;  ;;; ;;;  ;;;;; ;;;  ;;; ;;;;;; 
;    ;; ;;     ;     ;   ;   ;   ;     ;    ;;   ;   ;   ; 
;    ;; ;;    ; ;   ;        ;   ;     ;    ; ;  ;   ; ;   
;    ; ; ;    ; ;   ;        ;;;;;     ;    ; ;  ;   ;;;   
;    ; ; ;    ; ;   ;        ;   ;     ;    ;  ; ;   ; ;   
;    ;   ;    ;;;   ;        ;   ;     ;    ;  ; ;   ;     
;    ;   ;   ;   ;   ;   ;   ;   ;     ;    ;   ;;   ;   ; 
;   ;;; ;;; ;;; ;;;   ;;;   ;;; ;;;  ;;;;; ;;;  ;;  ;;;;;; 
;                                                          
;                                                          
;                                                          
;                                                          


(define (make-machine  
         ops 
         controller-text)
  (let ((machine (make-new-machine)))
    ((machine 'install-operations) ops)
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    machine))

(define (make-register name)
  (let ((contents '*unassigned*))
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
            ((eq? message 'set)
             (lambda (value) 
               (set! contents value)))
            (else
             (error "Unknown request: 
                     REGISTER"
                    message))))
    dispatch))

(define (get-contents register)
  (register 'get))

(define (set-contents! register value)
  ((register 'set) value))


(define (make-stack)
  (let ((s '())
        (number-pushes 0)
        (max-depth 0)
        (current-depth 0))
    (define (push x)
      (set! s (cons x s))
      (set! number-pushes (+ 1 number-pushes))
      (set! current-depth (+ 1 current-depth))
      (set! max-depth 
            (max current-depth max-depth)))
    (define (pop)
      (if (null? s)
          (error "Empty stack: POP")
          (let ((top (car s)))
            (set! s (cdr s))
            (set! current-depth
                  (- current-depth 1))
            top)))
    (define (initialize)
      (set! s '())
      (set! number-pushes 0)
      (set! max-depth 0)
      (set! current-depth 0)
      'done)

    (define (print-statistics)
      (newline)
      (display (list 'total-pushes 
                     '= 
                     number-pushes
                     'maximum-depth
                     '=
                     max-depth)))
    (define (dispatch message)
      (cond ((eq? message 'push) push)
            ((eq? message 'pop) (pop))
            ((eq? message 'initialize)
             (initialize))
            ((eq? message 'print-statistics)
             (print-statistics))
            (else
             (error "Unknown request: STACK"
                    message))))
    dispatch))

(define (pop stack) (stack 'pop))
(define (push stack value)
  ((stack 'push) value))

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '()))
    (let ((the-ops
           (list 
            (list 'initialize-stack
                  (lambda () 
                    (stack 'initialize)))
            (list 'read
                  (lambda () 
                    (read)))
            (list 'print
                  (lambda (s) 
                    (println s)))
            (list 'initialize-stack
                  (lambda () 
                    (stack 'initialize)))
            (list 'print-stack-statistics
                  (lambda () 
                    (stack 'print-statistics)))))
          (register-table
           (list (list 'pc pc) 
                 (list 'flag flag))))
      (define (allocate-register name)
        (if (assoc name register-table)
            (error 
             "Multiply defined register: " 
             name)
            (set! register-table
                  (cons 
                   (list name 
                         (make-register name))
                   register-table)))
        'register-allocated)
      (define (lookup-register name)
        (let ((val 
               (assoc name register-table)))
          (if val
              (cadr val)
              (begin
                (allocate-register name)
                (cadr (assoc name register-table))))))
      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
              'done
              (begin
                ((instruction-execution-proc 
                  (car insts)))
                (execute)))))
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! 
                pc
                the-instruction-sequence)
               (execute))
              ((eq? 
                message 
                'install-instruction-sequence)
               (lambda (seq) 
                 (set! 
                  the-instruction-sequence 
                  seq)))
              ((eq? message 
                    'allocate-register) 
               allocate-register)
              ((eq? message 'get-register) 
               lookup-register)
              ((eq? message 
                    'install-operations)
               (lambda (ops) 
                 (set! the-ops 
                       (append the-ops ops))))
              ((eq? message 'stack) stack)
              ((eq? message 'operations) 
               the-ops)
              (else (error "Unknown request: 
                            MACHINE"
                           message))))
      dispatch)))

(define (start machine)
  (machine 'start))

(define (get-register-contents 
         machine register-name)
  (get-contents 
   (get-register machine register-name)))

(define (set-register-contents! 
         machine register-name value)
  (set-contents! 
   (get-register machine register-name) 
   value)
  'done)

(define (get-register machine reg-name)
  ((machine 'get-register) reg-name))


;                                                                          
;                                                                          
;                                                                          
;                                                                          
;     ;;     ;;; ;   ;;; ;  ;;;;;;  ;;; ;;; ;;;;;   ;;;     ;;;;;;  ;;;;;  
;      ;    ;   ;;  ;   ;;   ;   ;   ;; ;;   ;   ;   ;       ;   ;   ;   ; 
;     ; ;   ;       ;        ; ;     ;; ;;   ;   ;   ;       ; ;     ;   ; 
;     ; ;    ;;;;    ;;;;    ;;;     ; ; ;   ;;;;    ;       ;;;     ;   ; 
;     ; ;        ;       ;   ; ;     ; ; ;   ;   ;   ;       ; ;     ;;;;  
;     ;;;        ;       ;   ;       ;   ;   ;   ;   ;   ;   ;       ;  ;  
;    ;   ;  ;;   ;  ;;   ;   ;   ;   ;   ;   ;   ;   ;   ;   ;   ;   ;   ; 
;   ;;; ;;; ; ;;;   ; ;;;   ;;;;;;  ;;; ;;; ;;;;;   ;;;;;;  ;;;;;;  ;;;   ;
;                                                                          
;                                                                          
;                                                                          
;                                                                          



(define (assemble controller-text machine)
  (extract-labels controller-text
                  (lambda (insts labels)
                    (update-insts! insts labels machine)
                    insts)))

(define (extract-labels text receive)
  (if (null? text)
      (receive '() '())
      (extract-labels 
       (cdr text)
       (lambda (insts labels)
         (let ((next-inst (car text)))
           (if (symbol? next-inst)
               (receive 
                insts
                (cons 
                 (make-label-entry 
                  next-inst
                  insts)
                 labels))
               (receive 
                (cons (make-instruction 
                       next-inst)
                      insts)
                labels)))))))

(define (update-insts! insts labels machine)
  (let ((pc (get-register machine 'pc))
        (flag (get-register machine 'flag))
        (stack (machine 'stack))
        (ops (machine 'operations)))
    (for-each
     (lambda (inst)
       (set-instruction-execution-proc!
        inst
        (make-execution-procedure
         (instruction-text inst) 
         labels
         machine
         pc
         flag
         stack
         ops)))
     insts)))

(define (make-instruction text)
  (mcons text '()))
(define (instruction-text inst) (mcar inst))
(define (instruction-execution-proc inst)
  (mcdr inst))
(define (set-instruction-execution-proc!
         inst
         proc)
  (set-mcdr! inst proc))

(define (make-label-entry label-name insts)
  (cons label-name insts))

(define (lookup-label labels label-name)
  (let ((val (assoc label-name labels)))
    (if val
        (cdr val)
        (error "Undefined label: ASSEMBLE" 
               label-name))))


;                                                                                                                                                          
;                                                                                                                                                          
;                                                                                                                                                          
;                                                                                                                                                          
;   ;;;;;;  ;;; ;;; ;;;;;;    ;;;;  ;;; ;;; ;;;;;;;  ;;;;;    ;;;  ;;;  ;;;         ;;;;;   ;;;;;     ;;;     ;;;;  ;;;;;;  ;;;;    ;;; ;;; ;;;;;   ;;;;;; 
;    ;   ;   ;   ;   ;   ;   ;   ;   ;   ;  ;  ;  ;    ;     ;   ;  ;;   ;           ;   ;   ;   ;   ;   ;   ;   ;   ;   ;   ;  ;    ;   ;   ;   ;   ;   ; 
;    ; ;      ; ;    ; ;    ;        ;   ;     ;       ;    ;     ; ; ;  ;           ;   ;   ;   ;  ;     ; ;        ; ;     ;   ;   ;   ;   ;   ;   ; ;   
;    ;;;       ;     ;;;    ;        ;   ;     ;       ;    ;     ; ; ;  ;           ;   ;   ;   ;  ;     ; ;        ;;;     ;   ;   ;   ;   ;   ;   ;;;   
;    ; ;       ;     ; ;    ;        ;   ;     ;       ;    ;     ; ;  ; ;           ;;;;    ;;;;   ;     ; ;        ; ;     ;   ;   ;   ;   ;;;;    ; ;   
;    ;        ; ;    ;      ;        ;   ;     ;       ;    ;     ; ;  ; ;           ;       ;  ;   ;     ; ;        ;       ;   ;   ;   ;   ;  ;    ;     
;    ;   ;   ;   ;   ;   ;   ;   ;   ;   ;     ;       ;     ;   ;  ;   ;;           ;       ;   ;   ;   ;   ;   ;   ;   ;   ;  ;    ;   ;   ;   ;   ;   ; 
;   ;;;;;;  ;;; ;;; ;;;;;;    ;;;     ;;;     ;;;    ;;;;;    ;;;  ;;;  ;;          ;;;     ;;;   ;   ;;;     ;;;   ;;;;;;  ;;;;      ;;;   ;;;   ; ;;;;;; 
;                                                                                                                                                          
;                                                                                                                                                          
;                                                                                                                                                          
;                                                                                                                                                          

(define (make-execution-procedure 
         inst labels machine pc flag stack ops)
  (cond ((eq? (car inst) 'assign)
         (make-assign 
          inst machine labels ops pc))
        ((eq? (car inst) 'test)
         (make-test 
          inst machine labels ops flag pc))
        ((eq? (car inst) 'branch)
         (make-branch 
          inst machine labels flag pc))
        ((eq? (car inst) 'goto)
         (make-goto inst machine labels pc))
        ((eq? (car inst) 'save)
         (make-save inst machine stack pc))
        ((eq? (car inst) 'restore)
         (make-restore inst machine stack pc))
        ((eq? (car inst) 'perform)
         (make-perform
          inst machine labels ops pc))
        (else (error "Unknown instruction 
                      type: ASSEMBLE"
                     inst))))

(define (make-assign 
         inst machine labels operations pc)
  (let ((target 
         (get-register 
          machine 
          (assign-reg-name inst)))
        (value-exp (assign-value-exp inst)))
    (let ((value-proc
           (if (operation-exp? value-exp)
               (make-operation-exp
                value-exp 
                machine
                labels
                operations)
               (make-primitive-exp
                (car value-exp)
                machine
                labels))))
      (lambda ()   ; execution procedure
        ; for assign
        (set-contents! target (value-proc))
        (advance-pc pc)))))

(define (assign-reg-name assign-instruction)
  (cadr assign-instruction))
(define (assign-value-exp assign-instruction)
  (cddr assign-instruction))

(define (advance-pc pc)
  (set-contents! pc (cdr (get-contents pc))))

(define 
  (make-test 
   inst machine labels operations flag pc)
  (let ((condition (test-condition inst)))
    (if (operation-exp? condition)
        (let ((condition-proc
               (make-operation-exp
                condition 
                machine
                labels
                operations)))
          (lambda () 
            (set-contents! 
             flag (condition-proc))
            (advance-pc pc)))
        (error "Bad TEST instruction: 
                ASSEMBLE" inst))))

(define (test-condition test-instruction)
  (cdr test-instruction))

(define 
  (make-branch 
   inst machine labels flag pc)
  (let ((dest (branch-dest inst)))
    (if (label-exp? dest)
        (let ((insts
               (lookup-label 
                labels 
                (label-exp-label dest))))
          (lambda ()
            (if (get-contents flag)
                (set-contents! pc insts)
                (advance-pc pc))))
        (error "Bad BRANCH instruction: 
                ASSEMBLE"
               inst))))

(define (branch-dest branch-instruction)
  (cadr branch-instruction))

(define (make-goto inst machine labels pc)
  (let ((dest (goto-dest inst)))
    (cond ((label-exp? dest)
           (let ((insts
                  (lookup-label 
                   labels
                   (label-exp-label dest))))
             (lambda () 
               (set-contents! pc insts))))
          ((register-exp? dest)
           (let ((reg
                  (get-register 
                   machine
                   (register-exp-reg dest))))
             (lambda ()
               (set-contents! 
                pc
                (get-contents reg)))))
          (else (error "Bad GOTO instruction: 
                        ASSEMBLE"
                       inst)))))

(define (goto-dest goto-instruction)
  (cadr goto-instruction))

(define (make-save inst machine stack pc)
  (let ((reg (get-register 
              machine
              (stack-inst-reg-name inst))))
    (lambda ()
      (push stack (get-contents reg))
      (advance-pc pc))))

(define (make-restore inst machine stack pc)
  (let ((reg (get-register
              machine
              (stack-inst-reg-name inst))))
    (lambda ()
      (set-contents! reg (pop stack))
      (advance-pc pc))))

(define (stack-inst-reg-name 
         stack-instruction)
  (cadr stack-instruction))

(define (make-perform 
         inst machine labels operations pc)
  (let ((action (perform-action inst)))
    (if (operation-exp? action)
        (let ((action-proc
               (make-operation-exp
                action
                machine
                labels
                operations)))
          (lambda ()
            (action-proc)
            (advance-pc pc)))
        (error "Bad PERFORM instruction: 
                ASSEMBLE"
               inst))))

(define (perform-action inst) (cdr inst))

(define (make-primitive-exp exp machine labels)
  (cond ((constant-exp? exp)
         (let ((c (constant-exp-value exp)))
           (lambda () c)))
        ((label-exp? exp)
         (let ((insts
                (lookup-label 
                 labels
                 (label-exp-label exp))))
           (lambda () insts)))
        ((register-exp? exp)
         (let ((r (get-register
                   machine
                   (register-exp-reg exp))))
           (lambda () (get-contents r))))
        (else (error "Unknown expression type: 
                      ASSEMBLE"
                     exp))))

(define (register-exp? exp)
  (tagged-list? exp 'reg))
(define (register-exp-reg exp)
  (cadr exp))
(define (constant-exp? exp)
  (tagged-list? exp 'const))
(define (constant-exp-value exp)
  (cadr exp))
(define (label-exp? exp)
  (tagged-list? exp 'label))
(define (label-exp-label exp) 
  (cadr exp))
(define (the-side-of-p dec) (car dec))
(define (the-side-of-e dec) (cadr dec))

(define (make-operation-exp
         exp machine labels operations)
  (let ((op (lookup-prim 
             (operation-exp-op exp)
             operations))
        (aprocs
         (map (lambda (e)
                (make-primitive-exp 
                 e machine labels))
              (operation-exp-operands exp))))
    (lambda () (apply op (map (lambda (p) (p))
                              aprocs)))))

(define (operation-exp? exp)
  (and (pair? exp)
       (tagged-list? (car exp) 'op)))
(define (operation-exp-op operation-exp)
  (cadr (car operation-exp)))
(define (operation-exp-operands operation-exp)
  (cdr operation-exp))

(define (lookup-prim symbol operations)
  (let ((val (assoc symbol operations)))
    (if val
        (cadr val)
        (error "Unknown operation: ASSEMBLE"
               symbol))))


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


;(define open-code-primitives '())
(define open-code-primitives '(= * - +))


(define (open-code-primitives? exp cenv)
  (if (pair? exp)
      (if (memq (car exp) open-code-primitives)
          (if (eq? (find-variable (car exp) cenv) NOT-FOUND)
              true
              false)
          false)
      false))


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
        ((open-code-primitives? exp cenv)
         (compile-open-coded
          exp target linkage cenv))
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


(define (spread-arguments operand-list cenv)
  (let ((operand1 (car operand-list))
        (operand2 (cadr operand-list)))
    (let ((compile-operand1 (compile operand1 'val 'next cenv))
          (compile-operand2 (compile operand2 'val 'next cenv)))
      (preserving
       '(env)
       (append-instruction-sequences
        compile-operand1
        (make-instruction-sequence
         '()
         '(arg1)
         '((assign arg1 (reg val)))))
       (preserving
        '(arg1)
        compile-operand2
        (make-instruction-sequence
         '(arg1)
         '(arg2)
         '((assign arg2 (reg val)))))))))


(define (compile-open-coded exp target linkage cenv)
  (define (oc-for-others)
    (end-with-linkage
     linkage
     (preserving
      '(continue)
      (spread-arguments (operands exp) cenv)
      (make-instruction-sequence
       '(arg1 arg2)
       `(,target)
       `((assign ,target (op ,(operator exp))
                 (reg arg1)
                 (reg arg2)))))))
  (define (oc-for-+*)
    (let ((first-2-oprd (take (operands exp) 2))
          (rest-oprd (drop (operands exp) 2)))
      (let ((first-2-seqs
             (preserving
              '(continue env)
              (spread-arguments first-2-oprd cenv)
              (make-instruction-sequence
               '(arg1 arg2)
               `(,target)
               `((assign ,target (op ,(operator exp))
                         (reg arg1)
                         (reg arg2)))))))
        (oc-manage-over2 first-2-seqs rest-oprd))))
  (define (oc-manage-over2 seqs rest)
    (if (null? rest)
        (end-with-linkage linkage seqs)
        (oc-manage-over2
         (append-instruction-sequences
          seqs
          (preserving
           `(,target env continue)
           (append-instruction-sequences
            (compile (car rest) 'val 'next cenv)
            (make-instruction-sequence
             '(val)
             '(arg1)
             '(
               (assign arg1 (reg val))
               )))
           (make-instruction-sequence
            `(arg1 ,target)
            `(,target)
            `(
              (assign ,target (op ,(operator exp))
                      (reg arg1)
                      (reg ,target))))))
         (cdr rest))))
  (if (memq (operator exp) '(+ *))
      (oc-for-+*)
      (oc-for-others)))

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
        (compound-branch
         (make-label 'compound-branch))
        (compound-branch-end
         (make-label 'compound-branch-end))
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
           (label ,primitive-branch))
          (test 
           (op compound-procedure?)
           (reg proc))
          (branch 
           (label ,compound-branch))))
       (parallel-instruction-sequences
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
        (append-instruction-sequences
         compound-branch
         (make-instruction-sequence
          '(compapp)
          '()
          `((save continue)
            (assign continue (label ,compound-branch-end))
            (save continue)
            (goto (reg compapp))))
         compound-branch-end
         (make-instruction-sequence
          '()
          (list target)
          `((restore continue)
            (assign 
             ,target
             (reg val))))
         (compile-linkage linkage)))
            
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
;   ;;;;;   ;;; ;;;;;;  ;;; ;;;;;;;  ;;;;;  ;;; ;;; ;;;;;;           ;;; ;  ;;; ;;; ;;;;;   ;;;;;     ;;;   ;;;;;   ;;;;;;;         ;;;;;   ;;;;;     ;;;     ;;;;  ;;;;;;  ;;;;    ;;; ;;; ;;;;;   ;;;;;; 
;    ;   ;   ;   ;  ;;   ;  ;  ;  ;    ;     ;; ;;   ;   ;          ;   ;;   ;   ;   ;   ;   ;   ;   ;   ;   ;   ;  ;  ;  ;          ;   ;   ;   ;   ;   ;   ;   ;   ;   ;   ;  ;    ;   ;   ;   ;   ;   ; 
;    ;   ;   ;   ;  ; ;  ;     ;       ;     ;; ;;   ; ;            ;        ;   ;   ;   ;   ;   ;  ;     ;  ;   ;     ;             ;   ;   ;   ;  ;     ; ;        ; ;     ;   ;   ;   ;   ;   ;   ; ;   
;    ;   ;   ;   ;  ; ;  ;     ;       ;     ; ; ;   ;;;             ;;;;    ;   ;   ;   ;   ;   ;  ;     ;  ;   ;     ;             ;   ;   ;   ;  ;     ; ;        ;;;     ;   ;   ;   ;   ;   ;   ;;;   
;    ;;;;    ;   ;  ;  ; ;     ;       ;     ; ; ;   ; ;                 ;   ;   ;   ;;;;    ;;;;   ;     ;  ;;;;      ;             ;;;;    ;;;;   ;     ; ;        ; ;     ;   ;   ;   ;   ;;;;    ; ;   
;    ;  ;    ;   ;  ;  ; ;     ;       ;     ;   ;   ;                   ;   ;   ;   ;       ;      ;     ;  ;  ;      ;             ;       ;  ;   ;     ; ;        ;       ;   ;   ;   ;   ;  ;    ;     
;    ;   ;   ;   ;  ;   ;;     ;       ;     ;   ;   ;   ;          ;;   ;   ;   ;   ;       ;       ;   ;   ;   ;     ;             ;       ;   ;   ;   ;   ;   ;   ;   ;   ;  ;    ;   ;   ;   ;   ;   ; 
;   ;;;   ;   ;;;  ;;;  ;;    ;;;    ;;;;;  ;;; ;;; ;;;;;;          ; ;;;     ;;;   ;;;     ;;;       ;;;   ;;;   ;   ;;;           ;;;     ;;;   ;   ;;;     ;;;   ;;;;;;  ;;;;      ;;;   ;;;   ; ;;;;;; 
;                                                                                                                                                                                                          
;                                                                                                                                                                                                          
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
        (list 'null? null?)
        (list 'eq? eq?)
        (list 'eq? not)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list '> >)
        (list '< <)
        (list '= =)
        (list '>= >=)
        (list '<= <=)
        (list 'append append)
        (list 'newline newline)
        (list 'display display)
        (list 'println println)
        (list 'sin sin)
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

(define input-prompt  ";;; L-Eval input:")
(define output-prompt ";;; L-Eval value:")

(define (prompt-for-input string)
  (newline) (newline) 
  (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (cond ((compound-procedure? object)
         (display 
          (list 'compound-procedure
                (procedure-parameters object)
                (procedure-body object)
                '<procedure-env>)))
        ((compiled-procedure? object)
         (display '<compiled-procedure>))
        (else (display object))))

(define (get-global-environment)
  the-global-environment)

(define (compile-and-go expression)
  (set! the-global-environment
        (setup-environment))
  (let ((instructions
         (assemble 
          (statements
           (compile 
            expression 'val 'return the-global-environment))
          eceval)))
    (set-register-contents! 
     eceval 'val instructions)
    (set-register-contents! 
     eceval 'flag true)
    (start eceval)))

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

(define (get-eceval) eceval)

(define eceval-operations
  (list (list 'eq? eq?)
        (list 'null? null?)
        (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'list list)
        (list 'prompt-for-input prompt-for-input)
        (list 'get-global-environment get-global-environment)
        (list 'announce-output announce-output)
        (list 'user-print user-print)
        (list 'self-evaluating? self-evaluating?)
        (list 'variable? variable?)
        (list 'quoted? quoted?)
        (list 'assignment? assignment?)
        (list 'definition? definition?)
        (list 'if? if?)
        (list 'lambda? lambda?)
        (list 'begin? begin?)
        (list 'application? application?)
        (list 'lookup-variable-value lookup-variable-value)
        (list 'text-of-quotation text-of-quotation)
        (list 'lambda-parameters lambda-parameters)
        (list 'lambda-body lambda-body)
        (list 'make-procedure make-procedure)
        (list 'operands operands)
        (list 'operator operator)
        (list 'empty-arglist empty-arglist)
        (list 'no-operands? no-operands?)
        (list 'first-operand first-operand)
        (list 'last-operand? last-operand?)
        (list 'adjoin-arg adjoin-arg)
        (list 'rest-operands rest-operands)
        (list 'primitive-procedure? primitive-procedure?)
        (list 'compound-procedure? compound-procedure?)
        (list 'apply-primitive-procedure apply-primitive-procedure)
        (list 'procedure-parameters procedure-parameters)
        (list 'procedure-environment procedure-environment)
        (list 'extend-environment extend-environment)
        (list 'procedure-body procedure-body)
        (list 'begin-actions begin-actions)
        (list 'first-exp first-exp)
        (list 'last-exp? last-exp?)
        (list 'rest-exps rest-exps)
        (list 'if-predicate if-predicate)
        (list 'true? true?)
        (list 'false? false?)
        (list 'if-alternative if-alternative)
        (list 'if-consequent if-consequent)
        (list 'assignment-variable assignment-variable)
        (list 'assignment-value assignment-value)
        (list 'set-variable-value! set-variable-value!)
        (list 'definition-variable definition-variable)
        (list 'definition-value definition-value)
        (list 'define-variable! define-variable!)
        (list 'cond? cond?)
        (list 'let? let?)
        (list 'cond-clauses cond-clauses)
        (list 'cond-else-clause? cond-else-clause?)
        (list 'cond-predicate cond-predicate)
        (list 'cond-actions cond-actions)
        (list 'let-var-declarations let-var-declarations)
        (list 'the-side-of-p the-side-of-p)
        (list 'the-side-of-e the-side-of-e)
        (list 'let-body let-body)
        (list 'make-lambda make-lambda)
        (list 'sequence->exp sequence->exp)
        ;;; added for compiling
        (list 'compiled-procedure? compiled-procedure?)
        (list 'lexical-address-lookup lexical-address-lookup)
        (list 'lexical-address-set! lexical-address-set!)
        (list 'make-compiled-procedure make-compiled-procedure)
        (list 'compiled-procedure-env compiled-procedure-env)
        (list 'compiled-procedure-entry compiled-procedure-entry)
        (list '+ +)
        (list '* *)
        (list '- -)
        (list '= =)
        (list 'compile compile)
        (list 'statements statements)
        (list 'assemble assemble)
        (list 'get-eceval get-eceval)
        ))
        



;;; evaluator machine 
(define eceval
  (make-machine
   eceval-operations
   '((assign compapp (label compound-apply))
     (branch (label external-entry))
     read-compile-execute-print-loop
     (perform (op initialize-stack))
     (perform (op prompt-for-input)
              (const ";;; EC-Eval input:"))
     (assign exp (op read))
     (test (op eq?) (reg exp) (const ___exit)) ;add for graceful exit on racket
     (branch (label end-of-repl)) ;
     (test (op eq?) (reg exp) (const ___e)) ;
     (branch (label end-of-repl)) ;
     (assign env (op get-global-environment))
     (assign val (op compile) (reg exp) (const val) (const return) (reg env))
     (assign val (op statements) (reg val))
     (assign exp (op get-eceval))
     (assign val (op assemble) (reg val) (reg exp))   
     (assign continue (label print-result))
     (goto (reg val))
     print-result
     (perform (op print-stack-statistics))
     (perform (op announce-output)
              (const ";;; EC-Eval value:"))
     (perform (op user-print) (reg val))
     (goto (label read-compile-execute-print-loop))
     external-entry
     (perform (op initialize-stack))
     (assign env (op get-global-environment))
     (assign continue (label print-result))
     (goto (reg val))
     unknown-expression-type
     (assign 
      val
      (const unknown-expression-type-error))
     (goto (label signal-error))
     unknown-procedure-type
     ; clean up stack (from apply-dispatch):
     (restore continue)    
     (assign 
      val
      (const unknown-procedure-type-error))
     (goto (label signal-error))
     signal-error
     (perform (op user-print) (reg val))
     (goto (label read-compile-execute-print-loop))
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
     (goto (label unknown-expression-type))
     ev-self-eval
     (assign val (reg exp))
     (goto (reg continue))
     ev-variable
     (assign val
             (op lookup-variable-value)
             (reg exp)
             (reg env))
     (goto (reg continue))
     ev-quoted
     (assign val
             (op text-of-quotation)
             (reg exp))
     (goto (reg continue))
     ev-lambda
     (assign unev
             (op lambda-parameters)
             (reg exp))
     (assign exp 
             (op lambda-body)
             (reg exp))
     (assign val 
             (op make-procedure)
             (reg unev)
             (reg exp)
             (reg env))
     (goto (reg continue))
     ev-application
     (save continue)
     (save env)
     (assign unev (op operands) (reg exp))
     (save unev)
     (assign exp (op operator) (reg exp))
     (assign
      continue (label ev-appl-did-operator))
     (goto (label eval-dispatch))
     ev-appl-did-operator
     (restore unev)             ; the operands
     (restore env)
     (assign argl (op empty-arglist))
     (assign proc (reg val))    ; the operator
     (test (op no-operands?) (reg unev))
     (branch (label apply-dispatch))
     (save proc)
     ev-appl-operand-loop
     (save argl)
     (assign exp
             (op first-operand)
             (reg unev))
     (test (op last-operand?) (reg unev))
     (branch (label ev-appl-last-arg))
     (save env)
     (save unev)
     (assign continue 
             (label ev-appl-accumulate-arg))
     (goto (label eval-dispatch))
     ev-appl-accumulate-arg
     (restore unev)
     (restore env)
     (restore argl)
     (assign argl 
             (op adjoin-arg)
             (reg val)
             (reg argl))
     (assign unev
             (op rest-operands)
             (reg unev))
     (goto (label ev-appl-operand-loop))
     ev-appl-last-arg
     (assign continue 
             (label ev-appl-accum-last-arg))
     (goto (label eval-dispatch))
     ev-appl-accum-last-arg
     (restore argl)
     (assign argl 
             (op adjoin-arg)
             (reg val)
             (reg argl))
     (restore proc)
     (goto (label apply-dispatch))
     apply-dispatch
     (test (op primitive-procedure?) (reg proc))
     (branch (label primitive-apply))
     (test (op compound-procedure?) (reg proc))
     (branch (label compound-apply))
     (test (op compiled-procedure?) (reg proc))
     (branch (label compiled-apply))
     (goto (label unknown-procedure-type))
     primitive-apply
     (assign val (op apply-primitive-procedure)
             (reg proc)
             (reg argl))
     (restore continue)
     (goto (reg continue))
     compound-apply
     (assign unev 
             (op procedure-parameters)
             (reg proc))
     (assign env
             (op procedure-environment)
             (reg proc))
     (assign env
             (op extend-environment)
             (reg unev)
             (reg argl)
             (reg env))
     (assign unev
             (op procedure-body)
             (reg proc))
     (goto (label ev-sequence))
     compiled-apply
     (restore continue)
     (assign val
             (op compiled-procedure-entry)
             (reg proc))
     (goto (reg val))
     ev-begin
     (assign unev
             (op begin-actions)
             (reg exp))
     (save continue)
     (goto (label ev-sequence))
     ev-sequence
     (assign exp (op first-exp) (reg unev))
     (test (op last-exp?) (reg unev))
     (branch (label ev-sequence-last-exp))
     (save unev)
     (save env)
     (assign continue
             (label ev-sequence-continue))
     (goto (label eval-dispatch))
     ev-sequence-continue
     (restore env)
     (restore unev)
     (assign unev
             (op rest-exps)
             (reg unev))
     (goto (label ev-sequence))
     ev-sequence-last-exp
     (restore continue)
     (goto (label eval-dispatch))
     ev-if
     (save exp)   ; save expression for later
     (save env)
     (save continue)
     (assign continue (label ev-if-decide))
     (assign exp (op if-predicate) (reg exp))
     ; evaluate the predicate:
     (goto (label eval-dispatch))  
     ev-if-decide
     (restore continue)
     (restore env)
     (restore exp)
     (test (op true?) (reg val))
     (branch (label ev-if-consequent))
     ev-if-alternative
     (assign exp (op if-alternative) (reg exp))
     (goto (label eval-dispatch))
     ev-if-consequent
     (assign exp (op if-consequent) (reg exp))
     (goto (label eval-dispatch))
     ev-assignment
     (assign unev 
             (op assignment-variable)
             (reg exp))
     (save unev)   ; save variable for later
     (assign exp
             (op assignment-value)
             (reg exp))
     (save env)
     (save continue)
     (assign continue
             (label ev-assignment-1))
     ; evaluate the assignment value:
     (goto (label eval-dispatch))  
     ev-assignment-1
     (restore continue)
     (restore env)
     (restore unev)
     (perform (op set-variable-value!)
              (reg unev)
              (reg val)
              (reg env))
     (assign val
             (const ok))
     (goto (reg continue))
     ev-definition
     (assign unev 
             (op definition-variable)
             (reg exp))
     (save unev)   ; save variable for later
     (assign exp 
             (op definition-value)
             (reg exp))
     (save env)
     (save continue)
     (assign continue (label ev-definition-1))
     ; evaluate the definition value:
     (goto (label eval-dispatch))  
     ev-definition-1
     (restore continue)
     (restore env)
     (restore unev)
     (perform (op define-variable!)
              (reg unev)
              (reg val)
              (reg env))
     (assign val (const ok))
     (goto (reg continue))
     ev-cond
     (assign unev (op cond-clauses) (reg exp))
     (test (op null?) (reg unev))
     (branch (label ev-cond-null))
     (save env)
     (save continue)
     ev-cond-clau-test-loop
     (assign the-clause (op car) (reg unev))
     (test (op cond-else-clause?) (reg the-clause))
     (branch (label ev-cond-do-action))
     (save unev)
     (assign continue (label ev-cond-decide))
     (assign exp (op cond-predicate) (reg the-clause))
     (goto (label eval-dispatch))
     ev-cond-decide
     (restore unev)
     (test (op true?) (reg val))
     (branch (label ev-cond-do-action))
     (assign unev (op cdr) (reg unev))
     (goto (label ev-cond-clau-test-loop))
     ev-cond-null
     (assign exp (const 'false))
     (goto (label eval-dispatch))
     ev-cond-do-action
     (restore continue)
     (restore env)
     (assign exp (op cond-actions) (reg the-clause))
     (assign exp (op sequence->exp) (reg exp))
     (goto (label eval-dispatch))
     ev-let
     (assign params (op list))
     (assign exps-for-params (op list))
     (assign unev (op let-var-declarations) (reg exp))
     ev-let-decouple-loop
     (test (op null?) (reg unev))
     (branch (label ev-let-combination)) 
     (assign the-pair (op car) (reg unev))
     (assign one-side (op the-side-of-p) (reg the-pair))
     (assign params (op cons) (reg one-side) (reg params))
     (assign one-side (op the-side-of-e) (reg the-pair))
     (assign exps-for-params (op cons) (reg one-side) (reg exps-for-params))
     (assign unev (op cdr) (reg unev))
     (goto (label ev-let-decouple-loop))
     ev-let-combination
     (assign exp (op let-body) (reg exp))
     (assign new-lambda (op make-lambda) (reg params) (reg exp))
     (assign exp (op cons) (reg new-lambda) (reg exps-for-params))
     (goto (label eval-dispatch))
     end-of-repl
     (perform (op print) (const exit)))))

  
(define the-global-environment '())





;                          
;                          
;                          
;                          
;   ;;;;;   ;;; ;;;;;;  ;;;
;    ;   ;   ;   ;  ;;   ; 
;    ;   ;   ;   ;  ; ;  ; 
;    ;   ;   ;   ;  ; ;  ; 
;    ;;;;    ;   ;  ;  ; ; 
;    ;  ;    ;   ;  ;  ; ; 
;    ;   ;   ;   ;  ;   ;; 
;   ;;;   ;   ;;;  ;;;  ;; 
;                          
;                          
;                          
;                          


(compile-and-go
 '(begin
    (define (test-proc a) (+ a 2))
    ))
