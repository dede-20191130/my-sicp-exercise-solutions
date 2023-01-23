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

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (make-appendable-table)
  (let ((local-table (mlist '*table*)))
    (define (lookup key)
      (let ((record (massoc key (mcdr local-table))))
        (if record
            (mcdr record)
            false)))
    (define (insert! key value)
      (let ((record (massoc key (mcdr local-table))))
        (if record
            (set-mcdr! record (cons value (mcdr record)))
            (set-mcdr! local-table
                       (mcons (mcons key (list value)) 
                              (mcdr local-table)))))
      'ok)
    (define (display-table the-format)
      (mfor-each
       (lambda(m-entry)
         (printf the-format (mcar m-entry) (mcdr m-entry)))
       (mcdr local-table)))
    
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            ((eq? m 'display-table) display-table)
            ((eq? m 'table) local-table)
            (else (error "Unknown operation: 
                          TABLE" m))))
    dispatch))

(define (lookup tbl key) ((tbl 'lookup-proc) key))
(define (insert! tbl key value) ((tbl 'insert-proc!) key value))
(define (display-table tbl the-format) ((tbl 'display-table) the-format))

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


(define (make-machine register-names 
                      ops 
                      controller-text)
  (let ((machine (make-new-machine)))
    (for-each (lambda (register-name)
                ((machine 'allocate-register) 
                 register-name))
              register-names)
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
  (let ((s '()))
    (define (push x)
      (set! s (cons x s)))
    (define (pop)
      (if (null? s)
          (error "Empty stack: POP")
          (let ((top (car s)))
            (set! s (cdr s))
            top)))
    (define (initialize)
      (set! s '())
      'done)
    (define (dispatch message)
      (cond ((eq? message 'push) push)
            ((eq? message 'pop) (pop))
            ((eq? message 'initialize) 
             (initialize))
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
        (the-instruction-sequence '())
        (list-all-insts '())
        (list-reg-holding-ep '())
        (list-reg-dealing-with-stack '())
        (tbl-reg-and-assigned-data '()))
    (let ((the-ops
           (list 
            (list 'initialize-stack
                  (lambda () 
                    (stack 'initialize)))))
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
              (error "Unknown register:" 
                     name))))
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
              ((eq? 
                message 
                'install-list-all-insts)
               (lambda (thelist) 
                 (set! 
                  list-all-insts
                  (sort thelist (lambda(xlst ylst) (string<? (symbol->string (car xlst)) (symbol->string(car ylst))))))))
              ((eq? 
                message 
                'install-list-reg-holding-ep)
               (lambda (thelist) 
                 (set!  list-reg-holding-ep thelist)))
              ((eq? 
                message 
                'install-list-reg-dealing-with-stack)
               (lambda (thelist) 
                 (set!  list-reg-dealing-with-stack thelist)))
              ((eq? 
                message 
                'install-tbl-reg-and-assigned-data)
               (lambda (tbl) 
                 (set!  tbl-reg-and-assigned-data tbl)))
              ((eq? message 'get-list-all-insts) list-all-insts)
              ((eq? message 'get-list-reg-holding-ep) list-reg-holding-ep)
              ((eq? message 'get-list-reg-dealing-with-stack) list-reg-dealing-with-stack)
              ((eq? message 'display-tbl-reg-and-assigned-data) (display-table tbl-reg-and-assigned-data "Register:~a, values:~a\n"))
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
        (ops (machine 'operations))
        (list-all-insts '())
        (list-reg-holding-ep '())
        (list-reg-dealing-with-stack '())
        (tbl-reg-and-assigned-data (make-appendable-table)))
    (for-each
     (lambda (inst)
       (let ((the-exec-proc (make-execution-procedure
                             (instruction-text inst) 
                             labels
                             machine
                             pc
                             flag
                             stack
                             ops)))
         (if (exec-proc-list? the-exec-proc)
             (begin
               (cond ((eq? (exec-proc-list-tag the-exec-proc) 'with-reg-holding-ep)
                      (set!
                       list-reg-holding-ep
                       (add-list-with-no-duplicated list-reg-holding-ep (exec-proc-list-appendix the-exec-proc))))
                     ((eq? (exec-proc-list-tag the-exec-proc) 'with-reg-dealing-with-stack)
                      (set!
                       list-reg-dealing-with-stack
                       (add-list-with-no-duplicated list-reg-dealing-with-stack (exec-proc-list-appendix the-exec-proc))))
                     ((eq? (exec-proc-list-tag the-exec-proc) 'with-reg-and-assigned-data)
                      (let ((reg-and-assigned (exec-proc-list-appendix the-exec-proc)))
                        (insert! tbl-reg-and-assigned-data (car reg-and-assigned) (cdr reg-and-assigned))))
                     (else
                      (error "Undefined execution procedure list's tag name ASSEMBLE" 
                             (exec-proc-list-tag the-exec-proc))))
               (set-instruction-execution-proc! inst (exec-proc-list-proc the-exec-proc)))
             (set-instruction-execution-proc! inst the-exec-proc))
         (set! list-all-insts (add-list-with-no-duplicated list-all-insts (instruction-text inst)))))
     insts)
    ((machine 'install-list-all-insts) list-all-insts)
    ((machine 'install-list-reg-holding-ep) list-reg-holding-ep)
    ((machine 'install-list-reg-dealing-with-stack) list-reg-dealing-with-stack)
    ((machine 'install-tbl-reg-and-assigned-data) tbl-reg-and-assigned-data)))

(define (make-instruction text)
  (mcons text '()))
(define (instruction-text inst) (mcar inst))
(define (instruction-execution-proc inst)
  (mcdr inst))
(define (set-instruction-execution-proc!
         inst
         proc)
  (set-mcdr! inst proc))

(define (make-exec-proc-list tag appendix proc) (list tag appendix proc))
(define (exec-proc-list? tgt-exec-proc) (list? tgt-exec-proc))
(define (exec-proc-list-tag exec-proc-list) (car exec-proc-list))
(define (exec-proc-list-appendix exec-proc-list) (cadr exec-proc-list))
(define (exec-proc-list-proc exec-proc-list) (caddr exec-proc-list))
         
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
      (make-exec-proc-list
       'with-reg-and-assigned-data
       (list (assign-reg-name inst) value-exp)
       (lambda ()   ; execution procedure
         ; for assign
         (set-contents! target (value-proc))
         (advance-pc pc))))))

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
             (make-exec-proc-list
              'with-reg-holding-ep
              (register-exp-reg dest)
              (lambda ()
                (set-contents! 
                 pc
                 (get-contents reg))))))
          (else (error "Bad GOTO instruction: 
                        ASSEMBLE"
                       inst)))))

(define (goto-dest goto-instruction)
  (cadr goto-instruction))

(define (make-save inst machine stack pc)
  (let ((reg (get-register 
              machine
              (stack-inst-reg-name inst))))
    (make-exec-proc-list
     'with-reg-dealing-with-stack
     (stack-inst-reg-name inst)
     (lambda ()
       (push stack (get-contents reg))
       (advance-pc pc)))))

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
;    ;;;;; ;;;  ;;; ;;;;;;    ;;;   ;;;;;   ;;; ;;;   ;;    ;;;;;;;  ;;;;;    ;;;  ;;;  ;;;         ;;;;;;    ;;;   ;;;;;           ;;;;      ;;    ;;;;;;;   ;;    ;;;;;     ;;    ;;;;;;; ;;; ;;;
;      ;    ;;   ;   ;   ;   ;   ;   ;   ;   ;; ;;     ;    ;  ;  ;    ;     ;   ;  ;;   ;           ;   ;   ;   ;   ;   ;           ;  ;      ;    ;  ;  ;    ;     ;   ;     ;    ;  ;  ;  ;   ; 
;      ;    ; ;  ;   ; ;    ;     ;  ;   ;   ;; ;;    ; ;      ;       ;    ;     ; ; ;  ;           ; ;    ;     ;  ;   ;           ;   ;    ; ;      ;      ; ;    ;   ;    ; ;      ;     ;   ; 
;      ;    ; ;  ;   ;;;    ;     ;  ;   ;   ; ; ;    ; ;      ;       ;    ;     ; ; ;  ;           ;;;    ;     ;  ;   ;           ;   ;    ; ;      ;      ; ;    ;   ;    ; ;      ;     ;;;;; 
;      ;    ;  ; ;   ; ;    ;     ;  ;;;;    ; ; ;    ; ;      ;       ;    ;     ; ;  ; ;           ; ;    ;     ;  ;;;;            ;   ;    ; ;      ;      ; ;    ;;;;     ; ;      ;     ;   ; 
;      ;    ;  ; ;   ;      ;     ;  ;  ;    ;   ;    ;;;      ;       ;    ;     ; ;  ; ;           ;      ;     ;  ;  ;            ;   ;    ;;;      ;      ;;;    ;        ;;;      ;     ;   ; 
;      ;    ;   ;;   ;       ;   ;   ;   ;   ;   ;   ;   ;     ;       ;     ;   ;  ;   ;;           ;       ;   ;   ;   ;           ;  ;    ;   ;     ;     ;   ;   ;       ;   ;     ;     ;   ; 
;    ;;;;; ;;;  ;;  ;;;       ;;;   ;;;   ; ;;; ;;; ;;; ;;;   ;;;    ;;;;;    ;;;  ;;;  ;;          ;;;       ;;;   ;;;   ;         ;;;;    ;;; ;;;   ;;;   ;;; ;;; ;;;     ;;; ;;;   ;;;   ;;; ;;;
;                                                                                                                                                                                                  
;                                                                                                                                                                                                  
;                                                                                                                                                                                                  
;                                                                                                                                                                                                  

(define (add-list-with-no-duplicated the-list entry)
  (if (member entry the-list)
      the-list
      (cons entry the-list)))


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


(define fact-machine
  (make-machine
   '(continue n val)
   (list (list '- -) (list '* *) (list '= =))
   '((assign continue (label fact-done))    
     fact-loop
     (test (op =) (reg n) (const 1))
     (branch (label base-case))
     (save continue)
     (save n)
     (assign n (op -) (reg n) (const 1))
     (assign continue (label after-fact))
     (goto (label fact-loop))
     after-fact
     (restore n)
     (restore continue)
     (assign val (op *) (reg n) (reg val))   ; val now contains n(n - 1)!
     (goto (reg continue))                   ; return to caller
     base-case
     (assign val (const 1))                  ; base case: 1! = 1
     (goto (reg continue))                   ; return to caller
     fact-done)))

(define fib-machine
  (make-machine
   '(continue n val)
   (list (list '- -) (list '+ +) (list '< <))
   '((assign continue (label fib-done))
     fib-loop
     (test (op <) (reg n) (const 2))
     (branch (label immediate-answer))
     ;; set up to compute Fib(n - 1)
     (save continue)
     (assign continue (label afterfib-n-1))
     (save n)           ; save old value of n
     (assign n 
             (op -)
             (reg n)
             (const 1)) ; clobber n to n-1
     (goto 
      (label fib-loop)) ; perform recursive call
     afterfib-n-1 ; upon return, val contains Fib(n - 1)
     (restore n)
     (restore continue)
     ;; set up to compute Fib(n - 2)
     (assign n (op -) (reg n) (const 2))
     (save continue)
     (assign continue (label afterfib-n-2))
     (save val)         ; save Fib(n - 1)
     (goto (label fib-loop))
     afterfib-n-2 ; upon return, val contains Fib(n - 2)
     (assign n 
             (reg val)) ; n now contains Fib(n - 2)
     (restore val)      ; val now contains Fib(n - 1)
     (restore continue)
     (assign val        ; Fib(n - 1) + Fib(n - 2)
             (op +) 
             (reg val)
             (reg n))
     (goto              ; return to caller,
      (reg continue))   ; answer is in val
     immediate-answer
     (assign val 
             (reg n))   ; base case: Fib(n) = n
     (goto (reg continue))
     fib-done)))
   

;;; get the infomation
(fact-machine 'get-list-all-insts)
(fact-machine 'get-list-reg-holding-ep)
(fact-machine 'get-list-reg-dealing-with-stack)
(fact-machine 'display-tbl-reg-and-assigned-data)
(println "--------------")
(fib-machine 'get-list-all-insts)
(fib-machine 'get-list-reg-holding-ep)
(fib-machine 'get-list-reg-dealing-with-stack)
(fib-machine 'display-tbl-reg-and-assigned-data)
(println "--------------")

;;; check if compute still works
(set-register-contents! fact-machine 'n 6)
(start fact-machine)
(get-register-contents fact-machine 'val)
(println "--------------")
(set-register-contents! fib-machine 'n 10)
(start fib-machine)
(get-register-contents fib-machine 'val)
