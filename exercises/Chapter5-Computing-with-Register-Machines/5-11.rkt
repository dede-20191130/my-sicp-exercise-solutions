#lang racket

;question 2
'(define (make-save inst machine stack pc)
   (let ((reg (get-register 
               machine
               (stack-inst-reg-name inst))))
     (lambda ()
       (push stack (make-stacked-pair
                    (stack-inst-reg-name inst)
                    (get-contents reg)))
       (advance-pc pc))))

'(define (make-restore inst machine stack pc)
   (let ((reg (get-register
               machine
               (stack-inst-reg-name inst))))
     (lambda ()
       (let ((stacked-pair (pop stack)))
         (if (eq?
              (stacked-pair-name stacked-pair)
              (stack-inst-reg-name inst))
             (begin
               (set-contents! reg (stacked-pair-value stacked-pair))
               (advance-pc pc))
             (error "The restored value was not saved from the register" (stack-inst-reg-name inst)))))))
    
'(define (make-stacked-pair reg-name reg-val)
   (cons reg-name reg-val))
'(define (stacked-pair-name sp) (car sp))
'(define (stacked-pair-value sp) (cdr sp))

; question 3
'(define (make-machine register-names 
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
     ((machine 'initialize-stack)) ;;; ***
     machine))

'(define (make-new-machine)
   (let ((pc (make-register 'pc))
         (flag (make-register 'flag))
         (stack '()) ;;; ***
         (the-instruction-sequence '()))
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
       (define (initialize-stack);;; ***
         (set!
          stack
          (map
           (lambda(reg-name)
             (list reg-name (make-stack)))
           stack)))
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
               ((eq? message 'initialize-stack) 
                initialize-stack)
               (else (error "Unknown request: 
                            MACHINE"
                            message))))
       dispatch)))

'(define (make-execution-procedure 
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
          (add-raw-stack! stack (stack-inst-reg-name inst)) ;;; ***
          (make-save inst machine stack pc))
         ((eq? (car inst) 'restore)
          (make-restore inst machine stack pc))
         ((eq? (car inst) 'perform)
          (make-perform
           inst machine labels ops pc))
         (else (error "Unknown instruction 
                      type: ASSEMBLE"
                      inst))))

'(define (add-raw-stack! stack reg-name)
   (if (memq reg-name stack)
       'ignored
       (set! stack (cons reg-name stack))))

'(define (make-save inst machine stack pc)
   (let ((reg (get-register 
               machine
               (stack-inst-reg-name inst))))
     (lambda ()
       (let ((the-reg-stack-entry (assoc (stack-inst-reg-name inst) stack)))
         (if the-reg-stack-entry
             (begin
               (push (stack-entry-stack the-reg-stack-entry) (get-contents reg))
               (advance-pc pc))
             (error "Unregistered name required" (stack-inst-reg-name inst)))))))

'(define (make-restore inst machine stack pc)
   (let ((reg (get-register
               machine
               (stack-inst-reg-name inst))))
     (lambda ()
       (let ((the-reg-stack-entry (assoc (stack-inst-reg-name inst) stack)))
         (if the-reg-stack-entry
             (begin
               (set-contents! reg (pop (stack-entry-stack the-reg-stack-entry)))
               (advance-pc pc))
             (error "Unregistered name required" (stack-inst-reg-name inst)))))))

'(define (stack-entry-stack stack-entry) (cadr stack-entry))