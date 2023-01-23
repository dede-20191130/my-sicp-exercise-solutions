#lang racket

; Evaluation order for operands in textbokk is right-to-left, for construct-arglist procedure for the first time reverses compiled operands code list
; and writes instruction sequences from the top of reversed list.

; The left-to-right order verions:

'(define (compile-application 
          exp target linkage)
   (let ((proc-code 
          (compile (operator exp) 'proc 'next))
         (operand-codes
          ;          (reverse ;;; modified
          (map (lambda (operand)
                 (compile operand 'val 'next))
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

'(define (construct-arglist operand-codes)
   ;;; modified
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
             (append-instruction-sequences ;;; modified
              code-to-get-last-arg ;;;
              (make-instruction-sequence ;;;
               '() ;;;
               '(argl) ;;;
               '((assign argl ;;;
                         (op reverse) ;;;
                         (reg argl))))) ;;;
             (preserving 
              '(env)
              code-to-get-last-arg
              (code-to-get-rest-args
               (cdr operand-codes)))))))

; The change above make the running of the code slow, for it's appended the instruction to reverse argment list.