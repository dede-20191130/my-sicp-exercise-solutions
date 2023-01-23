#lang racket

;;; > With the simulator as written, what will the contents of register a be when control reaches there?
'3
; lookup-label proc. searches for target label in the label list from the beginning.

;;; > Modify the extract-labels procedure so that the assembler will signal an error if the same label name is used to indicate two different locations.
'(define (extract-labels text receive)
   (if (null? text)
       (receive '() '())
       (extract-labels 
        (cdr text)
        (lambda (insts labels)
          (let ((next-inst (car text)))
            (if (symbol? next-inst)
                (if (assoc next-inst labels)
                    (error "The same label is used: ASSEMBLE" 
                           next-inst)
                    (receive 
                     insts
                     (cons 
                      (make-label-entry 
                       next-inst
                       insts)
                      labels)))
                (receive 
                 (cons (make-instruction 
                        next-inst)
                       insts)
                 labels)))))))