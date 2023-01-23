#lang racket

'(define (make-operation-exp
          exp machine labels operations)
   (let ((op (lookup-prim 
              (operation-exp-op exp)
              operations))
         (aprocs
          (map (lambda (e)
                 (if (label-exp? e)
                     (error "Operation should not include label operands: 
                      ASSEMBLE")
                     (make-primitive-exp 
                      e machine labels)))
               (operation-exp-operands exp))))
     (lambda () (apply op (map (lambda (p) (p))
                               aprocs)))))