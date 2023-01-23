#lang racket

;;; suppose the op syntax changes:
;;; before (op [name]) operand1 operand2 ...
;;; after  (op [name] operand1 operand2 ...)

;;; new operational syntaxes
'(define (operation-exp-operands operation-exp)
  (cddar operation-exp))