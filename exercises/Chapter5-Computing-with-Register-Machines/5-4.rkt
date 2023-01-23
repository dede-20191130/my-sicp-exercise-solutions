#lang racket

;;; recursive expt.
'(controller
  (assign continue (label expt-done))
  expt-loop
  (test (op =) (reg n) (const 0))
  (branch (label base-case))
  (save continue)
  (assign n (op -) (reg n) (const 1))
  (assign continue (label after-expt))
  (goto (label expt-loop))
  after-expt
  (restore continue)
  (assign acc (op *) (reg acc) (const b))
  (goto (reg continue))
  base-case
  (assign acc (const 1))
  (goto (reg continue))
  expt-done)

;;; iterative expt.
'(controller
  (assign prod (const 1))
  (assign cnt (reg n))
  expt-loop
  (test (op =) (cnt n) (const 0))
  (branch (label expt-done))
  (assign prod (op *) (reg prod) (reg b))
  (assign cnt (op -) (reg cnt) (const 1))
  (goto (label expt-loop))
  expt-done)