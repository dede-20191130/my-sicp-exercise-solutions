#lang racket

;;; as primitive
'(controller
  (assign g (const 1))
  test-ge
  (test (op good-enough) (reg g) (reg x) (const 0.001))
  (branch (label ge-done))
  (assign g (op improve) (reg g) (reg x))
  (goto (label test-ge))
  ge-done)

;;; extended
'(controller
  (assign g (const 1))
  test-ge
  (assign t1 (op square) (reg g))
  (assign t2 (op -) (reg t1) (reg x))
  (assign t3 (op abs) (reg t2))
  (test (op <) (reg t3) (const 0.001))
  (branch (label ge-done))
  (assign t4 (op /) (reg x) (reg g))
  (assign g (op average) (reg g) (reg t4))
  (goto (label test-ge))
  ge-done)