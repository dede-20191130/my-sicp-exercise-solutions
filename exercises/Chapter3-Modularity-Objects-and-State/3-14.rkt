#lang racket
(require compatibility/mlist)

(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (mcdr x)))
          (set-mcdr! x y)
          (loop temp x))))
  (loop x '()))

(define v (mlist 'a 'b 'c 'd))
v

(define w (mystery v))
v
w

