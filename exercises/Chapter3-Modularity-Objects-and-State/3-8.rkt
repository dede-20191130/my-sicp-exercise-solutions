#lang racket

(define stored 0)

(define (f val)
  (let ((curr stored))
    (set! stored val)
    curr))

(+ (f 0) (f 1))