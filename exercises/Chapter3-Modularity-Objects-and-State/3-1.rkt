#lang racket

(define (make-accumulator initial)
  (lambda (addend)
    (set! initial (+ initial addend))
    initial))

(define A (make-accumulator 5))

(A 10)

(A 10)

