#lang racket

'(define (f x)
   (+ x (g (+ x 2))))