#lang racket
(require compatibility/mlist)

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

(define list01 '(1 2 3))
(define list02 '((1 . 2) 3))
(define list03 '(((3) . 2) . 1))
(define subpair01 '(2 . 3))
(define list04 (cons 1 (cons subpair01 subpair01)))
(define sublist01 '(1 2))
(define list05 (cons sublist01 sublist01))
(define sublist02 (cons subpair01 subpair01))
(define list06 (cons sublist02 sublist02))

(count-pairs list01)
(count-pairs list02)
(count-pairs list03)
(count-pairs list04)
(count-pairs list05)
(count-pairs list06)

(eq? (car (car list06)) (cadr list04))

