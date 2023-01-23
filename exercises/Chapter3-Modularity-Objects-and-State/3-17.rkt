#lang racket
(require compatibility/mlist)

(define (print-line value) 
  (display value) 
  (newline))

(define (count-distinct-pairs x)
  (let ((counted-pair-list (list)))
    (define (helper tgt-list)
      (if (or
           (not (pair? tgt-list))
           (memq tgt-list counted-pair-list))
          0
          (begin
            (set! counted-pair-list (append counted-pair-list (list tgt-list)))
            (+ (helper (car tgt-list))
               (helper (cdr tgt-list))
               1))))
    (helper x)))

(define (count-distinct-pairs-mutable x)
  (let ((counted-pair-mlist (mlist)))
    (define (helper tgt-mlist)
      (if (or
           (not (mpair? tgt-mlist))
           (mmemq tgt-mlist counted-pair-mlist))
          0
          (begin
            (if (null? counted-pair-mlist);;method (ii)
                (set! counted-pair-mlist  (mlist tgt-mlist))
                (mappend! counted-pair-mlist (mlist tgt-mlist)))
            (+ (helper (mcar tgt-mlist))
               (helper (mcdr tgt-mlist))
               1))))
    (helper x)))

(define list01 '(1 2 3))
(define list02 '((1 . 2) 3))
(define list03 '(((3) . 2) . 1))
(define subpair01 '(2 . 3))
(define list04 (cons 1 (cons subpair01 subpair01)))
(define sublist01 '(1 2))
(define list05 (cons sublist01 sublist01))
(define sublist02 (cons subpair01 subpair01))
(define list06 (cons sublist02 sublist02))
(define mlist01 (mlist 1 2 3))
(set-mcdr! (mcdr (mcdr mlist01)) mlist01)


(count-distinct-pairs list01)
(count-distinct-pairs list02)
(count-distinct-pairs list03)
(count-distinct-pairs list04)
(count-distinct-pairs list05)
(count-distinct-pairs list06)

;(count-distinct-pairs-mutable (list->mlist list01))
;(count-distinct-pairs-mutable (list->mlist list02))
;(count-distinct-pairs-mutable (list->mlist list03))
;(count-distinct-pairs-mutable (list->mlist list04))
;(count-distinct-pairs-mutable (list->mlist list05))
;(count-distinct-pairs-mutable (list->mlist list06))
(count-distinct-pairs-mutable  mlist01)