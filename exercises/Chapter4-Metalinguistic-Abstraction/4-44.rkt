#lang sicp

(define nil '())

(define ($$require p)
  (if (not p) (amb)))

(define (distinct? items)
  (cond ((null? items) true)
        ((null? (cdr items)) true)
        ((member (car items) (cdr items)) false)
        (else (distinct? (cdr items)))))

(define empty-board nil)

(define (adjoin-position new-col-pos prev-poses)
  (append prev-poses (list new-col-pos)))

(define (row-check new-pos poses)
  (cond ((null? poses) #t)
        ((= (caar poses) (car new-pos)) #f)
        (else (row-check new-pos (cdr poses)))))

(define (top-left-diag-check new-pos poses)
  (define row-col-sum (+ (car new-pos) (cadr new-pos)))
  (cond ((null? poses) #t)
        ((= (+ (caar poses) (cadar poses)) row-col-sum) #f)
        (else (top-left-diag-check new-pos (cdr poses)))))

(define (bottom-left-diag-check new-pos poses)
  (define row-col-diff (- (car new-pos) (cadr new-pos)))
  (cond ((null? poses) #t)
        ((= (- (caar poses) (cadar poses)) row-col-diff) #f)
        (else (bottom-left-diag-check new-pos (cdr poses)))))

(define (safe? new-col-pos prev-poses)
  (and (row-check new-col-pos prev-poses) 
       (top-left-diag-check new-col-pos prev-poses) 
       (bottom-left-diag-check new-col-pos prev-poses)))

(define (queens board-size)
  (define (new-col-pos col)
    (let go ((row 1))
      (if (> row board-size)
          (amb)
          (amb (list row col)
               (go (+ row 1))))))
  (let gen-loop ((curr-rowindex 1)
                 (prev-poses empty-board))
    (if (> curr-rowindex board-size)
        prev-poses
        (let ((ncp (new-col-pos curr-rowindex)))
          ($$require (safe? ncp prev-poses))
          (gen-loop (+ curr-rowindex 1) (adjoin-position ncp prev-poses))))))

;;;test

(queens 4)
(amb)
;(amb)
  
  
(queens 8)
(amb)
(amb)
(amb)

