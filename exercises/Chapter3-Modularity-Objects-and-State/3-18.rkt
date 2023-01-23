#lang racket
(require compatibility/mlist)

(define (print-line value) 
  (display value) 
  (newline))

(define (last-mpair x)
  (if (null? (mcdr x))
      x
      (last-mpair (mcdr x))))

(define (make-cycle x)
  (set-mcdr! (last-mpair x) x)
  x)

(define (contains-cycle? mlst)
  (define (helper tgt-mlst successive-m-items)
    (cond ((not (mpair? tgt-mlst)) false)
          ((mmemq tgt-mlst successive-m-items) true)
          (else (or
                 (helper (mcar tgt-mlst) (mlist))
                 (helper (mcdr tgt-mlst) (mappend successive-m-items (mlist tgt-mlst)))))))
  (helper mlst (mlist)))

(define ml01 (mlist 1 2 3))
(define ml02 (mlist (mlist 1 2 (mlist 10 11)) 20 21 (mlist 30 31)))
(define mlccl01 (make-cycle (mlist 1 2 3)))
(define mlccl02 (mcons (mappend (mlist 10 11) (make-cycle (mlist 1 2 3))) (mlist 20 21)))
(define mlccl03 (mlist (mlist 1 2 (mlist 10 11 12 13 14)) 20 21 (mlist 30 31)))
(define part-of-mlccl03 (mcar (mcdr (mcdr (mcar mlccl03)))))
(set-mcdr! (last-mpair part-of-mlccl03) (mcdr part-of-mlccl03))


(contains-cycle? ml01)
(contains-cycle? ml02)
(contains-cycle? mlccl01)
(contains-cycle? mlccl02)
(contains-cycle? mlccl03)