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
  (define (point-start-obj? tgt-mlst start-obj count)
    (cond ((not (mpair? tgt-mlst)) (list true false))
          ((= count 0) (list false false))
          ((eq? tgt-mlst start-obj) (list false true))
          (else (point-start-obj? (mcdr tgt-mlst) start-obj (sub1 count)))))
        
  (define (starts-cycle? curr-mlst curr-idx)
    (if (not (mpair? curr-mlst))
        false
        (let ((result-check-pso (point-start-obj? (mcdr curr-mlst) curr-mlst curr-idx)))
          (let ((reached-last?? (car result-check-pso))
                (point?? (cadr result-check-pso)))
            (cond (reached-last?? false)
                  (point?? true)
                  (else (starts-cycle? (mcdr curr-mlst) (add1 curr-idx))))))))
  (define (check-nth-car curr-idx full-lgth)
    (cond ((> curr-idx full-lgth) false)
          ((contains-cycle? (mlist-ref mlst (sub1 curr-idx))) true)
          (else (check-nth-car (add1 curr-idx) full-lgth))))
        
  (if (starts-cycle? mlst 1)
      true
      (let (
            (mlst-length
             (cond ((mlist? mlst) (mlength mlst))
                   ((mpair? mlst) 1)
                   (else 0))))
        (check-nth-car 1 mlst-length))))
      
      
  

(define ml01 (mlist 1 2 3))
(define ml02 (mlist (mlist 1 2 (mlist 10 11)) 20 21 (mlist 30 31)))
(define mlccl01 (make-cycle (mlist 1 2 3)))
(define mlccl04 (mcons 20 (mcons 21 (make-cycle (mlist 1 2 3)))))
(define mlccl02 (mcons (mappend (mlist 10 11) (make-cycle (mlist 1 2 3))) (mlist 20 21)))
(define mlccl03 (mlist (mlist 1 2 (mlist 10 11 12 13 14)) 20 21 (mlist 30 31)))
(define part-of-mlccl03 (mcar (mcdr (mcdr (mcar mlccl03)))))
(set-mcdr! (last-mpair part-of-mlccl03) (mcdr part-of-mlccl03))

(contains-cycle? ml01)
(contains-cycle? ml02)
(contains-cycle? mlccl01)
(contains-cycle? mlccl02)
(contains-cycle? mlccl03)
(contains-cycle? mlccl04)

