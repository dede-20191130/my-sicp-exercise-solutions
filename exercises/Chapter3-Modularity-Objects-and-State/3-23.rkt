#lang racket
(require compatibility/mlist)

(define (print-line value) 
  (display value) 
  (newline))

(define (front-ptr deque) (mcar deque))
(define (rear-ptr deque) (mcdr deque))
(define (set-front-ptr! deque item) (set-mcar! deque item))
(define (set-rear-ptr! deque item) (set-mcdr! deque item))
(define (empty-deque? deque) (null? (front-ptr deque)))
(define (make-deque) (mcons '() '()))

(define (make-new-mpair item) (mcons item (mcons '() '())))
(define (item-part deq-unit) (mcar deq-unit))
(define (ref-part deq-unit) (mcdr deq-unit))
(define (front-ref tgt-ref-part) (mcar tgt-ref-part))
(define (rear-ref tgt-ref-part) (mcdr tgt-ref-part))
(define (set-front-ref! tgt-ref-part deq-unit) (set-mcar! tgt-ref-part deq-unit))
(define (set-rear-ref! tgt-ref-part deq-unit) (set-mcdr! tgt-ref-part deq-unit))

(define (front-deque deque)
  (if (empty-deque? deque)
      (error "FRONT called with an empty deque" deque)
      (mcar (front-ptr deque))))
(define (front-insert-deque! deque item)
  (let ((new-mpair (make-new-mpair item)))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-mpair)
           (set-rear-ptr! deque new-mpair)
           deque)
          (else
           (set-rear-ref! (ref-part new-mpair) (front-ptr deque))
           (set-front-ref! (ref-part (front-ptr deque)) new-mpair)
           (set-front-ptr! deque new-mpair)
           deque)))) 
(define (rear-insert-deque! deque item)
  (let ((new-mpair (make-new-mpair item)))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-mpair)
           (set-rear-ptr! deque new-mpair)
           deque)
          (else
           (set-rear-ref! (ref-part (rear-ptr deque)) new-mpair)
           (set-front-ref! (ref-part new-mpair) (rear-ptr deque))
           (set-rear-ptr! deque new-mpair)
           deque)))) 
(define (front-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "DELETE! called with an empty deque" deque))
        (else
         (cond ((eq? (front-ptr deque) (rear-ptr deque))
                (set-front-ptr! deque '())
                (set-rear-ptr! deque '()))
               (else
                (set-front-ptr! deque (rear-ref (ref-part (front-ptr deque))))))
         deque)))
(define (rear-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "DELETE! called with an empty deque" deque))
        (else
         (cond ((eq? (front-ptr deque) (rear-ptr deque))
                (set-rear-ptr! deque '())
                (set-front-ptr! deque '()))
                
               (else
                (set-rear-ptr! deque (front-ref (ref-part (rear-ptr deque))))))
         deque))) 
(define (print-deque deque)
  (define (iter tgt-deq-unit)
    (cond ((null? tgt-deq-unit) (display ")"))
          ((eq? tgt-deq-unit (rear-ptr deque))
           (display (item-part tgt-deq-unit))
           (display ")"))
          (else (begin
                  (display (item-part tgt-deq-unit))
                  (display ", ")
                  (iter (rear-ref (ref-part tgt-deq-unit)))))))
  (display "(")
  (iter (front-ptr deque))
  (newline))
 


(define q1 (make-deque))
(print-deque q1)
(print-line "-------------------")
(front-insert-deque! q1 'front-a)
(print-deque q1)
(print-line "-------------------")
(front-insert-deque! q1 'front-b)
(print-deque q1)
(print-line "-------------------")
(rear-insert-deque! q1 'rear-a)
(print-deque q1)
(print-line "-------------------")
(rear-insert-deque! q1 'rear-b)
(print-deque q1)
(print-line "-------------------")
(front-delete-deque! q1)
(print-deque q1)
(front-delete-deque! q1)
(print-deque q1)
(front-delete-deque! q1)
(print-deque q1)
(front-delete-deque! q1)
(print-deque q1)
(print-line "-------------------")

(rear-insert-deque! q1 'rear-1)
(print-deque q1)
(print-line "-------------------")
(rear-insert-deque! q1 'rear-2)
(print-deque q1)
(print-line "-------------------")
(front-delete-deque! q1)
(print-deque q1)
(print-line "-------------------")
(rear-insert-deque! q1 'rear-3)
(print-deque q1)
(print-line "-------------------")
(rear-insert-deque! q1 'rear-4)
(print-deque q1)
(print-line "-------------------")
(front-delete-deque! q1)
(print-deque q1)
(print-line "-------------------")
(rear-insert-deque! q1 'rear-5)
(print-deque q1)
(print-line "-------------------")
(rear-insert-deque! q1 'rear-6)
(print-deque q1)
(print-line "-------------------")
(rear-delete-deque! q1)
(print-deque q1)
(rear-delete-deque! q1)
(print-deque q1)
(rear-delete-deque! q1)
(print-deque q1)
(rear-delete-deque! q1)
(print-deque q1)
(print-line "-------------------")
