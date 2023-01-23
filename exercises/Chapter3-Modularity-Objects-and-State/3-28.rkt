#lang racket

(define (get-signal wire)
  (println "mocked get-signal")
  0)

(define (set-signal! new-v wire)
  (println "mocked set-signal!"))

(define (add-action! wire proc)
  (println "mocked add-action!"))

(define (after-delay delay proc)
  (println "mocked after-delay"))

(define or-gate-delay 0)

(define (logical-or bit1 bit2)
  (cond ((and (= bit1 0) (= bit1 0)) 0)
        ((or
          (and (= bit1 0) (= bit1 1))
          (and (= bit1 1) (= bit1 0))
          (and (= bit1 1) (= bit1 1))) 1)
        (else (error "Invalid signal" (list bit1 bit2)))))

(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value
           (logical-or (get-signal a1) 
                       (get-signal a2))))
      (after-delay 
       or-gate-delay
       (lambda ()
         (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)

(or-gate 'wire1 'wire2 'wire3)