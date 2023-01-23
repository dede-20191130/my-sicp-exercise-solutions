#lang racket

(define (make-wire) 'created-wire)

(define (get-signal wire)
  (println "mocked get-signal")
  0)

(define (set-signal! new-v wire)
  (println "mocked set-signal!"))

(define (add-action! wire proc)
  (println "mocked add-action!"))

(define (after-delay delay proc)
  (println "mocked after-delay"))

(define inverter-delay 0)
(define and-gate-delay 0)

(define (inverter input output)
  (define (invert-input)
    (let ((new-value 
           (logical-not (get-signal input))))
      (after-delay 
       inverter-delay
       (lambda ()
         (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)

(define (logical-and bit1 bit2)
  (println "mocked logical-and")
  0)

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
           (logical-and (get-signal a1) 
                        (get-signal a2))))
      (after-delay 
       and-gate-delay
       (lambda ()
         (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))

(define (logical-or bit1 bit2)
  (cond ((and (= bit1 0) (= bit1 0)) 0)
        ((or
          (and (= bit1 0) (= bit1 1))
          (and (= bit1 1) (= bit1 0))
          (and (= bit1 1) (= bit1 1))) 1)
        (else (error "Invalid signal" (list bit1 bit2)))))

(define (or-gate a1 a2 output)
  (let ((b1 (make-wire))
        (b2 (make-wire))
        (c1 (make-wire)))
    (inverter a1 b1)
    (inverter a2 b2)
    (and-gate b1 b2 c1)
    (inverter c1 output)
    'ok))

(or-gate 'wire1 'wire2 'wire3)