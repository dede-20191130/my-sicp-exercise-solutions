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

(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (full-adder a b c-in sum c-out)
  (let ((c1 (make-wire)) 
        (c2 (make-wire))
        (s  (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

(define (ripple-carry-adder a-wrs b-wrs s-wrs c)
  (define (iter curr-a-wrs curr-b-wrs curr-s-wrs curr-c)
    (if (= (length curr-a-wrs) 1)
        (full-adder (car curr-a-wrs) (car curr-b-wrs) curr-c (car curr-s-wrs) c)
        (let ((next-c (make-wire)))
          (full-adder (car curr-a-wrs) (car curr-b-wrs) curr-c (car curr-s-wrs) next-c)
          (iter (cdr curr-a-wrs) (cdr curr-b-wrs) (cdr curr-s-wrs) next-c))))
  (iter a-wrs b-wrs s-wrs 0)
  'ok)

(ripple-carry-adder '(a1 a2 a3 a4) '(b1 b2 b3 b4) '(s1 s2 s3 s4) (make-wire))