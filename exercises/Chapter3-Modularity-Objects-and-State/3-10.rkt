#lang racket

(define (make-withdraw initial-amount)
  (let ((balance initial-amount))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))))

(define (make-withdraw-attached-initial initial-amount)
  (let ((balance initial-amount))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 (list balance initial-amount))
          "Insufficient funds"))))


(define W1 (make-withdraw 100))

(W1 7)
(W1 11)

(define W2 (make-withdraw 100))

(W2 13)
(W2 17)

(define WA1 (make-withdraw-attached-initial 500))
(define WA2 (make-withdraw-attached-initial 1200))

(WA1 16)
(WA1 32)
(WA2 128)
(WA2 256)