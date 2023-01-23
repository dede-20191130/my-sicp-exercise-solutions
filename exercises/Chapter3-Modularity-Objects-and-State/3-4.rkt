#lang racket

(define (make-account balance pw)
  (let ((failed-count 0))
    (define (reset-failed) (set! failed-count 0))
    (define (announce-pw-check-failed . someargs)
      (set! failed-count (add1 failed-count))
      (if (>= failed-count 7)
          (call-the-cops)
          "Incorrect password"))
    (define (call-the-cops)
      "Cops have been called.")
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (define (dispatch input-pw m)
      (if (not (eq? input-pw pw))
          announce-pw-check-failed
          (begin
            (reset-failed)
            (cond ((eq? m 'withdraw) withdraw)
                  ((eq? m 'deposit)  deposit)
                  (else (error "Unknown request -- MAKE-ACCOUNT"
                               m))))))
    dispatch))


(define acc (make-account 100 'secret-password))

((acc 'secret-password 'withdraw) 40)
;60

((acc 'some-other-password 'deposit) 50)
;"Incorrect password"

((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'secret-password 'withdraw) 40) ;safe because the times pw is incorrect is less than 7
((acc 'some-other-password 'deposit) 3)
((acc 'some-other-password 'deposit) 3)
((acc 'some-other-password 'deposit) 3)
((acc 'some-other-password 'deposit) 3)
((acc 'some-other-password 'deposit) 3)
((acc 'some-other-password 'deposit) 3)
((acc 'some-other-password 'deposit) 3);danger
((acc 'secret-password 'deposit) 3)
((acc 'some-other-password 'deposit) 1)