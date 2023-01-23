#lang racket

(define (make-account balance pw)
  (let ((pw-list (list pw))
        (sfj 'secret-for-joint))
    (define (announce-pw-check-failed . someargs)
      "Incorrect password")
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
  
    (define (dispatch input-pw m)
      (cond ((not (memq input-pw (cons sfj pw-list))) announce-pw-check-failed)
            ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit)  deposit)
            ((eq? m 'make-joint)
             (lambda (new-pw)
               (set! pw-list (cons new-pw pw-list))
               dispatch))
            (else (error "Unknown request -- MAKE-ACCOUNT"
                         m))))
    dispatch))

(define (make-joint tgt-acc joint-pw new-pw)
  ((tgt-acc joint-pw 'make-joint) new-pw))


(define peter-acc (make-account 100 'secret-password))

((peter-acc 'secret-password 'withdraw) 40)
;60

((peter-acc 'some-other-password 'deposit) 50)
;"Incorrect password"

(define paul-acc
  (make-joint peter-acc 'secret-for-joint 'rosebud))

((paul-acc 'rosebud 'withdraw) 7)
((peter-acc 'secret-password 'deposit) 1000)
((paul-acc 'rosebud 'withdraw) 7)
((peter-acc 'secret-password 'deposit) 1000)