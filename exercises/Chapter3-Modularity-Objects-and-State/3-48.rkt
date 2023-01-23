#lang racket

(define account-serial-num 1000)
(define (make-account-and-serializer balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin 
          (set! balance (- balance amount))
          balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (set! account-serial-num (+ account-serial-num 1))
  (let ((balance-serializer 
         (make-serializer))
        (my-serial-num account-serial-num))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'balance) balance)
            ((eq? m 'serializer) 
             balance-serializer)
            ((eq? m 'serial-num) my-serial-num)
            (else (error "Unknown request: 
                          MAKE-ACCOUNT"
                         m))))
    dispatch))

(define (serialized-exchange account1 account2)
  (let ((serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer))
        (serial-num1 (account1 'serial-num))
        (serial-num2 (account2 'serial-num)))
    (cond ((> serial-num1 serial-num2)
           ((serializer1 (serializer2 exchange))
            account1
            account2))
          ((< serial-num1 serial-num2)
           ((serializer2 (serializer1 exchange))
            account1
            account2))
          (else (error "Incorrect account serial number" (list serial-num1 serial-num2))))))