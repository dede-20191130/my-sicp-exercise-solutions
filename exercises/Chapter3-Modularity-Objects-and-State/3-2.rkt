#lang racket

(define (make-monitored func)
  (let ((counter 0))
    (lambda (message . rest)
      (cond ((eq? message 'how-many-calls?) counter)
            ((eq? message 'reset-count) (set! counter 0))
            (else (begin
                    (set! counter (add1 counter))
                    (apply func message rest)))))))

(define s (make-monitored *))

(s 100 34 5)
;10

(s 'how-many-calls?)
;1

(s 27 3)
(s 9095 99 6 7 1)
(s 'how-many-calls?)
(s 'reset-count)
(s 'how-many-calls?)