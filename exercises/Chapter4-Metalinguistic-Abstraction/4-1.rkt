#lang racket

(define (list-of-values-from-left-to-right exps env)
  (define (get-operands e)
    (if (no-operands? e)
        '()
        (cons (first-operand exps)
              (get-operands 
               (rest-operands exps))))) 
             
  (let ((eval-results (list))
        (operands (get-operands exps)))
    (define (eval-loop opds)
      (if (null? opds)
          'ok
          (begin
            (set! eval-results (cons (eval (car opds) env) eval-results))
            (eval-loop (cdr opds)))))
    (eval-loop operands)
    (reverse eval-results)))


(define (list-of-values-from-right-to-left exps env)
  (define (get-operands e)
    (if (no-operands? e)
        '()
        (cons (first-operand exps)
              (get-operands 
               (rest-operands exps))))) 
             
  (let ((eval-results (list))
        (operands (get-operands exps)))
    (define (eval-loop opds)
      (if (null? opds)
          'ok
          (begin
            (set! eval-results (cons (eval (car opds) env) eval-results))
            (eval-loop (cdr opds)))))
    (eval-loop (reverse operands))
    eval-results))