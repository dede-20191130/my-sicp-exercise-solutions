#lang racket

;;; define exponentiation machine
'(define expt-machine
   (make-machine
    '(n b cnt prod)
    (list
     (list '= =)
     (list '- -)
     (list '* *))
    '(test-b
      (assign prod (const 1))
      (assign cnt (reg n))
      expt-loop
      (test (op =) (cnt n) (const 0))
      (branch expt-done)
      (assign prod (op *) (reg prod) (reg b))
      (assign cnt (op -) (reg cnt) (const 1))
      (goto (label expt-loop))
      expt-done)))

;;; initiate
'(set-register-contents! expt-machine 'n 10)
'done

'(set-register-contents! expt-machine 'b 2)
'done

;;; calculate
'(start gcd-machine)
'done

;;; return the result
'(get-register-contents gcd-machine 'prod)
'1024