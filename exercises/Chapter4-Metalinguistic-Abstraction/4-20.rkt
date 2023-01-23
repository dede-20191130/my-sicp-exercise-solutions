#lang racket
(require compatibility/mlist)
(require racket/exn)

(define sym-not-yel-assined '*unassigned*)

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (make-let decs body)
  (cons 'let (cons decs body)))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (definition? exp)
  (tagged-list? exp 'define))

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-frame variables values)
  (define (iter vars vals)
    (if (null? vars)
        (mlist)
        (mcons (mcons (car vars)
                      (car vals))
               (iter (cdr vars)
                     (cdr vals)))))
  (mcons '*frame* (iter variables values)))

(define (frame-pairs frame) (mcdr frame))
(define (frame-first-pair tgt-frame-pairs) (mcar tgt-frame-pairs))
(define (frame-rest-pairs tgt-frame-pairs) (mcdr tgt-frame-pairs))
(define (frame-pair-var tgt-pair) (mcar tgt-pair))
(define (frame-pair-val tgt-pair) (mcdr tgt-pair))
(define (change-frame-pair-val! tgt-pair val) (set-mcdr! tgt-pair val))
(define (add-binding-to-frame! var val frame)
  (set-mcdr! frame (mcons (mcons var val) (frame-pairs frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" 
                 vars 
                 vals)
          (error "Too few arguments supplied" 
                 vars 
                 vals))))



(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan tgt-frame-pairs)
      (if (null? tgt-frame-pairs)
          (env-loop (enclosing-environment env))
          (let ((first-pair (frame-first-pair tgt-frame-pairs)))
            (if (eq? var (frame-pair-var first-pair))
                (begin
                  (let ((pulled-val (frame-pair-val first-pair)))
                    (if (eq? pulled-val sym-not-yel-assined)
                        (error "SPECIFIED VARIABLE'S VALUE IS NOT YET ASSIGNED." var)
                        pulled-val)))
                (scan (frame-rest-pairs tgt-frame-pairs))))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (scan (frame-pairs (first-frame env)))))
  (env-loop env))




(define (letrec-var-declarations exp) (cadr exp))
(define (letrec-body exp) (caddr exp))
(define (letrec->combination exp)
  (let ((var-declarations (letrec-var-declarations exp))
        (body (letrec-body exp)))
    (make-let (map (λ(var-dec) (list (car var-dec) (quote '*unassigned*))) var-declarations)
              (append
               (map (λ(var-dec) (list 'set! (car var-dec) (cadr var-dec))) var-declarations)
               (list body)))))
    
      

;;;test 


(define my-eval
  (let ((ns (make-base-namespace)))
    (lambda (expr) (eval expr ns))))

(define evaled-letrec-exps (my-eval (letrec->combination
                                     '(letrec
                                          ((fact
                                            (lambda (n)
                                              (if (= n 1)
                                                  1
                                                  (* n (fact (- n 1)))))))
                                        (fact 10)))))

evaled-letrec-exps

(define evaled-proc1 (my-eval (make-lambda '(x)
                                           (list (letrec->combination
                                                  '(letrec
                                                       ((even?
                                                         (lambda (n)
                                                           (if (= n 0)
                                                               #t
                                                               (odd? (- n 1)))))
                                                        (odd?
                                                         (lambda (n)
                                                           (if (= n 0)
                                                               #f
                                                               (even? (- n 1))))))
                                                     (if (even? x)
                                                         'e
                                                         'o)))))))

(evaled-proc1 5)
(evaled-proc1 6)