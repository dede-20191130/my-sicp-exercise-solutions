#lang racket
(require compatibility/mlist)

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
                (frame-pair-val first-pair)
                (scan (frame-rest-pairs tgt-frame-pairs))))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (scan (frame-pairs (first-frame env)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan tgt-frame-pairs)
      (if (null? tgt-frame-pairs)
          (env-loop (enclosing-environment env))
          (let ((first-pair (frame-first-pair tgt-frame-pairs)))
            (if (eq? var (frame-pair-var first-pair))
                (set-mcdr! first-pair val)
                (scan (frame-rest-pairs tgt-frame-pairs))))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (scan (frame-pairs (first-frame env)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan tgt-frame-pairs)
      (if (null? tgt-frame-pairs)
          (add-binding-to-frame! var
                                 val
                                 frame)
          (let ((first-pair (frame-first-pair tgt-frame-pairs)))
            (if (eq? var (frame-pair-var first-pair))
                (set-mcdr! first-pair val)
                (scan (frame-rest-pairs tgt-frame-pairs))))))
    (scan (frame-pairs frame))))


;;;test 

(define env0 (extend-environment '(var0-1 var0-2) '(4 8) the-empty-environment))
(define env1 (extend-environment '(var1-1 var1-2) '(1 2) env0))
env1

(lookup-variable-value 'var1-2 env1)
(lookup-variable-value 'var0-1 env1)

(set-variable-value! 'var1-1 16 env1)
(set-variable-value! 'var0-2 32 env1)
env1

(define-variable! 'var1-1 64 env1)
(define-variable! 'var1-3 128 env1)
(define-variable! 'var0-1 256 env1)
env1

(lookup-variable-value 'var0-1 env1)
(lookup-variable-value 'var0-2 env1)
