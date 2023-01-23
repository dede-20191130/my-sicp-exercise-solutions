#lang racket
(require compatibility/mlist)
(require racket/exn)

(define sym-not-yel-assined '*unassigned*)

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (let-var-declarations exp) (cadr exp))
(define (let-body exp) (caddr exp))
(define (make-let decs body)
  (cons 'let (cons decs body)))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (definition? exp)
  (tagged-list? exp 'define))

(define (make-procedure parameters body env)
  (list
   'procedure
   parameters
   (scan-out-defines-simultaneous body)
   env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

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


(define (sort-partitioning pred? lst)
  (call-with-values (lambda () (partition pred? lst)) (lambda(top bottom) (list top bottom))))



;                                                                                                                                          
;                                                                                                                                          
;                                                                                                                                          
;                                                                                                                                          
;              ;                      ;;                                                                                      ;;           
;                                      ;     ;                                                                                 ;           
;    ;;;;;   ;;;   ;; ;  ;  ;;  ;;     ;    ;;;;;    ;;;;   ;; ;;    ;;;;    ;;;;   ;;  ;;   ;;;;;          ;; ;;;  ;;  ;;     ;     ;;;;  
;   ;    ;     ;    ;; ;; ;  ;   ;     ;     ;      ;    ;   ;;  ;  ;    ;  ;    ;   ;   ;  ;    ;           ;;      ;   ;     ;    ;    ; 
;    ;;;;      ;    ;  ;  ;  ;   ;     ;     ;       ;;;;;   ;   ;  ;;;;;;  ;    ;   ;   ;   ;;;;            ;       ;   ;     ;    ;;;;;; 
;        ;     ;    ;  ;  ;  ;   ;     ;     ;      ;    ;   ;   ;  ;       ;    ;   ;   ;       ;           ;       ;   ;     ;    ;      
;   ;    ;     ;    ;  ;  ;  ;  ;;     ;     ;   ;  ;   ;;   ;   ;  ;       ;    ;   ;  ;;  ;    ;           ;       ;  ;;     ;    ;      
;   ;;;;;    ;;;;; ;;; ;; ;   ;; ;;  ;;;;;    ;;;    ;;; ;; ;;; ;;;  ;;;;;   ;;;;     ;; ;; ;;;;;           ;;;;;     ;; ;;  ;;;;;   ;;;;; 
;                                                                                                                                          
;                                                                                                                                          
;                                                                                                                                          
;                                                                                                                                          



(define (scan-out-defines-simultaneous body)
  (let ((partitioned-body (sort-partitioning definition? body)))
    (let ((def-exps (car partitioned-body))
          (other-exps (cadr partitioned-body)))
      (append
       (map (λ(def-exp) (list (car def-exp) (cadr def-exp) (quote '*unassigned*))) def-exps)
       (map (λ(def-exp) (list 'set! (cadr def-exp) (caddr def-exp))) def-exps)
       other-exps))))

;;;test 

(define my-eval
  (let ((ns (make-base-namespace)))
    (lambda (expr) (eval expr ns))))


(define evaled-prop1 (my-eval (make-lambda '(x y)
                                           (scan-out-defines-simultaneous
                                            '((define inner-x 2)
                                              (println (+ x inner-x))
                                              (println (+ y inner-y))
                                              (println (+ inner-x inner-y inner-z))
                                              (define inner-y 4)
                                              (define inner-z 8))))))
(evaled-prop1 16 32)