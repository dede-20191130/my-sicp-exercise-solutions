#lang racket
(require compatibility/mlist)

;                                                                                                  
;                                                                                                  
;                                                                                                  
;                                                                                                  
;     ;;;;  ;;;       ;;;   ;;;;;     ;;    ;;;             ;;;;;;;   ;;    ;;;;;   ;;;     ;;;;;; 
;    ;   ;   ;       ;   ;   ;   ;     ;     ;              ;  ;  ;    ;     ;   ;   ;       ;   ; 
;   ;        ;      ;     ;  ;   ;    ; ;    ;                 ;      ; ;    ;   ;   ;       ; ;   
;   ;        ;      ;     ;  ;;;;     ; ;    ;                 ;      ; ;    ;;;;    ;       ;;;   
;   ;   ;;;  ;      ;     ;  ;   ;    ; ;    ;                 ;      ; ;    ;   ;   ;       ; ;   
;   ;    ;   ;   ;  ;     ;  ;   ;    ;;;    ;   ;             ;      ;;;    ;   ;   ;   ;   ;     
;    ;   ;   ;   ;   ;   ;   ;   ;   ;   ;   ;   ;             ;     ;   ;   ;   ;   ;   ;   ;   ; 
;     ;;;   ;;;;;;    ;;;   ;;;;;   ;;; ;;; ;;;;;;            ;;;   ;;; ;;; ;;;;;   ;;;;;;  ;;;;;; 
;                                                                                                  
;                                                                                                  
;                                                                                                  
;                                                                                                  

(define get '())

(define put '())

(define (make-table)
  (let ((local-table (mlist '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (massoc key-1 (mcdr local-table))))
        (if subtable
            (let ((record (massoc key-2 (mcdr subtable))))
              (if record
                  (mcdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (massoc key-1 (mcdr local-table))))
        (if subtable
            (let ((record (massoc key-2 (mcdr subtable))))
              (if record
                  (set-mcdr! record value)
                  (set-mcdr! subtable
                             (mcons (mcons key-2 value)
                                    (mcdr subtable)))))
            (set-mcdr! local-table
                       (mcons (mlist key-1
                                     (mcons key-2 value))
                              (mcdr local-table)))))
      'ok)    
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))
;                                                                                                          
;                                                                                                          
;                                                                                                          
;                                                                                                          
;    ;;; ;  ;;;;;;; ;;;;;   ;;;;;;    ;;    ;;; ;;;         ;;; ;;;   ;;   ;;;  ;;;  ;;;;;  ;;;;;          
;   ;   ;;  ;  ;  ;  ;   ;   ;   ;     ;     ;; ;;           ;; ;;     ;    ;;   ;     ;     ;   ;         
;   ;          ;     ;   ;   ; ;      ; ;    ;; ;;           ;; ;;    ; ;   ; ;  ;     ;     ;   ;         
;    ;;;;      ;     ;   ;   ;;;      ; ;    ; ; ;           ; ; ;    ; ;   ; ;  ;     ;     ;   ;         
;        ;     ;     ;;;;    ; ;      ; ;    ; ; ;           ; ; ;    ; ;   ;  ; ;     ;     ;;;;          
;        ;     ;     ;  ;    ;        ;;;    ;   ;           ;   ;    ;;;   ;  ; ;     ;     ;             
;   ;;   ;     ;     ;   ;   ;   ;   ;   ;   ;   ;           ;   ;   ;   ;  ;   ;;     ;     ;        ;;   
;   ; ;;;     ;;;   ;;;   ; ;;;;;;  ;;; ;;; ;;; ;;;         ;;; ;;; ;;; ;;;;;;  ;;   ;;;;;  ;;;       ;;   
;                                                                                                          
;                                                                                                          
;                                                                                                          
;                                                                                                          

(define stream-null? stream-empty?)
(define the-empty-stream empty-stream)
(define stream-car stream-first)
(define stream-cdr stream-rest)
(define (cons-stream first-expr rest-expr)
  (stream-cons first-expr rest-expr))

;                                  
;                                  
;                                  
;                                  
;   ;;;;;; ;;;  ;;;   ;;    ;;;    
;    ;   ;  ;    ;     ;     ;     
;    ; ;    ;    ;    ; ;    ;     
;    ;;;     ;  ;     ; ;    ;     
;    ; ;     ;  ;     ; ;    ;     
;    ;       ;  ;     ;;;    ;   ; 
;    ;   ;    ;;     ;   ;   ;   ; 
;   ;;;;;;    ;;    ;;; ;;; ;;;;;; 
;                                  
;                                  
;                                  
;                                  


(define (qeval query frame-stream)
  (let ((qproc (get (type query) 'qeval)))
    (let ((evaled-frame-stream  (if qproc
                                    (qproc (contents query) frame-stream)
                                    (simple-query query frame-stream))))
      (stream-flatmap
       (lambda(frm)
         ;         (println frm)
         (if (prmss-null? (frame-prmss frm))
             (singleton-stream frm)
             (let ((curr-prms (first-prms (frame-prmss frm))))
               (if ((prms-pred curr-prms) frm)
                   ((prms-delayed curr-prms) frm)
                   (singleton-stream frm)))))
       evaled-frame-stream))))
    

(define (simple-query query-pattern 
                      frame-stream)
  (stream-flatmap
   (lambda (frame)
     (stream-append-delayed
      (find-assertions query-pattern frame)
      (delay
        (apply-rules query-pattern frame))))
   frame-stream))

(define (conjoin conjuncts frame-stream)
  (if (empty-conjunction? conjuncts)
      frame-stream
      (conjoin (rest-conjuncts conjuncts)
               (qeval 
                (first-conjunct conjuncts)
                frame-stream))))
;(put 'and 'qeval conjoin)

(define (disjoin disjuncts frame-stream)
  (if (empty-disjunction? disjuncts)
      the-empty-stream
      (interleave-delayed
       (qeval (first-disjunct disjuncts) 
              frame-stream)
       (delay (disjoin 
               (rest-disjuncts disjuncts)
               frame-stream)))))
;(put 'or 'qeval disjoin)

(define (negate operands frame-stream)
  (stream-flatmap
   (lambda (frame)
     (singleton-stream
      (create-frame
       (frame-body frame)
       (add-prms
        (frame-prmss frame)
        (create-prms
         (lambda(frm)
           (let ((is-nobound false))
             (instantiate (negated-query operands) (frame-body frm) (lambda(v f) (set! is-nobound true)))
             (not is-nobound)))
         (lambda(frm)
           (if (stream-null? 
                (qeval (negated-query operands)
                       (singleton-stream (create-frame (frame-body frm) (rest-prmss (frame-prmss frm))))))
               ;               (singleton-stream frame)
               (singleton-stream (create-frame (frame-body frm) (rest-prmss (frame-prmss frm))))
               the-empty-stream)))))))
   frame-stream))
;(put 'not 'qeval negate)

(define (lisp-value call frame-stream)
  (stream-flatmap
   (lambda (frame)
     (singleton-stream
      (create-frame
       (frame-body frame)
       (add-prms
        (frame-prmss frame)
        (create-prms
         (lambda(frm)
           (let ((is-nobound false))
             (instantiate call (frame-body frm) (lambda(v f) (set! is-nobound true)))
             (not is-nobound)))
         (lambda(frm)
           (if (execute
                (instantiate
                    call
                  (frame-body frm)
                  (lambda (v f)
                    (error 
                     "Unknown pat var: LISP-VALUE" 
                     v))))
               (singleton-stream (create-frame (frame-body frm) (rest-prmss (frame-prmss frm))))
               the-empty-stream)))))))
   frame-stream))

(define user-initial-environment (make-base-namespace))

(define (execute exp)
  (apply (eval (predicate exp) 
               user-initial-environment)
         (args exp)))

(define (always-true ignore frame-stream) 
  frame-stream)
;(put 'always-true 'qeval always-true)



;                                                                                                                                                  
;                                                                                                                                                  
;                                                                                                                                                  
;                                                                                                                                                  
;   ;;;;;;   ;;;;; ;;;  ;;; ;;;;     ;;;;; ;;;  ;;;   ;;;;            ;;     ;;; ;   ;;; ;  ;;;;;;  ;;;;;    ;;; ;   ;;;;;    ;;;  ;;;  ;;;  ;;; ; 
;    ;   ;     ;    ;;   ;   ;  ;      ;    ;;   ;   ;   ;             ;    ;   ;;  ;   ;;   ;   ;   ;   ;  ;   ;;     ;     ;   ;  ;;   ;  ;   ;; 
;    ; ;       ;    ; ;  ;   ;   ;     ;    ; ;  ;  ;                 ; ;   ;       ;        ; ;     ;   ;  ;          ;    ;     ; ; ;  ;  ;      
;    ;;;       ;    ; ;  ;   ;   ;     ;    ; ;  ;  ;                 ; ;    ;;;;    ;;;;    ;;;     ;   ;   ;;;;      ;    ;     ; ; ;  ;   ;;;;  
;    ; ;       ;    ;  ; ;   ;   ;     ;    ;  ; ;  ;   ;;;           ; ;        ;       ;   ; ;     ;;;;        ;     ;    ;     ; ;  ; ;       ; 
;    ;         ;    ;  ; ;   ;   ;     ;    ;  ; ;  ;    ;            ;;;        ;       ;   ;       ;  ;        ;     ;    ;     ; ;  ; ;       ; 
;    ;         ;    ;   ;;   ;  ;      ;    ;   ;;   ;   ;           ;   ;  ;;   ;  ;;   ;   ;   ;   ;   ;  ;;   ;     ;     ;   ;  ;   ;;  ;;   ; 
;   ;;;      ;;;;; ;;;  ;;  ;;;;     ;;;;; ;;;  ;;    ;;;           ;;; ;;; ; ;;;   ; ;;;   ;;;;;;  ;;;   ; ; ;;;    ;;;;;    ;;;  ;;;  ;;  ; ;;;  
;                                                                                                                                                  
;                                                                                                                                                  
;                                                                                                                                                  
;                                                                                                                                                  



(define (find-assertions pattern frame)
  (stream-flatmap 
   (lambda (datum) 
     (check-an-assertion datum pattern frame))
   (fetch-assertions pattern frame)))

(define (check-an-assertion 
         assertion query-pat frame)
  (let ((match-result
         (pattern-match 
          query-pat assertion (frame-body frame))))
    (if (eq? match-result 'failed)
        the-empty-stream
        (singleton-stream (create-frame match-result (frame-prmss frame))))))

(define (pattern-match pat dat f-body)
  (cond ((eq? f-body 'failed) 'failed)
        ((equal? pat dat) f-body)
        ((var? pat) 
         (extend-if-consistent 
          pat dat f-body))
        ((and (pair? pat) (pair? dat))
         (pattern-match 
          (cdr pat) 
          (cdr dat)
          (pattern-match
           (car pat) (car dat) f-body)))
        (else 'failed)))

(define (extend-if-consistent var dat f-body)
  (let ((binding (binding-in-frame var f-body)))
    (if binding
        (pattern-match 
         (binding-value binding) dat f-body)
        (extend var dat f-body))))



;                                          
;                                          
;                                          
;                                          
;   ;;;;;   ;;; ;;; ;;;     ;;;;;;   ;;; ; 
;    ;   ;   ;   ;   ;       ;   ;  ;   ;; 
;    ;   ;   ;   ;   ;       ; ;    ;      
;    ;   ;   ;   ;   ;       ;;;     ;;;;  
;    ;;;;    ;   ;   ;       ; ;         ; 
;    ;  ;    ;   ;   ;   ;   ;           ; 
;    ;   ;   ;   ;   ;   ;   ;   ;  ;;   ; 
;   ;;;   ;   ;;;   ;;;;;;  ;;;;;;  ; ;;;  
;                                          
;                                          
;                                          
;                                          


(define (apply-rules pattern frame)
  (stream-flatmap 
   (lambda (rule)
     (apply-a-rule rule pattern frame))
   (fetch-rules pattern frame)))

(define (apply-a-rule rule
                      query-pattern
                      query-frame)
  (let ((clean-rule 
         (rename-variables-in rule)))
    (let ((unify-result
           (unify-match query-pattern
                        (conclusion clean-rule)
                        (frame-body query-frame))))
      (if (eq? unify-result 'failed)
          the-empty-stream
          (qeval (rule-body clean-rule)
                 (singleton-stream 
                  (create-frame  unify-result (frame-prmss query-frame))))))))

(define (rename-variables-in rule)
  (let ((rule-application-id 
         (new-rule-application-id)))
    (define (tree-walk exp)
      (cond ((var? exp)
             (make-new-variable 
              exp 
              rule-application-id))
            ((pair? exp)
             (cons (tree-walk (car exp))
                   (tree-walk (cdr exp))))
            (else exp)))
    (tree-walk rule)))

(define (unify-match p1 p2 f-body)
  (cond ((eq? f-body 'failed) 'failed)
        ((equal? p1 p2) f-body)
        ((var? p1)
         (extend-if-possible p1 p2 f-body))
        ((var? p2)
         (extend-if-possible 
          p2 
          p1 
          f-body))        ; ***
        ((and (pair? p1) 
              (pair? p2))
         (unify-match 
          (cdr p1) 
          (cdr p2)
          (unify-match 
           (car p1)
           (car p2)
           f-body)))
        (else 'failed)))

(define (extend-if-possible var val f-body)
  (let ((binding (binding-in-frame var f-body)))
    (cond (binding
           (unify-match
            (binding-value binding) val f-body))
          ((var? val)                   ; ***
           (let ((binding 
                  (binding-in-frame 
                   val
                   f-body)))
             (if binding
                 (unify-match
                  var 
                  (binding-value binding) 
                  f-body)
                 (extend var val f-body))))
          ((depends-on? val var f-body)  ; ***
           'failed)
          (else (extend var val f-body)))))

(define (depends-on? exp var f-body)
  (define (tree-walk e)
    (cond ((var? e)
           (if (equal? var e)
               true
               (let
                   ((b (binding-in-frame 
                        e 
                        f-body)))
                 (if b
                     (tree-walk 
                      (binding-value b))
                     false))))
          ((pair? e)
           (or (tree-walk (car e))
               (tree-walk (cdr e))))
          (else false)))
  (tree-walk exp))


;                                                                                                                                                          
;                                                                                                                                                          
;                                                                                                                                                          
;                                                                                                                                                          
;   ;;;;      ;;    ;;;;;;;   ;;    ;;;;;     ;;     ;;; ;  ;;;;;;          ;;; ;;;   ;;   ;;;  ;;;   ;;      ;;;;  ;;;;;;  ;;; ;;; ;;;;;; ;;;  ;;; ;;;;;;;
;    ;  ;      ;    ;  ;  ;    ;     ;   ;     ;    ;   ;;   ;   ;           ;; ;;     ;    ;;   ;     ;     ;   ;   ;   ;   ;; ;;   ;   ;  ;;   ;  ;  ;  ;
;    ;   ;    ; ;      ;      ; ;    ;   ;    ; ;   ;        ; ;             ;; ;;    ; ;   ; ;  ;    ; ;   ;        ; ;     ;; ;;   ; ;    ; ;  ;     ;   
;    ;   ;    ; ;      ;      ; ;    ;;;;     ; ;    ;;;;    ;;;             ; ; ;    ; ;   ; ;  ;    ; ;   ;        ;;;     ; ; ;   ;;;    ; ;  ;     ;   
;    ;   ;    ; ;      ;      ; ;    ;   ;    ; ;        ;   ; ;             ; ; ;    ; ;   ;  ; ;    ; ;   ;   ;;;  ; ;     ; ; ;   ; ;    ;  ; ;     ;   
;    ;   ;    ;;;      ;      ;;;    ;   ;    ;;;        ;   ;               ;   ;    ;;;   ;  ; ;    ;;;   ;    ;   ;       ;   ;   ;      ;  ; ;     ;   
;    ;  ;    ;   ;     ;     ;   ;   ;   ;   ;   ;  ;;   ;   ;   ;           ;   ;   ;   ;  ;   ;;   ;   ;   ;   ;   ;   ;   ;   ;   ;   ;  ;   ;;     ;   
;   ;;;;    ;;; ;;;   ;;;   ;;; ;;; ;;;;;   ;;; ;;; ; ;;;   ;;;;;;          ;;; ;;; ;;; ;;;;;;  ;;  ;;; ;;;   ;;;   ;;;;;;  ;;; ;;; ;;;;;; ;;;  ;;    ;;;  
;                                                                                                                                                          
;                                                                                                                                                          
;                                                                                                                                                          
;                                                                                                                                                          

(define THE-ASSERTIONS the-empty-stream)

(define (fetch-assertions pattern frame)
  (if (use-index? pattern)
      (get-indexed-assertions pattern)
      (get-all-assertions)))

(define (get-all-assertions) THE-ASSERTIONS)

(define (get-indexed-assertions pattern)
  (get-stream (index-key-of pattern)
              'assertion-stream))

(define (get-stream key1 key2)
  (let ((s (get key1 key2)))
    (if s s the-empty-stream)))

(define THE-RULES the-empty-stream)

(define (fetch-rules pattern frame)
  (if (use-index? pattern)
      (get-indexed-rules pattern)
      (get-all-rules)))

(define (get-all-rules) THE-RULES)

(define (get-indexed-rules pattern)
  (stream-append
   (get-stream (index-key-of pattern)
               'rule-stream)
   (get-stream '? 'rule-stream)))

(define (add-rule-or-assertion! assertion)
  (if (rule? assertion)
      (add-rule! assertion)
      (add-assertion! assertion)))

(define (add-assertion! assertion)
  (store-assertion-in-index assertion)
  (let ((old-assertions THE-ASSERTIONS))
    (set! THE-ASSERTIONS
          (cons-stream assertion 
                       old-assertions))
    'ok))

(define (add-rule! rule)
  (store-rule-in-index rule)
  (let ((old-rules THE-RULES))
    (set! THE-RULES
          (cons-stream rule  old-rules))
    'ok))

(define (store-assertion-in-index assertion)
  (if (indexable? assertion)
      (let ((key (index-key-of assertion)))
        (let ((current-assertion-stream
               (get-stream 
                key 'assertion-stream)))
          (put key
               'assertion-stream
               (cons-stream 
                assertion
                current-assertion-stream))))
      'false))

(define (store-rule-in-index rule)
  (let ((pattern (conclusion rule)))
    (if (indexable? pattern)
        (let ((key (index-key-of pattern)))
          (let ((current-rule-stream
                 (get-stream 
                  key 'rule-stream)))
            (put key
                 'rule-stream
                 (cons-stream 
                  rule
                  current-rule-stream))))
        'false)))

(define (indexable? pat)
  (or (constant-symbol? (car pat))
      (var? (car pat))))

(define (index-key-of pat)
  (let ((key (car pat)))
    (if (var? key) '? key)))

(define (use-index? pat)
  (constant-symbol? (car pat)))


;                                                                                                                                          
;                                                                                                                                          
;                                                                                                                                          
;                                                                                                                                          
;    ;;; ;  ;;;;;;; ;;;;;   ;;;;;;    ;;    ;;; ;;;           ;;;   ;;;;;   ;;;;;;  ;;;;;     ;;    ;;;;;;;  ;;;;;    ;;;  ;;;  ;;;  ;;; ; 
;   ;   ;;  ;  ;  ;  ;   ;   ;   ;     ;     ;; ;;           ;   ;   ;   ;   ;   ;   ;   ;     ;    ;  ;  ;    ;     ;   ;  ;;   ;  ;   ;; 
;   ;          ;     ;   ;   ; ;      ; ;    ;; ;;          ;     ;  ;   ;   ; ;     ;   ;    ; ;      ;       ;    ;     ; ; ;  ;  ;      
;    ;;;;      ;     ;   ;   ;;;      ; ;    ; ; ;          ;     ;  ;   ;   ;;;     ;   ;    ; ;      ;       ;    ;     ; ; ;  ;   ;;;;  
;        ;     ;     ;;;;    ; ;      ; ;    ; ; ;          ;     ;  ;;;;    ; ;     ;;;;     ; ;      ;       ;    ;     ; ;  ; ;       ; 
;        ;     ;     ;  ;    ;        ;;;    ;   ;          ;     ;  ;       ;       ;  ;     ;;;      ;       ;    ;     ; ;  ; ;       ; 
;   ;;   ;     ;     ;   ;   ;   ;   ;   ;   ;   ;           ;   ;   ;       ;   ;   ;   ;   ;   ;     ;       ;     ;   ;  ;   ;;  ;;   ; 
;   ; ;;;     ;;;   ;;;   ; ;;;;;;  ;;; ;;; ;;; ;;;           ;;;   ;;;     ;;;;;;  ;;;   ; ;;; ;;;   ;;;    ;;;;;    ;;;  ;;;  ;;  ; ;;;  
;                                                                                                                                          
;                                                                                                                                          
;                                                                                                                                          
;                                                                                                                                          

(define (stream-append-delayed s1 delayed-s2)
  (if (stream-null? s1)
      (force delayed-s2)
      (cons-stream
       (stream-car s1)
       (stream-append-delayed (stream-cdr s1)
                              delayed-s2))))

(define (interleave-delayed s1 delayed-s2)
  (if (stream-null? s1)
      (force delayed-s2)
      (cons-stream
       (stream-car s1)
       (interleave-delayed 
        (force delayed-s2)
        (delay (stream-cdr s1))))))

(define (stream-flatmap proc s)
  (flatten-stream (stream-map proc s)))

(define (flatten-stream stream)
  (if (stream-null? stream)
      the-empty-stream
      (interleave-delayed
       (stream-car stream)
       (delay (flatten-stream
               (stream-cdr stream))))))

(define (singleton-stream x)
  (cons-stream x  the-empty-stream))



;                                                                                                                                          
;                                                                                                                                          
;                                                                                                                                          
;                                                                                                                                          
;    ;;; ;  ;;; ;;;;;;  ;;; ;;;;;;;   ;;    ;;; ;;;         ;;;;;   ;;;;;     ;;;     ;;;;  ;;;;;;  ;;;;    ;;; ;;; ;;;;;   ;;;;;;   ;;; ; 
;   ;   ;;   ;   ;  ;;   ;  ;  ;  ;    ;     ;   ;           ;   ;   ;   ;   ;   ;   ;   ;   ;   ;   ;  ;    ;   ;   ;   ;   ;   ;  ;   ;; 
;   ;         ; ;   ; ;  ;     ;      ; ;     ; ;            ;   ;   ;   ;  ;     ; ;        ; ;     ;   ;   ;   ;   ;   ;   ; ;    ;      
;    ;;;;     ; ;   ; ;  ;     ;      ; ;      ;             ;   ;   ;   ;  ;     ; ;        ;;;     ;   ;   ;   ;   ;   ;   ;;;     ;;;;  
;        ;     ;    ;  ; ;     ;      ; ;      ;             ;;;;    ;;;;   ;     ; ;        ; ;     ;   ;   ;   ;   ;;;;    ; ;         ; 
;        ;     ;    ;  ; ;     ;      ;;;     ; ;            ;       ;  ;   ;     ; ;        ;       ;   ;   ;   ;   ;  ;    ;           ; 
;   ;;   ;     ;    ;   ;;     ;     ;   ;   ;   ;           ;       ;   ;   ;   ;   ;   ;   ;   ;   ;  ;    ;   ;   ;   ;   ;   ;  ;;   ; 
;   ; ;;;     ;;;  ;;;  ;;    ;;;   ;;; ;;; ;;; ;;;         ;;;     ;;;   ;   ;;;     ;;;   ;;;;;;  ;;;;      ;;;   ;;;   ; ;;;;;;  ; ;;;  
;                                                                                                                                          
;                                                                                                                                          
;                                                                                                                                          
;                                                                                                                                          


(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))


;                                                                                                                                                                                          
;                                                                                                                                                                                          
;                                                                                                                                                                                          
;                                                                                                                                                                                          
;     ;;;   ;;; ;;; ;;;;;;  ;;;;;   ;;; ;;;          ;;; ;  ;;; ;;;;;;  ;;; ;;;;;;;   ;;    ;;; ;;;         ;;;;;   ;;;;;     ;;;     ;;;;  ;;;;;;  ;;;;    ;;; ;;; ;;;;;   ;;;;;;   ;;; ; 
;    ;   ;   ;   ;   ;   ;   ;   ;   ;   ;          ;   ;;   ;   ;  ;;   ;  ;  ;  ;    ;     ;   ;           ;   ;   ;   ;   ;   ;   ;   ;   ;   ;   ;  ;    ;   ;   ;   ;   ;   ;  ;   ;; 
;   ;     ;  ;   ;   ; ;     ;   ;    ; ;           ;         ; ;   ; ;  ;     ;      ; ;     ; ;            ;   ;   ;   ;  ;     ; ;        ; ;     ;   ;   ;   ;   ;   ;   ; ;    ;      
;   ;     ;  ;   ;   ;;;     ;   ;    ; ;            ;;;;     ; ;   ; ;  ;     ;      ; ;      ;             ;   ;   ;   ;  ;     ; ;        ;;;     ;   ;   ;   ;   ;   ;   ;;;     ;;;;  
;   ;     ;  ;   ;   ; ;     ;;;;      ;                 ;     ;    ;  ; ;     ;      ; ;      ;             ;;;;    ;;;;   ;     ; ;        ; ;     ;   ;   ;   ;   ;;;;    ; ;         ; 
;   ;     ;  ;   ;   ;       ;  ;      ;                 ;     ;    ;  ; ;     ;      ;;;     ; ;            ;       ;  ;   ;     ; ;        ;       ;   ;   ;   ;   ;  ;    ;           ; 
;    ;   ;   ;   ;   ;   ;   ;   ;     ;            ;;   ;     ;    ;   ;;     ;     ;   ;   ;   ;           ;       ;   ;   ;   ;   ;   ;   ;   ;   ;  ;    ;   ;   ;   ;   ;   ;  ;;   ; 
;     ;;;     ;;;   ;;;;;;  ;;;   ;   ;;;           ; ;;;     ;;;  ;;;  ;;    ;;;   ;;; ;;; ;;; ;;;         ;;;     ;;;   ;   ;;;     ;;;   ;;;;;;  ;;;;      ;;;   ;;;   ; ;;;;;;  ; ;;;  
;     ;;;;;                                                                                                                                                                                
;                                                                                                                                                                                          
;                                                                                                                                                                                          
;                                                                                                                                                                                          

(define (type exp)
  (if (pair? exp)
      (car exp)
      (error "Unknown expression TYPE"
             exp)))

(define (contents exp)
  (if (pair? exp)
      (cdr exp)
      (error "Unknown expression CONTENTS"
             exp)))

(define (assertion-to-be-added? exp)
  (eq? (type exp) 'assert!))

(define (add-assertion-body exp)
  (car (contents exp)))

(define (empty-conjunction? exps) (null? exps))
(define (first-conjunct exps) (car exps))
(define (rest-conjuncts exps) (cdr exps))
(define (empty-disjunction? exps) (null? exps))
(define (first-disjunct exps) (car exps))
(define (rest-disjuncts exps) (cdr exps))
(define (negated-query exps) (car exps))
(define (predicate exps) (car exps))
(define (args exps) (cdr exps))

(define (rule? statement)
  (tagged-list? statement 'rule))

(define (conclusion rule) (cadr rule))

(define (rule-body rule)
  (if (null? (cddr rule))
      '(always-true)
      (caddr rule)))

(define (query-syntax-process exp)
  (map-over-symbols expand-question-mark exp))

(define (map-over-symbols proc exp)
  (cond ((pair? exp)
         (cons (map-over-symbols 
                proc (car exp))
               (map-over-symbols 
                proc (cdr exp))))
        ((symbol? exp) (proc exp))
        (else exp)))

(define (expand-question-mark symbol)
  (let ((chars (symbol->string symbol)))
    (if (string=? (substring chars 0 1) "?")
        (list '? (string->symbol
                  (substring
                   chars 
                   1 
                   (string-length chars))))
        symbol)))

(define (var? exp) (tagged-list? exp '?))
(define (constant-symbol? exp) (symbol? exp))

(define rule-counter 0)

(define (new-rule-application-id)
  (set! rule-counter (+ 1 rule-counter))
  rule-counter)

(define (make-new-variable 
         var rule-application-id)
  (cons '? (cons rule-application-id
                 (cdr var))))

(define (contract-question-mark variable)
  (string->symbol
   (string-append "?"
                  (if (number? (cadr variable))
                      (string-append
                       (symbol->string (caddr variable))
                       "-"
                       (number->string (cadr variable)))
                      (symbol->string (cadr variable))))))


;                                                                                                                                          
;                                                                                                                                          
;                                                                                                                                          
;                                                                                                                                          
;   ;;;;;;  ;;;;;     ;;    ;;; ;;; ;;;;;;   ;;; ;                          ;;;;;    ;;;;; ;;;  ;;; ;;;;     ;;;;; ;;;  ;;;   ;;;;   ;;; ; 
;    ;   ;   ;   ;     ;     ;; ;;   ;   ;  ;   ;;             ;;;           ;   ;     ;    ;;   ;   ;  ;      ;    ;;   ;   ;   ;  ;   ;; 
;    ; ;     ;   ;    ; ;    ;; ;;   ; ;    ;                 ;              ;   ;     ;    ; ;  ;   ;   ;     ;    ; ;  ;  ;       ;      
;    ;;;     ;   ;    ; ;    ; ; ;   ;;;     ;;;;             ;              ;;;;      ;    ; ;  ;   ;   ;     ;    ; ;  ;  ;        ;;;;  
;    ; ;     ;;;;     ; ;    ; ; ;   ; ;         ;            ;;             ;   ;     ;    ;  ; ;   ;   ;     ;    ;  ; ;  ;   ;;;      ; 
;    ;       ;  ;     ;;;    ;   ;   ;           ;           ; ; ;           ;   ;     ;    ;  ; ;   ;   ;     ;    ;  ; ;  ;    ;       ; 
;    ;       ;   ;   ;   ;   ;   ;   ;   ;  ;;   ;           ;  ;            ;   ;     ;    ;   ;;   ;  ;      ;    ;   ;;   ;   ;  ;;   ; 
;   ;;;     ;;;   ; ;;; ;;; ;;; ;;; ;;;;;;  ; ;;;             ;;;;          ;;;;;    ;;;;; ;;;  ;;  ;;;;     ;;;;; ;;;  ;;    ;;;   ; ;;;  
;                                                                                                                                          
;                                                                                                                                          
;                                                                                                                                          
;                                                                                                                                          


(define (make-binding variable value)
  (mcons variable value))

(define (binding-variable binding)
  (mcar binding))

(define (binding-value binding)
  (mcdr binding))

(define (binding-in-frame variable f-body)
  (massoc variable f-body))

(define (extend variable value f-body)
  (mcons (make-binding variable value) f-body))

(define (create-frame l p) (mcons l p))
(define (frame-body frame) (mcar frame))
(define (frame-prmss frame) (mcdr frame))
(define (first-prms prmss) (mcar prmss))
(define (rest-prmss prmss) (mcdr prmss))
(define (add-prms prmss v) (mcons v prmss))
(define (prmss-null? prmss) (null? prmss))
(define the-empty-prmss '())
(define (prms-pred prms) (car prms))
(define (prms-delayed prms) (cdr prms))
(define (create-prms predproc delayedproc) (cons predproc delayedproc))

;                                                                                          
;                                                                                          
;                                                                                          
;                                                                                          
;   ;;;;    ;;;;;    ;;;;; ;;;  ;;; ;;;;;;  ;;;;;           ;;;       ;;;     ;;;   ;;;;;  
;    ;  ;    ;   ;     ;    ;    ;   ;   ;   ;   ;           ;       ;   ;   ;   ;   ;   ; 
;    ;   ;   ;   ;     ;    ;    ;   ; ;     ;   ;           ;      ;     ; ;     ;  ;   ; 
;    ;   ;   ;   ;     ;     ;  ;    ;;;     ;   ;           ;      ;     ; ;     ;  ;   ; 
;    ;   ;   ;;;;      ;     ;  ;    ; ;     ;;;;            ;      ;     ; ;     ;  ;;;;  
;    ;   ;   ;  ;      ;     ;  ;    ;       ;  ;            ;   ;  ;     ; ;     ;  ;     
;    ;  ;    ;   ;     ;      ;;     ;   ;   ;   ;           ;   ;   ;   ;   ;   ;   ;     
;   ;;;;    ;;;   ;  ;;;;;    ;;    ;;;;;;  ;;;   ;         ;;;;;;    ;;;     ;;;   ;;;    
;                                                                                          
;                                                                                          
;                                                                                          
;                                                                                          


(define input-prompt  ";;; Query input:")
(define output-prompt ";;; Query results:")

(define (prompt-for-input string)
  (newline) (newline) 
  (display string) (newline))

(define (display-stream s)
  (println (stream->list s)))

(define (query-driver-loop)
  (prompt-for-input input-prompt)
  (let ((q (query-syntax-process (read))))
    (cond ((assertion-to-be-added? q)
           (add-rule-or-assertion! 
            (add-assertion-body q))
           (newline)
           (display 
            "Assertion added to data base.")
           (query-driver-loop))
          (else
           (newline)
           (display output-prompt)
           (display-stream
            (stream-map
             (lambda (frame)
               (instantiate
                   q
                 (frame-body frame)
                 (lambda (v f)
                   (contract-question-mark v))))
             (qeval q (singleton-stream (create-frame '() '())))))
           (query-driver-loop)))))

(define (instantiate 
            exp f-body unbound-var-handler)
  (define (copy exp)
    (cond ((var? exp)
           (let ((binding 
                  (binding-in-frame 
                   exp f-body)))
             (if binding
                 (copy 
                  (binding-value binding))
                 (unbound-var-handler 
                  exp f-body))))
          ((pair? exp)
           (cons (copy (car exp)) 
                 (copy (cdr exp))))
          (else exp)))
  (copy exp))


;                                                                          
;                                                                          
;                                                                          
;                                                                          
;   ;;;;;   ;;;;;   ;;;;;;  ;;;;;     ;;    ;;;;;    ;;;;; ;;;  ;;;   ;;;; 
;    ;   ;   ;   ;   ;   ;   ;   ;     ;     ;   ;     ;    ;;   ;   ;   ; 
;    ;   ;   ;   ;   ; ;     ;   ;    ; ;    ;   ;     ;    ; ;  ;  ;      
;    ;   ;   ;   ;   ;;;     ;   ;    ; ;    ;   ;     ;    ; ;  ;  ;      
;    ;;;;    ;;;;    ; ;     ;;;;     ; ;    ;;;;      ;    ;  ; ;  ;   ;;;
;    ;       ;  ;    ;       ;        ;;;    ;  ;      ;    ;  ; ;  ;    ; 
;    ;       ;   ;   ;   ;   ;       ;   ;   ;   ;     ;    ;   ;;   ;   ; 
;   ;;;     ;;;   ; ;;;;;;  ;;;     ;;; ;;; ;;;   ;  ;;;;; ;;;  ;;    ;;;  
;                                                                          
;                                                                          
;                                                                          
;                                                                          


(define (initialize-data-base rules-and-assertions)
  (define (deal-out r-and-a rules assertions)
    (cond ((null? r-and-a)
           (set! THE-ASSERTIONS (in-list assertions))
           (set! THE-RULES (in-list rules))
           'done)
          (else
           (let ((s (query-syntax-process (car r-and-a))))
             (cond ((rule? s)
                    (store-rule-in-index s)
                    (deal-out (cdr r-and-a)
                              (cons s rules)
                              assertions))
                   (else
                    (store-assertion-in-index s)
                    (deal-out (cdr r-and-a)
                              rules
                              (cons s assertions))))))))
  (let ((operation-table (make-table)))
    (set! get (operation-table 'lookup-proc))
    (set! put (operation-table 'insert-proc!)))
  (put 'and 'qeval conjoin)
  (put 'or 'qeval disjoin)
  (put 'not 'qeval negate)
  (put 'lisp-value 'qeval lisp-value)
  (put 'always-true 'qeval always-true)
  ;  (put 'unique 'qeval uniquely-asserted)
  (deal-out rules-and-assertions '() '()))

(define microshaft-data-base
  '(
    ;; from section 4.4.1
    (address (Bitdiddle Ben) (Slumerville (Ridge Road) 10))
    (job (Bitdiddle Ben) (computer wizard))
    (salary (Bitdiddle Ben) 60000)

    (address (Hacker Alyssa P) (Cambridge (Mass Ave) 78))
    (job (Hacker Alyssa P) (computer programmer))
    (salary (Hacker Alyssa P) 40000)
    (supervisor (Hacker Alyssa P) (Bitdiddle Ben))

    (address (Fect Cy D) (Cambridge (Ames Street) 3))
    (job (Fect Cy D) (computer programmer))
    (salary (Fect Cy D) 35000)
    (supervisor (Fect Cy D) (Bitdiddle Ben))

    (address (Tweakit Lem E) (Boston (Bay State Road) 22))
    (job (Tweakit Lem E) (computer technician))
    (salary (Tweakit Lem E) 25000)
    (supervisor (Tweakit Lem E) (Bitdiddle Ben))

    (address (Reasoner Louis) (Slumerville (Pine Tree Road) 80))
    (job (Reasoner Louis) (computer programmer trainee))
    (salary (Reasoner Louis) 30000)
    (supervisor (Reasoner Louis) (Hacker Alyssa P))

    (supervisor (Bitdiddle Ben) (Warbucks Oliver))

    (address (Warbucks Oliver) (Swellesley (Top Heap Road)))
    (job (Warbucks Oliver) (administration big wheel))
    (salary (Warbucks Oliver) 150000)

    (address (Scrooge Eben) (Weston (Shady Lane) 10))
    (job (Scrooge Eben) (accounting chief accountant))
    (salary (Scrooge Eben) 75000)
    (supervisor (Scrooge Eben) (Warbucks Oliver))

    (address (Cratchet Robert) (Allston (N Harvard Street) 16))
    (job (Cratchet Robert) (accounting scrivener))
    (salary (Cratchet Robert) 18000)
    (supervisor (Cratchet Robert) (Scrooge Eben))

    (address (Aull DeWitt) (Slumerville (Onion Square) 5))
    (job (Aull DeWitt) (administration secretary))
    (salary (Aull DeWitt) 25000)
    (supervisor (Aull DeWitt) (Warbucks Oliver))

    (can-do-job (computer wizard) (computer programmer))
    (can-do-job (computer wizard) (computer technician))

    (can-do-job (computer programmer)
                (computer programmer trainee))

    (can-do-job (administration secretary)
                (administration big wheel))

    (son Adam Cain)
    (son Cain Enoch)
    (son Enoch Irad)
    (son Irad Mehujael)
    (son Mehujael Methushael)
    (son Methushael Lamech)
    (wife Lamech Ada)
    (son Ada Jabal)
    (son Ada Jubal)

    (rule (lives-near ?person-1 ?person-2)
          (and (address ?person-1 (?town . ?rest-1))
               (address ?person-2 (?town . ?rest-2))
               (not (same ?person-1 ?person-2))))

    (rule (same ?x ?x))

    (rule (wheel ?person)
          (and (supervisor ?middle-manager ?person)
               (supervisor ?x ?middle-manager)))

    (rule (outranked-by ?staff-person ?boss)
          (or (supervisor ?staff-person ?boss)
              (and (supervisor ?staff-person ?middle-manager)
                   (outranked-by ?middle-manager ?boss))))

    (rule (supervise-only-one ?person)
          (and (job ?person ?job)
               (unique (supervisor ?x ?person))))

    (rule (last-pair (?first . ?rest) ?last)
          (last-pair ?rest ?last))
    (rule (last-pair (?last) (?last)))

    (rule (append-to-form ()
                          ?last
                          (?last)))
    (rule (append-to-form (?first . ?rest)
                          ?last
                          (?first . ?another-rest))
          (append-to-form ?rest ?last ?another-rest))

    (rule (reverse () ()))
    (rule (reverse (?first . ?rest) ?another)
          (and (reverse ?rest ?rest-reversed)
               (append-to-form ?rest-reversed ?first ?another)))

    (rule (grandson ?old ?kid)
          (and (has-son ?old ?young)
               (has-son ?young ?kid)))
    (rule (has-son ?old ?young)
          (or (son ?old ?young)
              (and (wife ?old ?old-wife)
                   (son ?old-wife ?young))))

    (rule (list-end-in-grandson (grandson)))
    (rule (list-end-in-grandson (?first . ?rest))
          (list-end-in-grandson ?rest))
    (rule ((grandson) ?old ?young)
          (grandson ?old ?young))
    (rule ((great . ?rel) ?a ?b)
          (and (has-son ?c ?b)
               (?rel ?a ?c)
               (list-end-in-grandson ?rel)))

    ))

(initialize-data-base microshaft-data-base)

(query-driver-loop)