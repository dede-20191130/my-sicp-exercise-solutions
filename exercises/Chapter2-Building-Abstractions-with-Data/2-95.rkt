#lang racket 
 
(define (print-line value) 
  (display value) 
  (newline))
(define nil '())

(define (square x) (* x x))

(define (some? predicate? tgt-list)
  (cond ((null? tgt-list) false)
        ((predicate? (car tgt-list)) true)
        (else (some? predicate? (cdr tgt-list)))))

(define (every? predicate? tgt-list)
  (cond ((null? tgt-list) true)
        ((not (predicate? (car tgt-list))) false)
        (else (some? predicate? (cdr tgt-list)))))

(define global-array '())

(define (make-entry k v) (list k v))
(define (key entry) (car entry))
(define (value entry) (cadr entry))


(define (put op type item)
  (define (put-helper k array)
    (cond ((null? array) (list(make-entry k item)))
          ((equal? (key (car array)) k) array)
          (else (cons (car array) (put-helper k (cdr array))))))
  (set! global-array (put-helper (list op type) global-array)))

(define (get op type)
  (define (get-helper k array)
    (cond ((null? array) false)
          ((equal? (key (car array)) k) (value (car array)))
          (else (get-helper k (cdr array)))))
  (get-helper (list op type) global-array))



(define (attach-tag type-tag contents) 
  (if (number? contents) 
      contents 
      (cons type-tag contents)))

(define (type-tag datum)
  (cond ((number? datum) 'scheme-number)
        ((symbol? datum) 'scheme-symbol)
        ((pair? datum)
         (car datum))
        (else (error "Bad tagged datum -- TYPE-TAG" datum))))
(define (contents datum)
  (cond ((or (number? datum) (symbol? datum)) datum)
        ((pair? datum)
         (cdr datum))
        (else (error "Bad tagged datum -- TYPE-TAG" datum))))

(define tower-of-types '(integer rational scheme-number complex))

;                                                                                                          
;                                                                                                          
;                                                                                                          
;                                                                                                          
;                             ;;                                                                      ;;   
;                              ;                                                                       ;   
;    ;;;;   ;; ;;   ;; ;;      ;    ;;; ;;;          ;;; ;;  ;;;;   ;; ;;    ;;;;   ;; ;;;   ;;;;      ;   
;   ;    ;   ;;  ;   ;;  ;     ;     ;   ;          ;   ;;  ;    ;   ;;  ;  ;    ;   ;;     ;    ;     ;   
;    ;;;;;   ;   ;   ;   ;     ;     ;   ;  ;;;;;;  ;    ;  ;;;;;;   ;   ;  ;;;;;;   ;       ;;;;;     ;   
;   ;    ;   ;   ;   ;   ;     ;      ; ;           ;    ;  ;        ;   ;  ;        ;      ;    ;     ;   
;   ;   ;;   ;   ;   ;   ;     ;      ; ;           ;   ;;  ;        ;   ;  ;        ;      ;   ;;     ;   
;    ;;; ;;  ;;;;    ;;;;    ;;;;;     ;             ;;; ;   ;;;;;  ;;; ;;;  ;;;;;  ;;;;;    ;;; ;;  ;;;;; 
;            ;       ;                 ;                 ;                                                 
;           ;;;     ;;;              ;;;             ;;;;                                                  
;                                                                                                          
;                                                                                                          


(define (apply-generic op . args)
  (define (all-same-type? my-tags val-1st)
    (cond ((null? my-tags) true)
          ((eq? (car my-tags) val-1st) (all-same-type? (cdr my-tags) val-1st))
          (else false)))
  
  (define (get-type-rank tgt-type)
    (define (gtr tower rank)
      (cond ((null? tower) (error "The Type is not in hierarchy tower of type" tgt-type))
            ((eq? tgt-type (car tower)) rank)
            (else (gtr (cdr tower) (+ rank 1)))))
    (gtr tower-of-types 1))
        
    
  (define (detect-highest-type my-tags )
    ;    (print-line my-tags)
    (define (dht inr-tags highest rank)
      (if (null? inr-tags)
          highest
          (let ((curr-type-rank (get-type-rank (car inr-tags))))
            (if (> curr-type-rank rank)
                (dht (cdr inr-tags) (car inr-tags) curr-type-rank)
                (dht (cdr inr-tags) highest rank)))))
    (dht my-tags (car tower-of-types) 1))

  (define (raise-to-highest-type tgt-data highest)
    (if (eq? (type-tag tgt-data) highest)
        tgt-data
        (raise-to-highest-type (raise tgt-data) highest)))
  
  (define (drop-as-possible tgt-data)
    (if (or (eq? op 'drop) (eq? op 'raise) (not (droppable? tgt-data)))
        tgt-data
        (let ((tag-before (type-tag tgt-data))
              (dropped (drop tgt-data)))
          (if (eq? tag-before (type-tag dropped))
              dropped
              (drop-as-possible dropped)))))

  (define (helper arg-list)
    ;    (print-line arg-list)
    (let ((type-tags (map type-tag arg-list)))
      (let ((proc (get op type-tags)))
        (if proc
            (apply proc (map contents arg-list))
            (cond ((< (length arg-list) 2) (error "No method for these types"
                                                  (list op type-tags)))
                  ((not (some? raisable? arg-list)) (error "No method for these types"
                                                           (list op type-tags)))
                  ((all-same-type? type-tags (car type-tags)) (helper (map raise arg-list)))
                  (else
                   (let ((highest-type (detect-highest-type type-tags)))
                     (helper (map
                              (lambda (tgt-data) (raise-to-highest-type tgt-data highest-type))
                              arg-list)))))))))

  (helper (map drop-as-possible args)))



(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (do-negate x) (apply-generic 'do-negate x))
(define (greatest-common-divisor x y) (apply-generic 'do-gcd x y))
(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? x) (apply-generic '=zero? x))
(define (exp x y) (apply-generic 'exp x y))
(define (raise x) (apply-generic 'raise x))
(define (drop x) (apply-generic 'drop x))
(define (raisable? x) (not (false? (get 'raise (list (type-tag x))))))
(define (droppable? x) (not (false? (get 'drop (list (type-tag x))))))



(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a) 
    (cons (* r (cos a)) (* r (sin a))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular 
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular 
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y) 
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar 
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)


(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))


(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))
(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))

(define (install-scheme-number-package)
  (define (equ? x y) (= x y))
  (define (project x) (make-integer (round x)))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y)  (+ x y)))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y)  (- x y)))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y)  (* x y)))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y)  (/ x y)))
  (put 'do-negate '(scheme-number)
       (lambda (x) (* x -1)))
  (put 'equ? '(scheme-number scheme-number) equ?)
       
  (put '=zero? '(scheme-number)
       (lambda (x) (= x 0)))
  (put 'do-gcd '(scheme-number scheme-number)
       (lambda (x y) (gcd x y)))
  
  ;; following added to Scheme-number package
  (put 'exp '(scheme-number scheme-number)
       (lambda (x y)  (expt x y))) ; using primitive expt
  (put 'raise '(scheme-number)
       (lambda (x) ((get 'make-from-real-imag 'complex) x 0)))
  (put 'drop '(scheme-number)
       (lambda (x)
         (let ((projected (project x)))
           (if (equ? x (contents (raise (raise projected))))
               projected
               x))))
  (put 'make 'scheme-number
       (lambda (x) x))
  'done)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(define (install-integer-package)
  
  (define (tag x) (cons 'integer x))
  (put 'add '(integer integer)
       (lambda (n m)  (tag (+ n m))))
  (put 'sub '(integer integer)
       (lambda (n m)  (tag (- n m))))
  (put 'mul '(integer integer)
       (lambda (n m)  (tag (* n m))))
  (put 'do-negate '(integer)
       (lambda (n) (tag (* n -1))))
  (put 'equ? '(integer integer)
       (lambda (n m) (= n m)))
  (put '=zero? '(integer)
       (lambda (n) (= n 0)))
  (put 'raise '(integer)
       (lambda (n) ((get 'make 'rational) n 1)))
  (put 'make 'integer
       (lambda (n)
         (if (integer? n)
             (tag n)
             (error "The argument is not integer" n))))
  'done)

(define (make-integer n)
  ((get 'make 'integer) n))
  

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  
  (define (equ? x y) (and (= (denom x) (denom y)) (= (numer x) (numer y))))
  (define (project x) (make-integer (round (/ (numer x) (denom x)))))
  (define (tag x) (attach-tag 'rational x))
  (define make (lambda (n d) (tag (make-rat n d))))
  ;; interface to rest of the system
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'do-negate '(rational)
       (lambda (x) (tag (make-rat (* (numer x) -1) (denom x)))))
  
  (put 'equ? '(rational rational) equ?)
       
  (put '=zero? '(rational)
       (lambda (x) (and (not (= (denom x) 0)) (= (numer x) 0))))
  (put 'raise '(rational)
       (lambda (x) ((get 'make 'scheme-number) (/ (numer x) (denom x)))))
  (put 'drop '(rational)
       (lambda (x)
         (let ((projected (project x)))
           (if (equ? x (contents(raise projected)))
               projected
               (tag x)))))
  (put 'make 'rational make)
       
  'done)
(define (make-rational n d)
  ((get 'make 'rational) n d))



(define (install-complex-package)
  (define (tag z) (attach-tag 'complex z))
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))
  
  (define (equ? z1 z2) (and (= (real-part z1) (real-part z2)) (= (imag-part z1) (imag-part z2))))
  (define (project z)
    (make-scheme-number (real-part z)))
  ;; interface to rest of the system
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'do-negate '(complex)
       (lambda (z) (make-from-real-imag (* (real-part z) -1) (* (imag-part z) -1))))
  (put 'equ? '(complex complex) equ?)
       
  (put '=zero? '(complex)
       (lambda (z) (and (= (real-part z) 0) (= (imag-part z) 0))))
  (put 'drop '(complex)
       (lambda (z)
         (let ((projected (project z)))
           (if (equ? z (contents(raise projected)))
               projected
               (tag z)))))

  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)

  'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))


;                                                                                                                                                  
;                                                                                                                                                  
;                                                                                                                                                  
;                                                                                                                                                  
;   ;;;;;             ;;                                       ;              ;;            ;;;;;                   ;;                             
;    ;   ;             ;                                                       ;             ;   ;                   ;                             
;    ;   ;   ;;;;      ;    ;;; ;;; ;; ;;    ;;;;  ;; ;  ;   ;;;     ;;;;      ;             ;   ;   ;;;;    ;;; ;   ; ;;;;  ;;;;    ;;; ;;  ;;;;  
;    ;   ;  ;    ;     ;     ;   ;   ;;  ;  ;    ;  ;; ;; ;    ;    ;    ;     ;             ;   ;  ;    ;  ;   ;;   ;  ;   ;    ;  ;   ;;  ;    ; 
;    ;;;;   ;    ;     ;     ;   ;   ;   ;  ;    ;  ;  ;  ;    ;     ;;;;;     ;             ;;;;    ;;;;;  ;        ;;;     ;;;;;  ;    ;  ;;;;;; 
;    ;      ;    ;     ;      ; ;    ;   ;  ;    ;  ;  ;  ;    ;    ;    ;     ;             ;      ;    ;  ;        ; ;    ;    ;  ;    ;  ;      
;    ;      ;    ;     ;      ; ;    ;   ;  ;    ;  ;  ;  ;    ;    ;   ;;     ;             ;      ;   ;;  ;    ;   ;  ;   ;   ;;  ;   ;;  ;      
;   ;;;      ;;;;    ;;;;;     ;    ;;; ;;;  ;;;;  ;;; ;; ;  ;;;;;   ;;; ;;  ;;;;;          ;;;      ;;; ;;  ;;;;   ;;  ;;;  ;;; ;;  ;;; ;   ;;;;; 
;                              ;                                                                                                         ;         
;                            ;;;                                                                                                     ;;;;          
;                                                                                                                                                  
;                                                                                                                                                  


(define (install-polynomial-package)
  ;;  manage tower for ordering variables
  (define tower-of-variables
    '(
      x
      y
      z
      X
      Y
      Z
      ))

  (define (get-variable-rank tgt-var)
    (define (gvr tower rank)
      (cond ((null? tower) (error "The Variable is not in hierarchy tower of variables" tgt-var))
            ((eq? tgt-var (car tower)) rank)
            (else (gvr (cdr tower) (+ rank 1)))))
    (gvr tower-of-variables 1))

  
  ;; internal procedures
  ;; representation of poly
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))
  (define (polynomial? p)
    (cond ((not (pair? p)) false)
          ((eq? (type-tag p) 'polynomial) true)
          (else false)))

  ;; representation of terms and term lists
  (define (install-termlist-dense-package)
    (define (tag tlc) (attach-tag 'dense tlc))
    (define (poly-order-of-term-list term-list-content) (- (length term-list-content) 1))
    (define (adjoin-term term term-list-content)
      (if (=zero? (coeff term))
          term-list-content
          (let ((curr-order (poly-order-of-term-list term-list-content)))
            (cond
              ((> (- (order term) 1) curr-order) (adjoin-term term (cons 0 term-list-content)))
              (else (cons (coeff term) term-list-content))))))

    
    (define (first-term term-list-content)
      (list (poly-order-of-term-list term-list-content) (car term-list-content)))
    (define (negate-terms term-list-content)
      (if (null? term-list-content)
          nil
          (cons (do-negate (coeff (first-term term-list-content))) (negate-terms (cdr term-list-content)))))
  
    (put 'adjoin-term '(dense) 
         (lambda (term term-list-content) (tag (adjoin-term term term-list-content))))
    (put 'first-term '(dense) first-term)
    (put 'negate-terms '(dense) 
         (lambda (term-list-content) (tag (negate-terms term-list-content))))
    (print-line 'done))
  (install-termlist-dense-package)
  
  (define (install-termlist-sparse-package)
    (define (tag tlc) (attach-tag 'sparse tlc))
    (define (adjoin-term term term-list-content)
      (if (=zero? (coeff term))
          term-list-content
          (cons term term-list-content)))
    (define (first-term term-list-content) (car term-list-content))
    (define (negate-terms term-list-content)
      (if (null? term-list-content)
          nil
          (cons (make-term (order (first-term term-list-content)) (do-negate (coeff (first-term term-list-content)))) (negate-terms (cdr term-list-content)))))
  
    (put 'adjoin-term '(sparse) 
         (lambda (term term-list-content) (tag (adjoin-term term term-list-content))))
    (put 'first-term '(sparse) first-term)
    (put 'negate-terms '(sparse) 
         (lambda (term-list-content) (tag (negate-terms term-list-content))))
    (print-line 'done))
  (install-termlist-sparse-package)

  (define (density term-list) (type-tag term-list))
  (define (term-list-content term-list) (contents term-list))
  (define (make-term-list tgt-density tgt-term-list-cont) (attach-tag tgt-density tgt-term-list-cont))
  (define (adjoin-term term term-list)
    ((get 'adjoin-term (list (density term-list))) term (term-list-content term-list)))
  (define (the-empty-termlist) (make-term-list 'sparse nil))
  (define (first-term term-list) (apply-generic 'first-term term-list))
  (define (rest-terms term-list) 
    (make-term-list (density term-list) (cdr (term-list-content term-list))))

  (define (empty-termlist? term-list) (null? (term-list-content term-list)))
  (define (negate-terms term-list) (apply-generic 'negate-terms term-list))

  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))

  (define (any->poly var val)
    (tag
     (any->poly-content var val)))
      
  (define (any->poly-content var val)
    (make-poly
     var
     (make-term-list
      'dense
      (list val))))
  

  ;; continued on next page

  (define (canonicalize-poly p)
    (let ((tgt-term-list (term-list p)))
      (if (empty-termlist? tgt-term-list)
          p
          (let ((cano-first-poly (canonicalize-each (variable p) (first-term tgt-term-list))))
            (add-poly
             cano-first-poly
             (canonicalize-poly (make-poly (variable p) (rest-terms tgt-term-list))))))))
              

  (define (canonicalize-each var term)
    (let ((od (order term))
          (ce (coeff term)))
      (if (not (polynomial? ce))
          (make-poly var (adjoin-term term (the-empty-termlist)))
          (let ((cano-ce-cont (canonicalize-poly (contents ce))))
            (let ((rank-of-origin (get-variable-rank var ))
                  (rank-of-ce (get-variable-rank (variable cano-ce-cont))))
              (if (>= rank-of-origin rank-of-ce)
                  (make-poly var (adjoin-term (make-term od (tag cano-ce-cont)) (the-empty-termlist)))
                  (mul-poly
                   cano-ce-cont
                   (any->poly-content (variable cano-ce-cont) (tag (make-poly var (adjoin-term (make-term od 1) (the-empty-termlist))))))))))))
   
  
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1)
                              (term-list p2)))
        (let ((rank-of-p1 (get-variable-rank (variable p1)))
              (rank-of-p2 (get-variable-rank (variable p2))))
          (if (> rank-of-p1 rank-of-p2)
              (add-poly
               p1
               (any->poly-content (variable p1) (tag p2)))             
              (add-poly
               p2
               (any->poly-content (variable p2) (tag p1)))))))
              
                            
  (define (sub-poly p1 p2)
    (add-poly p1 (make-poly (variable p2) (negate-terms (term-list p2)))))
  
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1)
                              (term-list p2)))
        (let ((rank-of-p1 (get-variable-rank (variable p1)))
              (rank-of-p2 (get-variable-rank (variable p2))))
          (if (> rank-of-p1 rank-of-p2)
              (mul-poly
               p1
               (any->poly-content (variable p1) (tag p2)))             
              (mul-poly
               p2
               (any->poly-content (variable p2) (tag p1)))))))
  
  (define (make-div-poly-result q r) (list q r))
  (define quotient-of-div-poly car)
  (define remainder-of-div-poly cadr)
  (define (div-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (let ((tgt-var (variable p1))
              (div-term-result (div-terms (term-list p1)
                                          (term-list p2))))
          (make-div-poly-result (make-poly tgt-var (quotient-of-rst div-term-result))
                                (make-poly tgt-var (remainder-of-rst div-term-result))))
        (error "Polys not in same var -- DIV-POLY"
               (list p1 p2))))

  (define (gcd-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (gcd-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- GCD-POLY"
               (list p1 p2))))
  
  (define (zero-poly? p)
    (define (iter curr-list)
      (cond ((empty-termlist? curr-list) true)
            ((not (=zero? (coeff (first-term curr-list))) ) false)
            (else (iter (rest-terms curr-list)))))
    (iter (term-list p)))

  (define (operate-coeffs op coeff1 coeff2)
    (cond ((and (polynomial? coeff1) (not (polynomial? coeff2))) (op coeff1 (any->poly (variable (contents coeff1)) coeff2)))
          ((and (not (polynomial? coeff1)) (polynomial? coeff2)) (op (any->poly (variable (contents coeff2)) coeff1) coeff2))
          (else (op coeff1 coeff2))))

  (define (add-terms L1 L2)
    (define (add-coeffs coeff1 coeff2) (operate-coeffs add coeff1 coeff2))
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1)) (t2 (first-term L2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term
                     t1 (add-terms (rest-terms L1) L2)))
                   ((< (order t1) (order t2))
                    (adjoin-term
                     t2 (add-terms L1 (rest-terms L2))))
                   (else
                    (adjoin-term
                     (make-term (order t1)
                                (add-coeffs (coeff t1) (coeff t2)))
                     (add-terms (rest-terms L1)
                                (rest-terms L2)))))))))

  
  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))
  (define (mul-term-by-all-terms t1 L)
    (define (mul-coeffs coeff1 coeff2) (operate-coeffs mul coeff1 coeff2))
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t2 (first-term L)))
          (adjoin-term
           (make-term (+ (order t1) (order t2))
                      (mul-coeffs (coeff t1) (coeff t2)))
           (mul-term-by-all-terms t1 (rest-terms L))))))

  (define (make-div-result q r) (list q r))
  (define quotient-of-rst car)
  (define remainder-of-rst cadr)
  (define (div-terms L1 L2)
    (if (empty-termlist? L1)
        (make-div-result (the-empty-termlist) (the-empty-termlist))
        (let ((t1 (first-term L1))
              (t2 (first-term L2)))
          (if (> (order t2) (order t1))
              (list (the-empty-termlist) L1)
              (let ((new-c (div (coeff t1) (coeff t2)))
                    (new-o (- (order t1) (order t2))))
                (let ((rest-of-result
                       (div-terms (add-terms L1 (negate-terms (mul-term-by-all-terms (make-term new-o new-c)  L2))) L2)
                       ))
                  (make-div-result (adjoin-term (make-term new-o new-c) (quotient-of-rst rest-of-result)) (remainder-of-rst rest-of-result))
                  ))))))
  
  (define (remainder-terms L1 L2)
    (remainder-of-rst (div-terms L1 L2)))

  (define (gcd-terms L1 L2)
;    (print-line L1)
;    (print-line L2)
    (if (empty-termlist? L2)
        L1
        (gcd-terms L2 (remainder-terms L1 L2))))
      
  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial) 
       (lambda (p1 p2) (tag (add-poly (canonicalize-poly p1) (canonicalize-poly p2)))))
  (put 'sub '(polynomial polynomial) 
       (lambda (p1 p2) (tag (sub-poly (canonicalize-poly p1) (canonicalize-poly p2)))))
  (put 'mul '(polynomial polynomial) 
       (lambda (p1 p2) (tag (mul-poly (canonicalize-poly p1) (canonicalize-poly p2)))))
  (put 'div '(polynomial polynomial) 
       (lambda (p1 p2)
         (let ((div-poly-result (div-poly p1 p2)))
           (list (tag (quotient-of-div-poly div-poly-result)) (tag (remainder-of-div-poly div-poly-result))))))
         
  (put 'do-negate '(polynomial)
       (lambda (p) (tag (make-poly (variable p) (negate-terms (term-list p))))))
  (put '=zero?  '(polynomial)
       (lambda (p) (zero-poly? p)))

  (put 'do-gcd '(polynomial polynomial)
       (lambda (p1 p2) (tag (gcd-poly p1 p2 ))))
  
  (put 'make-from-dense-list 'polynomial
       (lambda (var terms) (tag (make-poly var (make-term-list 'dense terms)))))
  
  (put 'make-from-sparse-list 'polynomial
       (lambda (var terms) (tag (make-poly var (make-term-list 'sparse terms)))))
       
  (put 'make-term 'polynomial make-term )
  
  'done)


(define (make-polynomial-from-dense-list var terms)
  ((get 'make-from-dense-list 'polynomial) var terms))

(define (make-polynomial-from-sparse-list var terms)
  ((get 'make-from-sparse-list 'polynomial) var terms))

(define (make-term order coeff)
  ((get 'make-term 'polynomial) order coeff))



;                                                                                                                                                                                                          
;                                                                                                                                                                                                          
;                                                                                                                                                                                                          
;                                                                                                                                                                                                          
;   ;;;;;                      ;                              ;;            ;;;;;;                                     ;                            ;;;;;                   ;;                             
;    ;   ;           ;                                         ;             ;   ;                           ;                                       ;   ;                   ;                             
;    ;   ;   ;;;;   ;;;;;    ;;;     ;;;;   ;; ;;    ;;;;      ;             ; ;    ;;  ;;  ;; ;;    ;;; ;  ;;;;;    ;;;     ;;;;   ;; ;;            ;   ;   ;;;;    ;;; ;   ; ;;;;  ;;;;    ;;; ;;  ;;;;  
;    ;   ;  ;    ;   ;         ;    ;    ;   ;;  ;  ;    ;     ;             ;;;     ;   ;   ;;  ;  ;   ;;   ;         ;    ;    ;   ;;  ;           ;   ;  ;    ;  ;   ;;   ;  ;   ;    ;  ;   ;;  ;    ; 
;    ;;;;    ;;;;;   ;         ;    ;    ;   ;   ;   ;;;;;     ;             ; ;     ;   ;   ;   ;  ;        ;         ;    ;    ;   ;   ;           ;;;;    ;;;;;  ;        ;;;     ;;;;;  ;    ;  ;;;;;; 
;    ;  ;   ;    ;   ;         ;    ;    ;   ;   ;  ;    ;     ;             ;       ;   ;   ;   ;  ;        ;         ;    ;    ;   ;   ;           ;      ;    ;  ;        ; ;    ;    ;  ;    ;  ;      
;    ;   ;  ;   ;;   ;   ;     ;    ;    ;   ;   ;  ;   ;;     ;             ;       ;  ;;   ;   ;  ;    ;   ;   ;     ;    ;    ;   ;   ;           ;      ;   ;;  ;    ;   ;  ;   ;   ;;  ;   ;;  ;      
;   ;;;   ;  ;;; ;;   ;;;    ;;;;;   ;;;;   ;;; ;;;  ;;; ;;  ;;;;;          ;;;       ;; ;; ;;; ;;;  ;;;;     ;;;    ;;;;;   ;;;;   ;;; ;;;         ;;;      ;;; ;;  ;;;;   ;;  ;;;  ;;; ;;  ;;; ;   ;;;;; 
;                                                                                                                                                                                                ;         
;                                                                                                                                                                                            ;;;;          
;                                                                                                                                                                                                          
;                                                                                                                                                                                                          
 

(define (install-rational-polynomial-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (cons n d))
  (define (add-rat x y)
    (make-rat (add (mul (numer x) (denom y))
                   (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (sub (mul (numer x) (denom y))
                   (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (mul (numer x) (numer y))
              (mul (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (mul (numer x) (denom y))
              (mul (denom x) (numer y))))
  
  
  (define (tag x) (attach-tag 'rational-poly x))
  (define make (lambda (n d) (tag (make-rat n d))))
  ;; interface to rest of the system
  (put 'add '(rational-poly rational-poly)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational-poly rational-poly)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational-poly rational-poly)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational-poly rational-poly)
       (lambda (x y) (tag (div-rat x y))))
  (put 'do-negate '(rational-poly)
       (lambda (x) (tag (make-rat (do-negate (numer x)) (denom x)))))
       
  (put '=zero? '(rational-poly)
       (lambda (x)  (=zero? (numer x))))
     
  (put 'make 'rational-poly make)
       
  'done)

(define (make-rational-poly n d)
  ((get 'make 'rational-poly) n d))


;---preparing

(install-polar-package)
(install-rectangular-package)
(install-scheme-number-package)
(install-rational-package)
(install-integer-package)
(install-complex-package)
(install-polynomial-package)
(install-rational-polynomial-package)

(define pi (* (asin 1) 2))
(define comp1 (make-complex-from-real-imag 3 4))
(define comp2 (make-complex-from-mag-ang 5 (/ pi 3)))
(define comp3 (make-complex-from-mag-ang 0 (/ pi 3)))
(define comp-droppable1 (make-complex-from-real-imag 10 0))
(define comp-droppable2 (make-complex-from-real-imag 7 0))
(define comp-droppable3 (make-complex-from-real-imag 2.5 0))


(define num1 (make-scheme-number 7.3))
(define num2 (make-scheme-number 3.1))
(define num3 (make-scheme-number 0))
(define num4 (make-scheme-number 2.5))

(define rat1 (make-rational 1 5))
(define rat2 (make-rational 6 35))
(define rat3 (make-rational 1 0))
(define rat4 (make-rational 10 3))
(define rat-droppable1 (make-rational 10 2))

(define int1 (make-integer 12))
(define int2 (make-integer -2))

(define term-x-1 (make-term 0 num1))
(define term-x-2 (make-term 1 int1))
(define term-x-3 (make-term 2 num2))
(define term-x-4 (make-term 0 0))
(define term-x-5 (make-term 1 0))
(define term-x-6 (make-term 0 rat1))
(define term-x-7 (make-term 1 rat2))
(define term-x-8 (make-term 2 2))


(define p1 (make-polynomial-from-dense-list 'x  '(1 -2 1)))
(define p2 (make-polynomial-from-dense-list 'x  '(11 0 7)))
(define p3 (make-polynomial-from-dense-list 'x  '(13 5)))
(define q1 (mul p1 p2))
(define q2 (mul p1 p3))

(define rem1 (make-polynomial-from-dense-list 'x (list (/ 1458  169) (/ -2916  169) (/ 1458  169) )))


(greatest-common-divisor q1 q2)
(div q1 rem1)
(div q2 rem1)

(greatest-common-divisor p2 p3)
(greatest-common-divisor p3 p2)

(define p2-dash (make-polynomial-from-dense-list 'x  (list (/ 1859 1458) 0 (/ 1183 1458))))
(define p3-dash (make-polynomial-from-dense-list 'x  (list (/ 2197 1458) (/ 845 1458))))
(print-line p2-dash)


(greatest-common-divisor (mul p1 p2-dash) (mul p1 p3-dash))