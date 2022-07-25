#lang racket 
 
(define (print-line value) 
  (display value) 
  (newline))
(define nil '())

(define (square x) (* x x))

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


  (define (helper arg-list)
    (let ((type-tags (map type-tag arg-list)))
      (let ((proc (get op type-tags)))
        (if proc
            (apply proc (map contents arg-list))
            (cond ((< (length arg-list) 2) (error "No method for these types"
                                                  (list op type-tags)))
                  ((all-same-type? type-tags (car type-tags)) (helper (map raise arg-list)))
                  (else
                   (let ((highest-type (detect-highest-type type-tags)))
                     (helper (map
                              (lambda (tgt-data) (raise-to-highest-type tgt-data highest-type))
                              arg-list)))))))))

  (helper args))



(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? x) (apply-generic '=zero? x))
(define (exp x y) (apply-generic 'exp x y))
(define (raise x) (apply-generic 'raise x))


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
  
  (put 'add '(scheme-number scheme-number)
       (lambda (x y)  (+ x y)))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y)  (- x y)))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y)  (* x y)))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y)  (/ x y)))
  (put 'equ? '(scheme-number scheme-number)
       (lambda (x y) (= x y)))
  (put '=zero? '(scheme-number)
       (lambda (x) (= x 0)))
  ;; following added to Scheme-number package
  (put 'exp '(scheme-number scheme-number)
       (lambda (x y)  (expt x y))) ; using primitive expt
  (put 'raise '(scheme-number)
       (lambda (x) ((get 'make-from-real-imag 'complex) x 0)))
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
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'equ? '(rational rational)
       (lambda (x y) (and (= (denom x) (denom y) (= (numer x) (numer y))))))
  (put '=zero? '(rational)
       (lambda (x) (and (not (= (denom x) 0)) (= (numer x) 0))))
  (put 'raise '(rational)
       (lambda (x) ((get 'make 'scheme-number) (/ (numer x) (denom x)))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)
(define (make-rational n d)
  ((get 'make 'rational) n d))



(define (install-complex-package)
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
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'equ? '(complex complex)
       (lambda (z1 z2) (and (= (real-part z1) (real-part z2)) (= (imag-part z1) (imag-part z2)))))
  (put '=zero? '(complex)
       (lambda (z) (and (= (real-part z) 0) (= (imag-part z) 0))))
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

(define (scheme-number->complex n)
  (make-complex-from-real-imag (contents n) 0))

(define (scheme-number->rational n)
  (make-rational n 1))




;---preparing

(install-polar-package)
(install-rectangular-package)
(install-scheme-number-package)
(install-rational-package)
(install-integer-package)
(install-complex-package)

(define pi (* (asin 1) 2))
(define comp1 (make-complex-from-real-imag 3 4))
(define comp2 (make-complex-from-mag-ang 5 (/ pi 3)))
(define comp3 (make-complex-from-mag-ang 0 (/ pi 3)))


(define num1 (make-scheme-number 7))
(define num2 (make-scheme-number 3))
(define num3 (make-scheme-number 0))

(define rat1 (make-rational 1 5))
(define rat2 (make-rational 6 35))
(define rat3 (make-rational 1 0))
(define rat4 (make-rational 10 3))

(define int1 (make-integer 12))
(define int2 (make-integer -2))

(print-line (add int1 int2))
(print-line (add int1 rat2))
(print-line (sub num1 comp1))
(print-line (div comp2 int1))
(print-line (exp int1 int2))
(print-line (exp rat1 rat4))

