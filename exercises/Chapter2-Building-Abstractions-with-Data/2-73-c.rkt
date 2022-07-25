#lang sicp

(define (print-line value) 
  (display value) 
  (newline))

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

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp)
                                           var))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (install-deriv-sum)
  (define (addend s) (car s))
  (define (augend s) 
    (if (null? (cddr s))
        (cadr s)
        (cons '+ (cdr s))))
  
  (put
   'deriv
   '+
   (lambda (exp var)
     (make-sum
      (deriv (addend exp) var) 
      (deriv (augend exp) var))
     ))
  'done)

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (car p))
(define (multiplicand p) 
  (if (null? (cddr p))
      (cadr p)
      (cons '* (cdr p))))
(define (make-product m1 m2)
  (let ((mm1 (if (product? m1)
                 (make-product (multiplier (cdr m1)) (multiplicand (cdr m1)))
                 m1))
        (mm2 (if (product? m2)
                 (make-product (multiplier (cdr m2)) (multiplicand (cdr m2)))
                 m2)))
    (cond 
      ((or (=number? mm1 0) (=number? mm2 0)) 0)
      ((=number? mm1 1) mm2)
      ((=number? mm2 1) mm1)
      ((and (number? mm1) (number? mm2)) (* mm1 mm2))
      (else (list '* mm1 mm2)))))


(define (install-deriv-product)
 
  (put
   'deriv
   '*
   (lambda (exp var)
     (make-sum
      (make-product (multiplier exp)
                    (deriv (multiplicand exp) var))
      (make-product (deriv (multiplier exp) var)
                    (multiplicand exp))))
   )
  'done)

(define (install-deriv-exponentiation)
  (define (base p) (car p))
  (define (exponent p) (cadr p))
  (define (make-exponentiation b e)
    (cond ((and (number? b) (number? e)) (expt b e))
          ((=number? e 0) 1)
          ((=number? e 1) b)
          (else (list '** b e))))

  (define (d-exponent b e v)
    (cond ((not (same-variable? b v)) 0)
          ((=number? e 0) 0)
          (else (make-product e (make-exponentiation b (- e 1))))))
  (put
   'deriv
   '**
   (lambda (exp var)
     (d-exponent (base exp) (exponent exp) var)
     ))
  'done)

(install-deriv-sum)
(install-deriv-product)
(install-deriv-exponentiation)

(print-line (deriv '(+ x 3) 'x))
(print-line (deriv '(+ x x x) 'x))
(print-line (deriv '(+ 3 6 7) 'x))
(print-line (deriv '(+ x 3 5) 'x))
(print-line (deriv '(+ x x y 5) 'x))
(print-line (deriv '(* x 5) 'x))
(print-line (deriv '(* x y 1) 'x))
(print-line (deriv '(* x y x 5 7) 'x))
(print-line (deriv '(* x y (+ x 3)) 'x))

(print-line (deriv '(** x 0) 'x))
(print-line (deriv '(** x 1) 'x))
(print-line (deriv '(** x 2) 'x))
(print-line (deriv '(** x 3) 'x))
(print-line (deriv '(** x 4) 'x))
(print-line (deriv '(** x 20) 'x))
(print-line (deriv '(+ x x y 1 2 3 (** x 2)) 'x))
(print-line (deriv '(+ x x y 1 2 3 (** x 2)) 'y))