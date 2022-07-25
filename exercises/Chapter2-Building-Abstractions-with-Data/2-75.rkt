#lang sicp
 

(define (print-line value) 
  (display value) 
  (newline))

(define (square x) (* x x))

(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) x)
          ((eq? op 'imag-part) y)
          ((eq? op 'magnitude)
           (sqrt (+ (square x) (square y))))
          ((eq? op 'angle) (atan y x))
          (else
           (error "Unknown op -- MAKE-FROM-REAL-IMAG" op))))
  dispatch)

(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond ((eq? op 'real-part)     (* r (cos a)))
          ((eq? op 'imag-part)     (* r (sin a)))
          ((eq? op 'magnitude )     r)
          ((eq? op 'angle)     a)
          (else
           (error "Unknown op -- MAKE-FROM-MAG-ANG" op))))
  dispatch)

(define (apply-generic op arg) (arg op))

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))


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



(define pi (* (asin 1) 2))
(define cmp-n-1 (make-from-real-imag 3 4))
(define cmp-n-2 (make-from-mag-ang 6 (/ pi 6)))

(print-line (real-part cmp-n-1 ))
(print-line (real-part(sub-complex cmp-n-2 cmp-n-1 )))
(print-line (imag-part(sub-complex cmp-n-2 cmp-n-1 )))
(print-line (mul-complex cmp-n-1 cmp-n-2 ))