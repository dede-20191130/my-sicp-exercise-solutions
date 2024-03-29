#lang racket

(define stream-null? stream-empty?)
(define the-empty-stream empty-stream)
(define stream-car stream-first)
(define stream-cdr stream-rest)
(define (cons-stream first-expr rest-expr)
  (stream-cons first-expr rest-expr))

(define (my-stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map
                    (λ(tgt-stream) (stream-car tgt-stream))
                    argstreams))
       (stream-lazy (apply my-stream-map
                           (cons proc 
                                 (map
                                  (λ(tgt-stream) (stream-cdr tgt-stream))
                                  argstreams)))))))

(define (add-streams s1 s2) 
  (my-stream-map + s1 s2))

(define (scale-stream stream factor)
  (stream-map
   (lambda (x) (* x factor))
   stream))

(define (integral
         delayed-integrand initial-value dt)
  (define int
    (cons-stream 
     initial-value
     (stream-lazy
      (let ((integrand 
             (force delayed-integrand)))
        (add-streams 
         (scale-stream integrand dt)
         int)))))
  int)

(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)
     
(define (solve-general-2nd f y0 dy0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (my-stream-map f dy y))
  y)

(stream-ref 
 (solve-general-2nd (λ(dy y) (+ (* dy 2) (* y 3))) 3 5 0.001) 1000)

(stream-ref 
 (solve-general-2nd (λ(dy y) (* dy y)) 0 2 0.00001) 100000)