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

(define (RLC R L C dt)
  (λ(vC0 iL0)
    (define vC (integral (delay dvC) vC0 dt))
    (define iL (integral (delay diL) iL0 dt))
    (define dvC (scale-stream iL (/ -1 C)))
    (define diL
      (add-streams
       (scale-stream vC L)
       (scale-stream iL (- (/ R L)))))
    (cons vC iL)))

(define RLC1 (RLC 1 0.2 1 0.1))

(define RLC1-1 (RLC1 0 10))
(for ((i (in-range 0 10)))
  (printf "(~a, ~a), " (stream-ref (car RLC1-1) i) (stream-ref (cdr RLC1-1) i)))
(newline)
