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

          

(define sense-data (in-list '(1 2 1.5 1 0.5 -0.1 -2 -3 -2 -0.5 0.2 3 4)))
(define (sign-change-detector curr prev)
  (cond ((and (< prev 0) (>= curr 0)) 1)
        ((and (>= prev 0) (< curr 0)) -1)
        (else 0)))
(define zero-crossings
  (my-stream-map sign-change-detector 
                 sense-data 
                 (cons-stream 0 sense-data)))
               

(for ((i zero-crossings))
  (printf "~a, " i))