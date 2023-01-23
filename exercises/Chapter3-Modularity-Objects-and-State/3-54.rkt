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

(define integers (in-naturals 1))

(define (mul-streams s1 s2) 
  (my-stream-map * s1 s2))

(define factorials 
  (cons-stream 1 (stream-lazy (mul-streams  (stream-cdr integers) factorials))))

(for ((i '(0 1 2 3 4 5 6 7 8 9)))
  (printf "~a, " (stream-ref factorials i)))
(newline)