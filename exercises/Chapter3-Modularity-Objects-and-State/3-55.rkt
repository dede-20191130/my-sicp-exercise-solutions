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

(define (add-streams s1 s2) 
  (my-stream-map + s1 s2))

(define (partial-sums s)
  (define summed-s
    (cons-stream (stream-car s) (stream-lazy (add-streams summed-s (stream-cdr s)))))
  summed-s)

(define fibs2
  (cons-stream 
   0 (stream-lazy (cons-stream
                   1 (stream-lazy (add-streams 
                                   (stream-cdr fibs2) fibs2))))))

(define partial-sums-integers (partial-sums integers))
(define partial-sums-fibs2 (partial-sums fibs2))

(for ((i '(0 1 2 3 4 5)))
  (println(stream-ref partial-sums-integers i)))

(for ((i '(0 1 2 3 4 5)))
  (println(stream-ref partial-sums-fibs2 i)))
