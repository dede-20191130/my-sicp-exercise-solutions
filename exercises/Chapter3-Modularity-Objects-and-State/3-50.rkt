#lang racket

(define stream-null? stream-empty?)
(define the-empty-stream empty-stream)
(define stream-car stream-first)
(define stream-cdr stream-rest)
(define (cons-stream first-expr rest-expr)
  (stream-cons first-expr rest-expr))

;single ver.
(define (my-stream-map-single proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream 
       (proc (stream-car s))
       (my-stream-map-single proc (stream-cdr s)))))

(define (my-stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map
                    (λ(tgt-stream) (stream-car tgt-stream))
                    argstreams))
       (apply my-stream-map
              (cons proc 
                    (map
                     (λ(tgt-stream) (stream-cdr tgt-stream))
                     argstreams))))))

(define (display-stream s)
  (println (stream->list s)))


(define stm01 (stream-lazy (in-range 11 16)))
(define stm02 (stream-lazy (in-list '(5 10 15 20 25))))
(define stm03 (stream-lazy (in-naturals)))

(display-stream (my-stream-map-single (λ(x) (+ x 100)) stm01))
(display-stream (my-stream-map (λ(x y z) (- x y z)) stm01 stm02 stm03))