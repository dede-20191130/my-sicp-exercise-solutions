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


(define (integrate-series power-series)
  (define result-stream
    (cons-stream (stream-car power-series)
                 (stream-lazy (my-stream-map
                               (λ(orig-coeff n) (/ orig-coeff n))
                               (stream-cdr power-series)
                               (in-naturals 2)))))
  result-stream)

(define ps01 (in-list '(1 3 2)))
(define ps02 (in-list '(5 6 7 14 13)))
(define integrated-ps01-without-const (integrate-series ps01))
(define integrated-ps02-without-const (integrate-series ps02))

(stream->list integrated-ps01-without-const)
(stream->list integrated-ps02-without-const)
                               