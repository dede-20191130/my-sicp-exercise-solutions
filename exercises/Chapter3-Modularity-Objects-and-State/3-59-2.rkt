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

(define (mul-streams s1 s2) 
  (my-stream-map * s1 s2))

(define (scale-stream stream factor)
  (stream-map
   (lambda (x) (* x factor))
   stream))

(define cosine-series 
  (cons-stream 1
               (stream-lazy
                (integrate-series
                 (scale-stream
                  sine-series
                  -1)))))
                 

(define sine-series
  (cons-stream 0
               (stream-lazy
                (integrate-series cosine-series))))
                 


(for ((i '(0 1 2 3 4 5 6 7)))
  (printf "~a, " (stream-ref cosine-series i)))
(newline)
(for ((i '(0 1 2 3 4 5 6 7)))
  (printf "~a, " (stream-ref sine-series i)))
(newline)