#lang racket

(define stream-null? stream-empty?)
(define the-empty-stream empty-stream)
(define stream-car stream-first)
(define stream-cdr stream-rest)
(define (cons-stream first-expr rest-expr)
  (stream-cons first-expr rest-expr))
(define display-line println)

(define (my-stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (my-stream-ref (stream-cdr s) (- n 1))))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-lazy (stream-enumerate-interval (+ low 1) high)))))

  (define (my-stream-map-single proc s)
    (if (stream-null? s)
        the-empty-stream
        (cons-stream 
         (proc (stream-car s))
         (stream-lazy (my-stream-map-single proc (stream-cdr s))))))

  (define (show x)
    (display-line x)
    x)

  (define x 
    (my-stream-map-single
     show 
     (stream-enumerate-interval 0 10)))

  (my-stream-ref x 5)
  (my-stream-ref x 7)