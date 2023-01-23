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

(define (add-streams s1 s2) 
  (my-stream-map + s1 s2))

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
                 

(define (mul-series s1 s2)
  (cons-stream
   (* (stream-car s1) (stream-car s2))
   (stream-lazy (add-streams
                 (add-streams
                  (scale-stream
                   (stream-cdr s1)
                   (stream-car s2))
                  (scale-stream
                   (stream-cdr s2)
                   (stream-car s1)))
                 (cons-stream 0
                              (stream-lazy (mul-series
                                            (stream-cdr s1)
                                            (stream-cdr s2))))))))

(define zeros (cons-stream 0 (stream-lazy zeros)))

(define stm01 (stream-append (in-list '(1 2 3)) zeros))
(define stm02 (stream-append (in-list '(-2 10 12)) zeros))

(define squared-sine
  (mul-series sine-series sine-series))

(define squared-cosine
  (mul-series cosine-series cosine-series))

(for ((i '(0 1 2 3 4 5 6 7)))
  (printf "~a, " (stream-ref (mul-series stm01 stm02) i)))
(newline)

(for ((i '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14)))
  (printf "~a, " (stream-ref (add-streams squared-sine squared-cosine) i)))
(newline)