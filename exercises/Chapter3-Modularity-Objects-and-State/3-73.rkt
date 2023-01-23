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

(define (RC R C dt)
  (λ(i-stm ini-v)
    (define capacitor-v
      (cons-stream
       ini-v
       (stream-lazy
        (add-streams
         (scale-stream (scale-stream i-stm dt) (/ 1 C))
         capacitor-v))))
    (if (stream-null? i-stm)
        the-empty-stream
        (cons-stream
         (stream-car capacitor-v)
         (stream-lazy
          (add-streams
           (stream-cdr capacitor-v)
           (scale-stream i-stm R)))))))

(define RC1 (RC 5 1 0.5))
(define i-stream01 (stream-map (λ(x) (+ x 1)) (scale-stream (in-naturals 0) 0.3)))

(for ((i (in-range 0 10)))
  (printf "~a, " (stream-ref (RC1 i-stream01 10) i)))
(newline)
