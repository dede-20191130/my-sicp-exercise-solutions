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

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream 
       (stream-car s1)
       (stream-lazy(interleave s2 (stream-cdr s1))))))

(define (pairs s t)
  (define (raw-pairs ss tt)
    (cons-stream
     (list (stream-car ss) (stream-car tt))
     (stream-lazy (interleave
                   (stream-map (lambda (x) 
                                 (list (stream-car ss) x))
                               (stream-cdr tt))
                   (raw-pairs (stream-cdr ss) (stream-cdr tt))))))
  (stream-map
   (λ(lst)(reverse lst))
   (stream-filter
    (λ(lst)
      (not (= (car lst) (cadr lst))))
    (raw-pairs s t))))

(define int-pairs (pairs (in-naturals 1) (in-naturals 1)))


(for ((i (in-range 0 20)))
  (printf "~a, " (stream-ref int-pairs i)))
(newline)
