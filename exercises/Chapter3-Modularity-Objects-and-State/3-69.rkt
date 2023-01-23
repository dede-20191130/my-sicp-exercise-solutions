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
  (cons-stream
   (list (stream-car s) (stream-car t))
   (stream-lazy (interleave
                 (stream-map (lambda (x) 
                               (list (stream-car s) x))
                             (stream-cdr t))
                 (pairs (stream-cdr s) (stream-cdr t))))))


(define (triples s t u)
  (cons-stream
   (list (stream-car s) (stream-car t) (stream-car u))
   (stream-lazy (interleave
                 (stream-map (lambda (uk) (list (stream-car s) (stream-car t) uk))
                             (stream-cdr u))
                 (interleave
                  (stream-map (lambda (tjuk) (cons (stream-car s) tjuk))
                              (pairs (stream-cdr t) (stream-cdr u)))
                  (triples (stream-cdr s) (stream-cdr t) (stream-cdr u)))))))

(define (square x)(* x x))
(define pythagorean-triples
  (stream-filter
   (λ(lst)
     (let ((i (car lst))
           (j (cadr lst))
           (k (caddr lst)))
       (= (+ (square i) (square j)) (square k))))
   (triples (in-naturals 1) (in-naturals 1) (in-naturals 1))))

(define int-triples (triples (in-naturals 1) (in-naturals 1) (in-naturals 1)))


(for ((i (in-range 0 30)))
  (printf "~a, " (stream-ref int-triples i)))
(newline)

(for ((i (in-range 0 4)))
  (printf "~a, " (stream-ref pythagorean-triples i)))
(newline)
