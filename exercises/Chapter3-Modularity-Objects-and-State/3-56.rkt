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

(define (scale-stream stream factor)
  (stream-map
   (lambda (x) (* x factor))
   stream))

(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (cons-stream 
                   s1car 
                   (stream-lazy (merge (stream-cdr s1) 
                                       s2))))
                 ((> s1car s2car)
                  (cons-stream 
                   s2car 
                   (stream-lazy (merge s1 
                                       (stream-cdr s2)))))
                 (else
                  (cons-stream 
                   s1car
                   (stream-lazy (merge 
                                 (stream-cdr s1)
                                 (stream-cdr s2))))))))))

(define s-factor-2-3 (cons-stream 1 (stream-lazy (merge (scale-stream s-factor-2-3 2) (scale-stream s-factor-2-3 3)))))
(define s-factor-2-3-5 (cons-stream 1 (stream-lazy (merge
                                                    (merge
                                                     (scale-stream s-factor-2-3-5 2)
                                                     (scale-stream s-factor-2-3-5 3))
                                                    (scale-stream s-factor-2-3-5 5)))))


(for ((i '(0 1 2 3 4 5 6 7 8 9)))
  (printf "~a, " (stream-ref s-factor-2-3 i)))
(newline)


(for ((i '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14)))
  (printf "~a, " (stream-ref s-factor-2-3-5 i)))
(newline)

