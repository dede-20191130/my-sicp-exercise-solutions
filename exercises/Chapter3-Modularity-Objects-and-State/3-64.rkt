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


(define (stream-limit s tolerance)
  (let ((front (stream-ref s 0))
        (rear (stream-ref s 1)))
    (if (> tolerance (abs (- front rear)))
        rear
        (stream-limit (stream-cdr s) tolerance))))

(define (average a b)(/ (+ a b) 2))
(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream 
     1.0
     (stream-lazy (stream-map
          (lambda (guess)
            (sqrt-improve guess x))
          guesses))))
  guesses)

(define (my-sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

(my-sqrt 5 0.1)
(my-sqrt 5 0.0001)