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

          

(define sense-data (in-list '(1 2 1.5 1 0.5 -0.1 -2 -3 -2 -0.5 0.2 3 4)))
(define (sign-change-detector curr prev)
  (cond ((and (< prev 0) (>= curr 0)) 1)
        ((and (>= prev 0) (< curr 0)) -1)
        (else 0)))

(define (smooth s)
  (define (helper inner-s last-value)
    (if (stream-null? inner-s)
        the-empty-stream
        (let ((avpt 
               (/ (+ (stream-car inner-s) 
                     last-value) 
                  2)))
          (cons-stream
           avpt
           (stream-lazy
            (helper (stream-cdr inner-s) (stream-car inner-s)))))))
  (helper s (stream-car s)))

(define (make-zero-crossings 
         input-stream last-value)
  (if (stream-null? input-stream)
      the-empty-stream
      (cons-stream 
       (sign-change-detector 
        (stream-car input-stream) 
        last-value)
       (stream-lazy(make-zero-crossings 
                    (stream-cdr input-stream) (stream-car input-stream) )))))

(define zero-crossings 
  (make-zero-crossings
   (smooth sense-data)
   0))


(for ((i zero-crossings))
  (printf "~a, " i))
