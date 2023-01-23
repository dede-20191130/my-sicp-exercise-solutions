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

(define (partial-sums s)
  (define summed-s
    (cons-stream (stream-car s) (stream-lazy (add-streams summed-s (stream-cdr s)))))
  summed-s)

(define (scale-stream stream factor)
  (stream-map
   (lambda (x) (* x factor))
   stream))

(define (pi-summands n)
  (cons-stream 
   (/ 1.0 n)
   (stream-lazy (stream-map - (pi-summands (+ n 2))))))

(define pi-stream
  (scale-stream 
   (partial-sums (pi-summands 1)) 4))

(define (ln2-summands n)
  (cons-stream
     (/ 1.0 n)
     (stream-lazy (scale-stream (ln2-summands (+ n 1)) -1))))

(define ln2-stream
   (partial-sums (ln2-summands 1)))

(define (square x) (* x x))

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))     ; Sn-1
        (s1 (stream-ref s 1))     ; Sn
        (s2 (stream-ref s 2)))    ; Sn+1
    (cons-stream 
     (- s2 (/ (square (- s2 s1))
              (+ s0 (* -2 s1) s2)))
     (stream-lazy (euler-transform (stream-cdr s))))))

(define (make-tableau transform s)
  (cons-stream 
   s
   (stream-lazy (make-tableau
                 transform
                 (transform s)))))

(define (accelerated-sequence transform s)
  (stream-map stream-car
              (make-tableau transform s)))

(for ((i (in-range 0 10)))
  (println  (stream-ref ln2-stream i)))
(newline)

(for ((i (in-range 0 10)))
  (println  (stream-ref (accelerated-sequence euler-transform ln2-stream) i)))
(newline)