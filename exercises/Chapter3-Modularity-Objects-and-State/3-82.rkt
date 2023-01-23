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

(define (monte-carlo experiment-stream 
                     passed 
                     failed)
  (define (next passed failed)
    (cons-stream
     (/ passed (+ passed failed))
     (stream-lazy
      (monte-carlo
       (stream-cdr experiment-stream) 
       passed 
       failed))))
  (if (stream-car experiment-stream)
      (next (+ passed 1) failed)
      (next passed (+ failed 1))))

(define (random-numbers-in-range low high)
  (let ((inner-prg (make-pseudo-random-generator)))
    (define (rand-nums)
      (cons-stream
       (+ low (* (random inner-prg) (- high low)))
       (stream-lazy
        (rand-nums))))
    (rand-nums)))

(define (circle-range-test x y center-x center-y radius)
  (<= (+ (expt (- x center-x) 2) (expt (- y center-y) 2)) (expt radius 2)))



(define (estimate-integral P x1 x2 y1 y2)
  (define (P-stream xs ys)
    (cons-stream
     (P (stream-car xs) (stream-car ys))
     (stream-lazy
      (P-stream (stream-cdr xs) (stream-cdr ys)))))
    
  (let ((area-rect (* (- x2 x1) (- y2 y1)))
        (x-range-numbers (random-numbers-in-range x1 x2))
        (y-range-numbers (random-numbers-in-range y1 y2)))
    (scale-stream
     (monte-carlo (P-stream x-range-numbers y-range-numbers) 0 0)
     area-rect)))


(define ei-stm01
  (estimate-integral
   (lambda (x y)
     (circle-range-test x y 5 7 3))
   2.0 8.0
   4 10))

(define ei-stm02
  (estimate-integral
   (lambda (x y)
     (circle-range-test x y 1 1 1))
   0.0 2.0
   0.0 2.0))

(for ((i '(1 10 100 1000 3000)))
  (printf "~a, " (stream-ref ei-stm01 i)))
(newline)

(for ((i '(1 10 100 1000 3000)))
  (printf "~a, " (stream-ref ei-stm02 i)))
