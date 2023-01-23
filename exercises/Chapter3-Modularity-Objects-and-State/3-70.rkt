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

(define (weighted-pairs s t calculate-weight)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (stream-lazy (calculate-weight
                 (stream-map (lambda (x) 
                               (list (stream-car s) x))
                             (stream-cdr t))
                 (weighted-pairs (stream-cdr s) (stream-cdr t) calculate-weight)))))




(define (merge-weighted s1 s2 weight)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (let ((weighing (weight s1car s2car)))
             (cond (weighing
                    (cons-stream 
                     s1car 
                     (stream-lazy (merge-weighted
                                   (stream-cdr s1) 
                                   s2
                                   weight))))
                   (else
                    (cons-stream 
                     s2car 
                     (stream-lazy (merge-weighted
                                   s1 
                                   (stream-cdr s2)
                                   weight))))))))))

(define (order-by-sum lst-a lst-b)
  (let ((sum-a (foldl + 0 lst-a))
        (sum-b (foldl + 0 lst-b)))
    (<= sum-a sum-b)))

(define (order-by-2-3-5 lst-a lst-b)
  (define (calc-2-3-5 i j)
    (+ (* i 2) (* j 3) (* i j 5)))
  (let ((calced-a (calc-2-3-5 (car lst-a) (cadr lst-a)))
        (calced-b (calc-2-3-5 (car lst-b) (cadr lst-b))))
    (<= calced-a calced-b)))

(define (divisible? x y) (= (remainder x y) 0))
(define int-without-multi-2-3-5
  (stream-filter
   (λ(n) (not
          (or
           (divisible? n 2)
           (divisible? n 3)
           (divisible? n 5))))
   (in-naturals 1)))


(define int-pairs01 (weighted-pairs (in-naturals 1) (in-naturals 1) interleave))
(define int-pairs02 (weighted-pairs
                     (in-naturals 1)
                     (in-naturals 1)
                     (λ(s1 s2)
                       (merge-weighted s1 s2 order-by-sum))))

(define int-pairs-without-multi-2-3-5 (weighted-pairs
                     int-without-multi-2-3-5
                     int-without-multi-2-3-5
                     (λ(s1 s2)
                       (merge-weighted s1 s2 order-by-2-3-5))))


(for ((i (in-range 0 20)))
  (printf "~a, " (stream-ref int-pairs02 i)))
(newline)

(for ((i (in-range 0 25)))
  (printf "~a, " (stream-ref int-without-multi-2-3-5 i)))
(newline)

(for ((i (in-range 0 20)))
  (printf "~a, " (stream-ref int-pairs-without-multi-2-3-5 i)))
(newline)
                            



