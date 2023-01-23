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


(define (calc-cube-sum lst)
  (foldl (λ(x rslt) (+ (* x x x) rslt)) 0 lst))

(define (order-by-cube-sum lst-a lst-b)
  (let ((sum-a (calc-cube-sum lst-a))
        (sum-b (calc-cube-sum lst-b)))
    (<= sum-a sum-b)))

(define int-pairs-ordered-cube-sum
  (weighted-pairs
   (in-naturals 1)
   (in-naturals 1)
   (λ(s1 s2)
     (merge-weighted s1 s2 order-by-cube-sum))))

(define (pairs-Ramanujan pairs-stream)
  (let ((lst-s0 (stream-ref pairs-stream 0))
        (lst-s1 (stream-ref pairs-stream 1)))
    (let ((cube-sum-s0 (calc-cube-sum lst-s0))
          (cube-sum-s1 (calc-cube-sum lst-s1)))
      (if (= cube-sum-s1 cube-sum-s0)
          (cons-stream
           cube-sum-s0
           (stream-lazy (pairs-Ramanujan (stream-cdr pairs-stream))))
          (stream-lazy (pairs-Ramanujan (stream-cdr pairs-stream)))))))
                            
(for ((i (in-range 0 20)))
  (printf "~a, " (stream-ref int-pairs-ordered-cube-sum i)))
(newline)

(for ((i (in-range 0 6)))
  (printf "~a, " (stream-ref (pairs-Ramanujan int-pairs-ordered-cube-sum) i)))
(newline)


