#lang racket

;;;factorial
((lambda (n)
   ((lambda (fact) (fact fact n))
    (lambda (ft k)
      (if (= k 1)
          1
          (* k (ft ft (- k 1)))))))
 10)

;;;fibonacci
((lambda(n)
   ((lambda (fact) (fact fact n))
    (lambda(ft k)
      (cond((= k 0) 0)
           ((= k 1) 1)
           (else (+ (ft ft (- k 1)) (ft ft (- k 2))))))))
 10)


;;;parity
(define (f x)
  ((lambda (even? odd?)
     (even? even? odd? x))
   (lambda (ev? od? n)
     (if (= n 0) 
         true 
         (od? ev? od? (- n 1))))
   (lambda (ev? od? n)
     (if (= n 0) 
         false 
         (ev? ev? od? (- n 1))))))

;;;test
(f 2)
(f 3)
(f 5)
(f 7)
(f 2000)