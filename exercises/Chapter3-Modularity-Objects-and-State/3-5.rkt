#lang racket
 
(define (print-line value) 
  (display value) 
  (newline))
(define nil '())


(define (random-in-range low high)
  (+ low (* (random) (- high low))))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(define (circle-range-test x y center-x center-y radius)
  (<= (+ (expt (- x center-x) 2) (expt (- y center-y) 2)) (expt radius 2)))

(define (estimate-integral P x1 x2 y1 y2 trials)
  (let ((area-rect (* (- x2 x1) (- y2 y1))))
    (* area-rect (monte-carlo
                  trials
                  (lambda ()
                    (P
                     (random-in-range x1 x2)
                     (random-in-range y1 y2)))))))

(estimate-integral
 (lambda (x y)
   (circle-range-test x y 5 7 3))
 2.0 8.0
 4 10
 10000)


(estimate-integral
 (lambda (x y)
   (circle-range-test x y 1 1 1))
 0.0 2.0
 0 2
 10000)

(estimate-integral
 (lambda (x y)
   (circle-range-test x y 1 1 1))
 0.0 50.0
 0 50
 10000)