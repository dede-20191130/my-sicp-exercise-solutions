#lang sicp

(define (%require p)
  (if (not p) (amb)))

(define (an-integer-between lb hb)
  (%require (< lb hb))
  (amb lb
       (an-integer-between (+ lb 1) hb)))

(define (an-integer-between-desc lb hb)
  (%require (< lb hb))
  (amb hb
       (an-integer-between-desc lb (- hb 1))))

(define (an-integer-starting-from n)
  (amb n (an-integer-starting-from (+ n 1))))

(define (a-pythagorean-triple)
  (let ((k (an-integer-starting-from 1)))
    (let ((j (an-integer-between-desc 0 k)))
      (let ((i (an-integer-between-desc 0 j)))
        (%require (= (+ (* i i) (* j j)) 
                     (* k k)))
        (list i j k)))))

;;;test

(a-pythagorean-triple)

(amb)
(amb)
(amb)
(amb)


