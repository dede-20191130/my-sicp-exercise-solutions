#lang sicp

(define (%require p)
  (if (not p) (amb)))

(define (an-integer-between lb hb)
  (%require (< lb hb))
  (amb lb
       (an-integer-between (+ lb 1) hb)))

(define (a-pythagorean-triple-between low high)
  (let ((i (an-integer-between low high)))
    (let ((j (an-integer-between i high)))
      (let ((k (an-integer-between j high)))
        (%require (= (+ (* i i) (* j j)) 
                    (* k k)))
        (list i j k)))))

;;;test

(an-integer-between 10 15)

(a-pythagorean-triple-between 3 5)
(a-pythagorean-triple-between 3 6)
(a-pythagorean-triple-between 4 15)