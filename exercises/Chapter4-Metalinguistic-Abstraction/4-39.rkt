#lang sicp

(define (%require p)
  (if (not p) (amb)))

(define (distinct? items)
  (cond ((null? items) true)
        ((null? (cdr items)) true)
        ((member (car items) (cdr items)) false)
        (else (distinct? (cdr items)))))

(define (multiple-dwelling-in-text)
  (let ((baker (amb 1 2 3 4 5))
        (cooper (amb 1 2 3 4 5))
        (fletcher (amb 1 2 3 4 5))
        (miller (amb 1 2 3 4 5))
        (smith (amb 1 2 3 4 5)))
    (%require
     (distinct? (list baker cooper fletcher 
                      miller smith)))
    (%require (not (= baker 5)))
    (%require (not (= cooper 1)))
    (%require (not (= fletcher 5)))
    (%require (not (= fletcher 1)))
    (%require (> miller cooper))
    (%require
     (not (= (abs (- smith fletcher)) 1)))
    (%require 
     (not (= (abs (- fletcher cooper)) 1)))
    (list (list 'baker baker)
          (list 'cooper cooper)
          (list 'fletcher fletcher)
          (list 'miller miller)
          (list 'smith smith))))

;distinct? is time consuming, so it should be placed after other tests.
(define (multiple-dwelling-improved)
  (let ((baker (amb 1 2 3 4 5))
        (cooper (amb 1 2 3 4 5))
        (fletcher (amb 1 2 3 4 5))
        (miller (amb 1 2 3 4 5))
        (smith (amb 1 2 3 4 5)))
    
    (%require (not (= baker 5)))
    (%require (not (= cooper 1)))
    (%require (not (= fletcher 5)))
    (%require (not (= fletcher 1)))
    (%require 
     (not (= (abs (- fletcher cooper)) 1)))
    (%require (> miller cooper))
    (%require
     (not (= (abs (- smith fletcher)) 1)))
    (%require
     (distinct? (list baker cooper fletcher 
                      miller smith)))
    (list (list 'baker baker)
          (list 'cooper cooper)
          (list 'fletcher fletcher)
          (list 'miller miller)
          (list 'smith smith))))

;;;test

(define stt false)
(define end false)

(set! stt (runtime))
(let go ((i 1))
  (if (> i 100)
      'ok
      (begin
        (multiple-dwelling-in-text)
        (go (+ i 1)))))
(set! end (runtime))
(display "process in text:")
(display (-  end stt))
(newline)


(set! stt (runtime))
(let go ((i 1))
  (if (> i 100)
      'ok
      (begin
        (multiple-dwelling-improved)
        (go (+ i 1)))))
(set! end (runtime))
(display "process improved:")
(display (-  end stt))
(newline)