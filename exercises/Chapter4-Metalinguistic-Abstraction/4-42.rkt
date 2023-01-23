#lang sicp

(define (%require p)
  (if (not p) (amb)))

(define (distinct? items)
  (cond ((null? items) true)
        ((null? (cdr items)) true)
        ((member (car items) (cdr items)) false)
        (else (distinct? (cdr items)))))


(define (liars-puzzle)
  (let ((ethel (amb 1 2 3 4 5))
        (sttmt-of-ethel (amb (lambda(e j) (and (= e 1) (not (= j 2))))
                             (lambda(e j) (and (= j 2) (not (= e 1))))))
        (joan (amb 1 2 3 4 5))
        (sttmt-of-joan (amb (lambda(e j) (and (= j 3) (not (= e 5))))
                            (lambda(e j) (and (= e 5) (not (= j 3)))))))
    (%require (sttmt-of-ethel ethel joan))
    (%require (sttmt-of-joan ethel joan))
    (let ((betty (amb 1 2 3 4 5))
          (sttmt-of-betty (amb (lambda(b k) (and (= k 2) (not (= b 3))))
                               (lambda(b k) (and (= b 3) (not (= k 2))))))
          (kitty (amb 1 2 3 4 5))
          (sttmt-of-kitty (amb (lambda(k m) (and (= k 2) (not (= m 4))))
                               (lambda(k m) (and (= m 4) (not (= k 2))))))
          (mary (amb 1 2 3 4 5))
          (sttmt-of-mary (amb (lambda(b m) (and (= m 4) (not (= b 1))))
                              (lambda(b m) (and (= b 1) (not (= m 4)))))))
      (%require (sttmt-of-betty betty kitty))
      (%require (sttmt-of-kitty kitty mary))
      (%require (sttmt-of-mary betty mary))
      (%require (distinct? (list betty ethel joan kitty mary)))
      (list (list 'baker betty)
            (list 'ethel ethel)
            (list 'joan joan)
            (list 'kitty kitty)
            (list 'mary mary)))))

    


;;;test

(liars-puzzle)
(amb)



