#lang sicp

(define ($$require p)
  (if (not p) (amb)))

(define (distinct? items)
  (cond ((null? items) true)
        ((null? (cdr items)) true)
        ((member (car items) (cdr items)) false)
        (else (distinct? (cdr items)))))

(define (daughters-amb)
  (let go ((lst '(mary gabrielle lorna rosalind melissa)))
    (if (null? lst)
        (amb)
        (amb (car lst)
             (go (cdr lst))))))

(define (ga-father_s-yacht-equal-pa-daughter? fath-daut-comb yac-nm-comb)
  (define first-of-two car)
  (define second-of-two cadr)
  (let ((pa-comb (assoc 'parker fath-daut-comb)))
    (if (not pa-comb)
        false
        (let ((ga_s-fath-comb (assoc 'gabrielle (map reverse fath-daut-comb))))
          (if (not ga_s-fath-comb)
              false
              (let ((ga_s-fath-yac-comb (assoc (second-of-two ga_s-fath-comb) yac-nm-comb)))
                (if (not ga_s-fath-yac-comb)
                    false
                    (eq? (second-of-two ga_s-fath-yac-comb) (second-of-two pa-comb)))))))))

(define (find-the-daughter-pzl)
  (let ((yacht-name-comb
         (list (list 'moore 'lorna)
               (list 'barnacle 'gabrielle)
               (list 'hall 'rosalind)
               (list 'parker 'mary)
               (list 'downing 'melissa)))
        (moore_s-da (daughters-amb)))
    ($$require (eq? moore_s-da 'mary))
    (let ((barnacle_s-da (daughters-amb)))
      ($$require (eq? barnacle_s-da 'melissa))
      (let ((hall_s-da (daughters-amb)))
        ($$require (not (eq? hall_s-da 'rosalind)))
        (let ((parker_s-da (daughters-amb)))
          ($$require (not (eq? parker_s-da 'gabrielle)))
          (let ((downing_s-da (daughters-amb)))
            ($$require (distinct? (list moore_s-da barnacle_s-da hall_s-da parker_s-da downing_s-da)))
            (let ((father-daughter-comb (list (list 'moore moore_s-da)
                                              (list 'barnacle barnacle_s-da)
                                              (list 'hall hall_s-da)
                                              (list 'parker parker_s-da)
                                              (list 'downing downing_s-da))))
              ($$require (ga-father_s-yacht-equal-pa-daughter? father-daughter-comb yacht-name-comb))
              father-daughter-comb)))))))
              

(define (find-the-daughter-pzl-in-case-of-Mary_Ann_s-father-unknown)
  (let ((yacht-name-comb
         (list (list 'moore 'lorna)
               (list 'barnacle 'gabrielle)
               (list 'hall 'rosalind)
               (list 'parker 'mary)
               (list 'downing 'melissa)))
        (moore_s-da (daughters-amb)))
    ($$require (not (eq? moore_s-da 'lorna))) ;changed
    (let ((barnacle_s-da (daughters-amb)))
      ($$require (eq? barnacle_s-da 'melissa))
      (let ((hall_s-da (daughters-amb)))
        ($$require (not (eq? hall_s-da 'rosalind)))
        (let ((parker_s-da (daughters-amb)))
          ($$require (not (eq? parker_s-da 'gabrielle)))
          (let ((downing_s-da (daughters-amb)))
            ($$require (distinct? (list moore_s-da barnacle_s-da hall_s-da parker_s-da downing_s-da)))
            (let ((father-daughter-comb (list (list 'moore moore_s-da)
                                              (list 'barnacle barnacle_s-da)
                                              (list 'hall hall_s-da)
                                              (list 'parker parker_s-da)
                                              (list 'downing downing_s-da))))
              ($$require (ga-father_s-yacht-equal-pa-daughter? father-daughter-comb yacht-name-comb))
              father-daughter-comb)))))))
              



;;;test

(find-the-daughter-pzl)
;(amb)

(find-the-daughter-pzl-in-case-of-Mary_Ann_s-father-unknown)
(amb)
;(amb)


