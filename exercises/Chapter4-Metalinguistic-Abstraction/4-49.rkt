#lang sicp

(define nil '())

(define ($$require p)
  (if (not p) (amb)))

(define nouns 
  '(noun student professor cat class))

(define verbs 
  '(verb studies lectures eats sleeps))

(define articles '(article the a))

(define prepositions 
  '(prep for to in by with))

(define (word-list-rdm tgtlist)
  (list-ref tgtlist (+ 1 (random (- (length tgtlist) 1)))))

(define (gen-word word-list)
  ($$require (not (= *remaining* 0)))
  (set! *remaining* (- *remaining* 1))
  (list (car word-list) (word-list-rdm word-list)))

(define (an-integer-starting-from n)
  (amb n (an-integer-starting-from (+ n 1))))

(define *remaining* 0)
(define (generate)
  (set! *remaining* (an-integer-starting-from 1))
  (let ((sent (gen-sentence)))
    ($$require (= *remaining* 0))
    sent))

(define (gen-prepositional-phrase)
  (list 'prep-phrase
        (gen-word prepositions)
        (gen-noun-phrase)))

(define (gen-sentence)
  (list 'sentence
        (gen-noun-phrase)
        (gen-verb-phrase)))

(define (gen-verb-phrase)
  (define (maybe-extend verb-phrase)
    (amb 
     verb-phrase
     (maybe-extend 
      (list 'verb-phrase
            verb-phrase
            (gen-prepositional-phrase)))))
  (maybe-extend (gen-word verbs)))

(define (gen-simple-noun-phrase)
  (list 'simple-noun-phrase
        (gen-word articles)
        (gen-word nouns)))

(define (gen-noun-phrase)
  (define (maybe-extend noun-phrase)
    (amb 
     noun-phrase
     (maybe-extend 
      (list 'noun-phrase
            noun-phrase
            (gen-prepositional-phrase)))))
  (maybe-extend (gen-simple-noun-phrase)))

(generate)
(amb)
(amb)
(amb)





;(amb)