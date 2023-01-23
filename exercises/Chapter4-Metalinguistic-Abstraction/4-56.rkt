#lang racket

;;; 1
'(and (supervisor ?person (Bitdiddle Ben))
      (address ?person ?where))

;;; 2
'(and (salary ?person ?amount)
      (salary (Bitdiddle Ben) ?ben_s-amount)
      (lisp-value < ?amount ?ben_s-amount))

;;; 3
'(and (supervisor ?person ?sv)
      (not (job ?sv (computer . ?jobname)))
      (job ?sv ?sv-jobname))
