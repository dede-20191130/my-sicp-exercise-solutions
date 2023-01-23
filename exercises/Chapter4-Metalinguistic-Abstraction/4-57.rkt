#lang racket

;;; rule
'(rule (replace ?person1 ?person2)
       (and
        (or (and (job ?person1 (?division1 . ?joblist1))
                 (job ?person2 (?division2 . ?joblist1)))
            (and (job ?person1 (?division1 . ?joblist1))
                 (job ?person2 (?division2 . ?joblist2))
                 (job ?anotherperson (?division0 ?joblist1 . ?rest1))
                 (job ?anotherperson (?division0 ?joblist2 . ?rest2))
                 ))
        (not (same ?person1 ?person2))))

;;; 1
'(replace ?person (Fect Cy D.))

;;; 2
'(and (replace ?morepaid-person ?lesspaid-person)
      (salary ?morepaid-person ?morepaid-amount)
      (salary ?lesspaid-person ?lesspaid-amount)
      (lisp-value > ?morepaid-amount ?lesspaid-amount))

;;; use can-do-job?
;http://community.schemewiki.org/?sicp-ex-4.57