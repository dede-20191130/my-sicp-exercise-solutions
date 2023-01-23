#lang racket

;;; 1
'(meeting ?division (Friday ?time))

;;; 2
'(rule (meeting-time ?person ?day-and-time)
       (and (meeting whole-company ?day-and-time)
            (and (job ?person (?division . ?type))
                 (meeting ?division ?day-and-time))))

;;; 3
'(meeting-time (Alyssa P. Hacker) (Wednesday ?time))