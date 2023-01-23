#lang racket

'(rule (big-shot ?bs-pers)
       (and (supervisor ?bs-pers ?sv)
            (job ?bs-pers (?div . ?type1))
            (job ?sv (?another-div . ?type2))
            (not (same ?div ?another-div))))