#lang racket

(define (caddddr p) (car (cddddr p)))

(define (enumerate-5) (sequence->list (in-range 1 6)))

(define (flat-map proc lst)
  (foldl append '() (map proc lst)))

(define (distinct? items)
  (cond ((null? items) true)
        ((null? (cdr items)) true)
        ((member (car items) (cdr items)) false)
        (else (distinct? (cdr items)))))

(define (baker-requirement baker)
  (not (= baker 5)))
(define (cooper-requirement cooper)
  (not (= cooper 1)))
(define (fletcher-requirement fletcher)
  (and (not (= fletcher 1)) (not (= fletcher 5))))

(define (fletcher-cooper-requirement fletcher cooper)
  (not (= (abs (- fletcher cooper)) 1)))
(define (miller-cooper-requirement miller cooper)
  (> miller cooper))
(define (smith-fletcher-requirement smith fletcher)
  (not (= (abs (- smith fletcher)) 1)))


;                                                                                                                                                  
;                                                                                                                                                  
;                                                                                                                                                  
;                                                                                                                                                  
;                      ;                              ;;       ;                                                               ;                   
;                                                      ;                     ;                                                                     
;   ;;  ;;   ;;;;;   ;;;    ;; ;;    ;;; ;;            ;     ;;;     ;;;;;  ;;;;;          ;; ;  ;   ;;;;   ;; ;;   ;; ;;    ;;;    ;; ;;    ;;; ;;
;    ;   ;  ;    ;     ;     ;;  ;  ;   ;;             ;       ;    ;    ;   ;              ;; ;; ; ;    ;   ;;  ;   ;;  ;     ;     ;;  ;  ;   ;; 
;    ;   ;   ;;;;      ;     ;   ;  ;    ;  ;;;;;;     ;       ;     ;;;;    ;      ;;;;;;  ;  ;  ;  ;;;;;   ;   ;   ;   ;     ;     ;   ;  ;    ; 
;    ;   ;       ;     ;     ;   ;  ;    ;             ;       ;         ;   ;              ;  ;  ; ;    ;   ;   ;   ;   ;     ;     ;   ;  ;    ; 
;    ;  ;;  ;    ;     ;     ;   ;  ;   ;;             ;       ;    ;    ;   ;   ;          ;  ;  ; ;   ;;   ;   ;   ;   ;     ;     ;   ;  ;   ;; 
;     ;; ;; ;;;;;    ;;;;;  ;;; ;;;  ;;; ;           ;;;;;   ;;;;;  ;;;;;     ;;;          ;;; ;; ;  ;;; ;;  ;;;;    ;;;;    ;;;;;  ;;; ;;;  ;;; ; 
;                                        ;                                                                   ;       ;                           ; 
;                                    ;;;;                                                                   ;;;     ;;;                      ;;;;  
;                                                                                                                                                  
;                                                                                                                                                  


(define (logic-puzzle-using-list-mapping)
  (filter distinct?
          (filter (lambda(lst)
                    (smith-fletcher-requirement (caddddr lst) (caddr lst)))
                  (flat-map (lambda (s)
                              (filter (lambda(lst)
                                        (miller-cooper-requirement (cadddr lst) (cadr lst)))
                                      (flat-map (lambda (m)
                                                  (filter (lambda(lst)
                                                            (fletcher-cooper-requirement (caddr lst) (cadr lst)))
                                                          (flat-map (lambda (f)
                                                                      (flat-map (lambda (c)
                                                                                  (map (lambda (b) 
                                                                                         (list b c f m s))
                                                                                       (filter baker-requirement (enumerate-5))))
                                                                                (filter cooper-requirement (enumerate-5))))
                                                                    (filter fletcher-requirement (enumerate-5)))))
                                                (enumerate-5))))
                            (enumerate-5)))))



;                                                                                                  
;                                                                                                  
;                                                                                                  
;                                                                                                  
;                      ;                                                                           
;                                                            ;                                     
;   ;;  ;;   ;;;;;   ;;;    ;; ;;    ;;; ;;          ;;;;;  ;;;;;   ;; ;;;   ;;;;    ;;;;  ;; ;  ; 
;    ;   ;  ;    ;     ;     ;;  ;  ;   ;;          ;    ;   ;       ;;     ;    ;  ;    ;  ;; ;; ;
;    ;   ;   ;;;;      ;     ;   ;  ;    ;  ;;;;;;   ;;;;    ;       ;      ;;;;;;   ;;;;;  ;  ;  ;
;    ;   ;       ;     ;     ;   ;  ;    ;               ;   ;       ;      ;       ;    ;  ;  ;  ;
;    ;  ;;  ;    ;     ;     ;   ;  ;   ;;          ;    ;   ;   ;   ;      ;       ;   ;;  ;  ;  ;
;     ;; ;; ;;;;;    ;;;;;  ;;; ;;;  ;;; ;          ;;;;;     ;;;   ;;;;;    ;;;;;   ;;; ;;;;; ;; ;
;                                        ;                                                         
;                                    ;;;;                                                          
;                                                                                                  
;                                                                                                  


(define (enumerate-5-stream) (in-range 1 6))

(define (logic-puzzle-using-stream)
  (define (baker-stream) (stream-filter baker-requirement (enumerate-5-stream)))
  (define (cooper-stream) (stream-filter cooper-requirement (enumerate-5-stream)))
  (define (fletcher-stream) (stream-filter fletcher-requirement (enumerate-5-stream)))
  (define (miller-stream) (enumerate-5-stream))
  (define (smith-stream) (enumerate-5-stream))
  
  (define (c-iter b-val)
    (let go-c ((c-stm (cooper-stream)))
      (if (stream-empty? c-stm)
          empty-stream
          (stream-append
           (f-iter b-val (stream-first c-stm))
           (stream-lazy
            (go-c (stream-rest c-stm)))))))
  
  (define (f-iter b-val c-val)
    (let go-f ((f-stm
                (stream-filter
                 (lambda(f-raw-val) (fletcher-cooper-requirement f-raw-val c-val))
                 (fletcher-stream))))
      (if (stream-empty? f-stm)
          empty-stream
          (stream-append
           (m-iter b-val c-val (stream-first f-stm))
           (stream-lazy
            (go-f (stream-rest f-stm)))))))
  
  (define (m-iter b-val c-val f-val)
    (let go-m ((m-stm
                (stream-filter
                 (lambda(m-raw-val) (miller-cooper-requirement m-raw-val c-val))
                 (miller-stream))))
      (if (stream-empty? m-stm)
          empty-stream
          (stream-append
           (s-iter b-val c-val f-val (stream-first m-stm))
           (stream-lazy
            (go-m (stream-rest m-stm)))))))
  
  (define (s-iter b-val c-val f-val m-val)
    (let go-s ((s-stm
                (stream-filter
                 (lambda(s-raw-val) (smith-fletcher-requirement s-raw-val f-val))
                 (smith-stream))))
      (if (stream-empty? s-stm)
          empty-stream
          (stream-cons
           (list b-val c-val f-val m-val (stream-first s-stm))
           (stream-lazy
            (go-s (stream-rest s-stm)))))))
  
  (define (b-iter b-stm)
    (if (stream-empty? b-stm)
        empty-stream
        (stream-append
         (c-iter (stream-first b-stm))
         (b-iter (stream-rest b-stm)))))
  
  (stream-filter distinct?
                 (b-iter (baker-stream))))


   


;;;test

(logic-puzzle-using-list-mapping)

(for ((lst (logic-puzzle-using-stream)))
  (println lst))
