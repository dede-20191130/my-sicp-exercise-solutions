#lang racket
(require compatibility/mlist)

(define table-records mcdr)
(define record-val mcdr)

(define (make-table)
  (define (lookup key-list tgt-table)
    (let ((record (massoc (car key-list) (table-records tgt-table))))
      (cond ((not record)false)
            ((null? (cdr key-list)) (record-val record))
            (else (lookup (cdr key-list) record)))))
       
  (define (insert! key-list value tgt-table)
    (define (organize-record kl value)
      (if (null? (cdr kl))
          (mcons (car kl) value)
          (mlist (car kl) (organize-record (cdr kl) value))))
    (let ((record (massoc (car key-list) (table-records tgt-table))))
      (if (null? (cdr key-list))
          (if record
              (set-mcdr! record value)
              (set-mcdr! tgt-table
                         (mcons (mcons (car key-list) value)
                                (table-records tgt-table))))
          (if record
              (insert! (cdr key-list) value record)
              (set-mcdr! tgt-table
                         (mcons (organize-record key-list value) (table-records tgt-table))))))
    'ok)
  (let ((local-table (mlist '*table*)))    
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) (λ (key-list) (lookup key-list local-table)))
            ((eq? m 'insert-proc!) (λ (key-list value) (insert! key-list value local-table)))
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))


(define (memoize f)
  (let ((table (make-table)))
    (let ((lookup (table 'lookup-proc))
          (insert! (table 'insert-proc!)))
      (lambda (x)
        (let ((listized-x (if (list? x) x (list x))))
          (let ((previously-computed-result (lookup listized-x)))
            (or previously-computed-result
                (let ((result (f x)))
                  (insert! listized-x result)
                  (println (string-append "insertion called with " (number->string x)))
                  result))))))))

(define memo-fib
  (memoize (lambda (n)
             (println (string-append "Fibonacci calcucation with " (number->string n)))
             (cond ((= n 0) 0)
                   ((= n 1) 1)
                   (else (+ (memo-fib (- n 1))
                            (memo-fib (- n 2))))))))

(define (fib n)
  (println (string-append "Fibonacci calcucation with " (number->string n)))
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

(define directly-memo-fib  (memoize fib))

(memo-fib 3)
(memo-fib 7)
(memo-fib 10)
(memo-fib 10)
(println "---------------------------------------------")
(directly-memo-fib 3)
(directly-memo-fib 7)
(directly-memo-fib 10)
(directly-memo-fib 10)