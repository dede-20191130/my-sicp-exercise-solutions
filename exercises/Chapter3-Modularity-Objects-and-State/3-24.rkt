#lang racket
(require compatibility/mlist)

(define (mcaar mpair) (mcar (mcar mpair)))
(define (mcadr mpair) (mcar (mcdr mpair)))
(define (print-line value) 
  (display value) 
  (newline))

(define (massoc-flex key records same-key?)
  (cond ((null? records) false)
        ((same-key? key (mcaar records)) (mcar records))
        (else (massoc-flex key (mcdr records) same-key?))))

(define (make-table same-key?)
  (let ((local-table (mlist '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (massoc-flex key-1 (mcdr local-table) same-key?)))
        (if subtable
            (let ((record (massoc-flex key-2 (mcdr subtable) same-key?)))
              (if record
                  (mcdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (massoc-flex key-1 (mcdr local-table) same-key?)))
        (if subtable
            (let ((record (massoc-flex key-2 (mcdr subtable) same-key?)))
              (if record
                  (set-mcdr! record value)
                  (set-mcdr! subtable
                             (mcons (mcons key-2 value)
                                    (mcdr subtable)))))
            (set-mcdr! local-table
                       (mcons (mlist key-1
                                     (mcons key-2 value))
                              (mcdr local-table)))))
      'ok)    
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define (is-equal-with-num-tolerance? a b)
  (if (number? a)
      (if (number? b)
          (<= (abs (- a b)) 5)
          (error "BOTH IS REQUIRED AS NUMBER" (list a b)))
      (equal? a b)))

(define operation-table (make-table is-equal-with-num-tolerance?))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

(put 'letters 'a 97)
(put 'letters 'b 98)
(put 'math '+ 43)
(put 'math '- 45)
(put 'math '* 42)
(get 'math '-)
(get 'letters 'b)
(get 'letters 'c)
(print-line "--------")
(put 'boundary 5 'apple)
(put 'boundary 15 'lemon)
(put 'boundary 25 'grape)
(get 'boundary 0)
(get 'boundary 3)
(get 'boundary 10)
(get 'boundary 15)
(get 'boundary 20)
(get 'boundary 23)
(get 'boundary 30)
