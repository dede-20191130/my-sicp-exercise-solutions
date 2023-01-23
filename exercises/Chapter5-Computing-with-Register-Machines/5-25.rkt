#lang racket

; prerequisite primitive operation
; procedure
;;  actual-value
;  delay-it
;  force-it

; eval and apply application clause
'(
  ev-application
  (save continue)
  (save env)
  (assign unev (op operands) (reg exp))
  (save unev)
  (assign exp (op operator) (reg exp))
  ;  (assign exp (op actual-value) (reg exp))
  (assign
   continue (label ev-appl-did-operator))
  (goto (label eval-dispatch))

  ev-appl-did-operator
  (restore unev)             
  (restore env)
  ;  (assign argl (op empty-arglist))
  (assign proc (reg val))
  (assign proc (op force-it) (reg proc)) ; if thunk returned get actual value
  ;  (test (op no-operands?) (reg unev))
  ;  (branch (label apply-dispatch))
  ;  (save proc)

  apply-dispatch
  (assign argl (op empty-arglist))
  (save proc)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-apply))
  (test (op compound-procedure?) (reg proc))
  (branch (label compound-apply))
  (goto (label unknown-procedure-type))

  primitive-apply
  (save argl)
  (assign exp
          (op first-operand)
          (reg unev))
  (test (op last-operand?) (reg unev))
  (branch (label apply-prim-last-arg))
  (save env)
  (save unev)
  (assign continue 
          (label apply-prim-accumulate-arg))
  (goto (label eval-dispatch))
  apply-prim-accumulate-arg
  (restore unev)
  (restore env)
  (restore argl)
  (assign val (op force-it) (reg val)) ; if thunk returned get actual value
  (assign argl 
          (op adjoin-arg)
          (reg val)
          (reg argl))
  (assign unev
          (op rest-operands)
          (reg unev))
  (goto (label primitive-apply))
  apply-prim-last-arg
  (assign continue 
          (label apply-prim-accum-last-arg))
  (goto (label eval-dispatch))
  apply-prim-accum-last-arg
  (restore argl)
  (assign val (op force-it) (reg val)) ; if thunk returned get actual value
  (assign argl 
          (op adjoin-arg)
          (reg val)
          (reg argl))
  (restore proc)
  (assign val (op apply-primitive-procedure)
          (reg proc)
          (reg argl))
  (restore continue)
  (goto (reg continue))

  compound-apply
  (save argl)
  (assign exp
          (op first-operand)
          (reg unev))
  (test (op last-operand?) (reg unev))
  (branch (label apply-cmpd-last-arg))
  (save env)
  (save unev)
  (assign continue 
          (label apply-cmpd-accumulate-arg))
  (goto (label eval-dispatch))
  apply-cmpd-accumulate-arg
  (restore unev)
  (restore env)
  (restore argl)
  (assign val (op delay-it) (reg val)) ; keep as thunk until it is comsumed in primitive proc.
  (assign argl 
          (op adjoin-arg)
          (reg val)
          (reg argl))
  (assign unev
          (op rest-operands)
          (reg unev))
  (goto (label compoun-apply))
  apply-cmpd-last-arg
  (assign continue 
          (label apply-cmpd-accum-last-arg))
  (goto (label eval-dispatch))
  apply-cmpd-accum-last-arg
  (restore argl)
  (assign val (op force-it) (reg val)) ; keep as thunk until it is comsumed in primitive proc.
  (assign argl 
          (op adjoin-arg)
          (reg val)
          (reg argl))
  (assign unev 
          (op procedure-parameters)
          (reg proc))
  (assign env
          (op procedure-environment)
          (reg proc))
  (assign env
          (op extend-environment)
          (reg unev)
          (reg argl)
          (reg env))
  (assign unev
          (op procedure-body)
          (reg proc))
  (goto (label ev-sequence)))


; conditional clause
'(
  ev-if
  (save exp)   
  (save env)
  (save continue)
  (assign continue (label ev-if-decide))
  (assign exp (op if-predicate) (reg exp))
  (goto (label eval-dispatch))
  ev-if-decide
  (restore continue)
  (restore env)
  (restore exp)
  (assign val (op force-it) (reg val)) ; if thunk returned get actual value
  (test (op true?) (reg val))
  (branch (label ev-if-consequent))
  ev-if-alternative
  (assign exp (op if-alternative) (reg exp))
  (goto (label eval-dispatch))
  ev-if-consequent
  (assign exp (op if-consequent) (reg exp))
  (goto (label eval-dispatch)))
