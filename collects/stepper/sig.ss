(define-signature stepper:stepper^
  (stepper-start
   stepper-step
   stepper-stop))

(define-signature stepper:error^
  (static-error dynamic-error internal-error))

(define-signature stepper:shared^
  (list-take
   read-exprs
   read->raw 
   arglist->ilist 
   make-closure-record
   closure-record-name
   closure-record-mark
   ;(struct closure-record (name mark))
   create-bogus-bound-varref
   create-bogus-top-level-varref
   *unevaluated* 
   if-temp
   struct-flag
   get-arg-symbol
   top-level-exp-gensym-source
   expr-read
   set-expr-read!
   flatten-take
   improper-map
   improper-foreach))

(define-signature stepper:annotate^
  (annotate))

(define-signature stepper:reconstruct^
  (closure-table-put! reconstruct))

  