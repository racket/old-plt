(define-signature stepper:stepper^
  (stepper-start
   stepper-step
   stepper-stop))

(define-signature stepper:error^
  (static-error dynamic-error internal-error))

(define-signature stepper:shared^
  (read-exprs
   read->raw 
   arglist->ilist 
   *unevaluated* 
   if-temp
   create-bogus-bound-varref
   create-bogus-top-level-varref
   get-arg-symbol
   (struct varref (var top-level?))
   expr-read
   set-expr-read!
   flatten-take
   improper-map
   improper-foreach))

(define-signature stepper:annotate^
  (annotate))

(define-signature stepper:reconstruct^
  (closure-table-put! reconstruct))

  