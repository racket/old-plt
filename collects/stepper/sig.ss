(define-signature stepper:error^
  (static-error dynamic-error internal-error))

(define-signature stepper:shared^
  (list-take
   read-exprs
   read->raw 
   arglist->ilist 
   (struct closure-record (name mark constructor?))
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
   improper-foreach
   closure-table-put!
   closure-table-lookup))

(define-signature stepper:annotate^
  (annotate))

(define-signature stepper:reconstruct^
  (reconstruct set-global-defined-vars! final-mark-list? stop-here?))

  