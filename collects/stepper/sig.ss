(define-signature stepper:error^
  (static-error dynamic-error internal-error))

(define-signature stepper:settings^
  (get-namespace
   get-global-defined-vars
   get-constructor-style-printing
   get-abbreviate-cons-as-list
   get-empty-list-name
   get-show-sharing
   get-cons
   get-vector
   get-vocabulary
   image?))

(define-signature stepper:shared^
  (list-take
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
  (reconstruct 
   final-mark-list?
   result-step-stop-here?
   stop-here?))

  