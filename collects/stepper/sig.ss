(define-signature stepper^
  (stepper-start
   stepper-step
   stepper-stop))

(define-signature stepper:error^
  (static-error dynamic-error internal-error))

(define-signature stepper:shared^
  (read->raw 
   arglist->ilist 
   *unevaluated* 
   get-arg-symbol
   (define-struct varref (var top-level?))
   expr-read
   set-expr-read!
   all-defs-list-sym
   current-def-sym
   flatten-take))

(define-signature stepper:annotate^
  (annotate))

(define-signature stepper:reconstruct^
  (closure-table-put! register-source reconstruct))

(define-signature stepper^
  (annotate reconstruct))
  
  