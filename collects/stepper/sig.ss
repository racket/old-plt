(define-signature stepper:zodiac-client-procs^
  (never-undefined-getter
   never-undefined-setter
   read-getter
   read-setter))

(define-signature stepper:error^
  (static-error dynamic-error internal-error))

(define-signature stepper:settings^
  (check-pre-defined-var
   check-global-defined
   global-lookup
   constructor-style-printing?
   abbreviate-cons-as-list?
   user-cons?
   user-vector?
   image?
   print-convert))

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
   highlight-placeholder
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
  (initial-env-package
   annotate))

(define-signature stepper:reconstruct^
  (expose-mark-list
   reconstruct-completed
   reconstruct-current
   final-mark-list?
   skip-result-step?
   skip-redex-step?))

  