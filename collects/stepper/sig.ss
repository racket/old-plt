(module sig mzscheme
  (provide ccond
	   stepper:cogen-utils^
	   plt:aries-no-break^
	   plt:aries^
	   stepper:marks^
	   stepper:client-procs^
	   stepper:model-input^
	   stepper:model^
	   stepper:shared^
	   stepper:annotate^
	   stepper:reconstruct^
	   stepper:mred-extensions^)




  
(define-signature plt:aries^
  ((open plt:aries-no-break^)
   break))



(define-signature stepper:model-input^
  (text-stream settings image? receive-result))



(define-signature stepper:shared^
  ((struct before-after-result (finished-exprs exp redex post-exp reduct after-exprs))
   (struct before-error-result (finished-exprs exp redex err-msg after-exprs))
   (struct error-result (finished-exprs err-msg))
   (struct finished-result (finished-exprs))
   get-binding-name
   list-take
   list-partition
   (struct closure-record (name mark constructor? lifted-name))
   bogus-binding?
   *unevaluated* 
   no-sexp
   multiple-highlight
   if-temp
   struct-flag
   highlight-placeholder
   get-arg-binding
   get-lifted-gensym
   expr-read
   set-expr-read!
   flatten-take
   closure-table-put!
   closure-table-lookup
   insert-highlighted-value
   binding-indexer 
   binding-index-reset))

(define-signature stepper:annotate^
  (initial-env-package
   annotate))

(define-signature stepper:reconstruct^
  (reconstruct-completed
   reconstruct-current
   final-mark-list?
   skip-result-step?
   skip-redex-step?))

(define-signature stepper:mred-extensions^
  (stepper-canvas%
   stepper-text%
   image?
   separator-snip% ;; these last two aren't required, but are useful 
   vertical-separator-snip%)) ;; for debugging purposes
)
