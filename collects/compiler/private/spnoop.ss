
;; Implements a MrSpidey that doesn't do anything

(module spnoop mzscheme
  (require (lib "unitsig.ss"))

  (require "sig.ss")

  (provide spidey-noop@)
  (define spidey-noop@
    (unit/sig compiler:mrspidey^
      (import)

      (define (copy-annotations! new old) new)

      (define (get-annotations old) null)

      (define (binding-mutated ast) #f)

      (define (analyze-program-sexps exprs input-directory)
	exprs)

      (define (constant-value ast) (values #f (void)))

      (define (SDL-type ast) #f)  

      (define (parsed-ftype _) #f)

      (define (Tvar-objs _) #f)
      (define (Tvar? _) #f)

      (define (fo-FlowType? _) #f)

      (define FlowType->Tvar #f)

      (define (prim-av? _) #f)

      (define (fo-ftype->AVs _) #f)

      (define (ast->AVs _) #f) 

      (define (AV->AVs _) #f))))
