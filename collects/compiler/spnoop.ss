
;; Implements a MrSpidey that doesn't do anything

(unit/sig compiler:mrspidey^
  (import)

  (define (copy-annotations! new old) new)

  (define (binding-mutated ast) #f)

  (define (SDL-type ast) #f)
  
  (define (analyze-program-sexps exprs input-directory)
      exprs)

  (define (constant-value ast) (values #f (void)))

  (define (parsed-ftype _) #f)

  (define (Tvar-objs _) #f)
  (define (Tvar? _) #f)

  (define (fo-FlowType? _) #f)

  (define FlowType->Tvar #f)

  (define (prim-av? _) #f)

  (define (fo-ftype->AVs _) #f)

  (define (ast->AVs _) #f) 

  (define (AV->AVs _) #f)
)
