
;; Implements a MrSpidey that doesn't do anything

(unit/sig compiler:mrspidey^
  (import)

  (define (copy-annotations! new old) new)

  (define (binding-mutated ast) #f)

  (define (SDL-type ast) #f)
  
  (define (constant-value ast) (values #f (void)))

  (define (analyze-program-sexps exprs input-directory)
      exprs))

