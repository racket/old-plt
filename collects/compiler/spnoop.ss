
;; Implements a MrSpidey that doesn't do anything

(unit/sig compiler:mrspidey^
  (import)

  (define (binding-mutated ast) #f)

  (define (SDL-type ast) #f)
  
  (define (analyze-program-sexps exprs input-directory)
      exprs))

