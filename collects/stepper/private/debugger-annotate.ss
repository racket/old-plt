(define debugger-annotate mzscheme
  
  (require (prefix kernel: (lib "kerncase.ss" "syntax"))
           (lib "contracts.ss")
           "breakpoint-token")
  
  (provide/contract [annotate (-> syntax-object? 
                                  syntax-object?)])
  
  ; what does the iterator need to know about 
  
  (-> (-> 
  (define (syntax-object-iterator fn stx)
    (kernel:kernel-syntax-case stx
      [(lambda . clause)
       ...]
      [(case-lambda . clauses)
       ...]
      [(if test then else)
       ...]
      [(if test then)
       ...]
      [(begin . bodies-stx)
       ...]
      [(begin0 . bodies-stx)
       ...]
      [(let-values . _)
       ...]
      [(letrec-values . _)
       ...]
      [(set! var val)
       ...]
      [(quote _)
       ...]
      [(quote-syntax _)
       ...]
      [(with-continuation-mark key mark body)
       ...]
      [(#%app . terms)
       ...]
      [(#%datum . _)
       ...]
      [(define-values vars-stx body)
       ...]
      [(#%top . var-stx)
       ...]
      [var-stx
       (identifier? (syntax var-stx))
       ...]
      [else ; require, require-for-syntax, define-syntaxes, module, provide
       ...]))
  
  