(module debugger-annotate mzscheme
  
  (require (prefix kernel: (lib "kerncase.ss" "syntax"))
           (lib "contracts.ss")
           "breakpoint-token.ss")
  
  ;(provide/contract [annotate (-> syntax-object? 
  ;                                syntax-object?)])
  
  ; what does the iterator need to know about 
  
  ; (-> (-> 
  (define expr-syntax-object-iterator 
    (contract
     (-> (-> syntax-object? syntax-object?)
         syntax-object?
         syntax-object?)
     (lambda (fn stx)
       (let* ([lambda-clause-abstraction
               (lambda (clause)
                 (kernel:kernel-syntax-case stx
                   [((variable ...) . bodies)
                    (rebuild-stx #`((variable ...) #,@(map fn (syntax->list bodies))) stx)]
                   [else
                    (error 'expr-syntax-object-iterator "unexpected (case-)lambda clause: ~a\n" (syntax-object->datum stx))]))]
              [let-values-abstraction
               (lambda (stx)
                 (kernel:kernel-syntax-case stx
                   [(kwd ((variable ...) ...) . bodies)
                    (rebuild-stx #`(kwd ((variable ...) ...) #,@(map fn (syntax->list #'bodies))))]
                   [else
                    (error 'expr-syntax-object-iterator "unexpected let(rec) expression: ~a\n" (syntax-object->datum stx))]))]) 
         (kernel:kernel-syntax-case stx
           [var-stx
            (identifier? (syntax var-stx))
            stx]
           [(lambda . clause)
            (rebuild-stx #`(lambda #,@(lambda-clause-abstraction #'clause)))]
           [(case-lambda . clauses)
            (rebuild-stx #`(case-lambda #,@(map lambda-clause-abstraction (syntax->list #'clauses))))]
           [(if test then)
            (rebuild-stx #`(if #,(fn #'test) #,(fn #'then)))]
           [(if test then else)
            (rebuild-stx #`(if #,(fn #'test) #,(fn #'then) #,(fn else)))]
           [(begin . bodies)
            (rebuild-stx #`(begin #,@(map fn (syntax->list #'bodies))))]
           [(begin0 . bodies)
            (rebuild-stx #`(begin0 #,@(map fn (syntax->list #'bodies))))]
           [(let-values . _)
            (let-values-abstraction stx)]
           [(letrec-values . _)
            (let-values-abstraction stx)]
           [(set! var val)
            (rebuild-stx #`(set! var #,(fn #'val)))]
           [(quote _)
            stx]
           [(quote-syntax _)
            stx]
           [(with-continuation-mark key mark body)
            (rebuild-stx #`(with-continuation-mark #,(fn #'key) #,(fn #'mark) #,(fn #'body)))]
           [(#%app . exprs)
            (rebuild-stx #`(#%app #,@(map fn (syntax->list #'exprs))))]
           [(#%datum . _)
            stx]
           [(#%top . var)
            stx]
           [else
            (error 'expr-syntax-object-iterator "unknown expr: ~a\n" (syntax-object->datum stx))])))
     'expr-syntax-object-iterator
     'caller))
  
  )