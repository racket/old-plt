(module debugger-annotate mzscheme
  
  (require (prefix kernel: (lib "kerncase.ss" "syntax"))
           (lib "contracts.ss")
           "shared.ss"
           "breakpoint-token.ss")
  
  ;(provide/contract [annotate (-> syntax-object? 
  ;                                syntax-object?)])
  
  ; what does the iterator need to know about 
  
  
  
  (define/contract expr-iterator
    (-> syntax?
        (-> syntax? (vector/f syntax? any?))
        (vector/f syntax? any?))
    (lambda (fn stx)
      (let* ([rebuild
              (lambda (new-sexp) 
                (rebuild-stx new-sexp stx))]
             [lambda-clause-abstraction
              (lambda (clause)
                (kernel:kernel-syntax-case stx #f
                  [((variable ...) . bodies)
                   (let*-2vals ([(new-bodies outs)
                                 (2vals-map fn (map fn (syntax->list #'bodies)))])
                     (2vals
                      `(#'(variable ...) ,@new-bodies)
                      outs))]
                   [else
                    (error 'expr-syntax-object-iterator "unexpected (case-)lambda clause: ~a\n" (syntax-object->datum stx))]))]
              [let-values-abstraction
               (lambda (stx)
                 (kernel:kernel-syntax-case stx #f
                   [(kwd ((variable ...) ...) . bodies)
                    (let*-2vals ([(new-bodies outs)
                                  (2vals-map fn (map fn (syntax->list #'bodies)))])
                    (2vals 
                     (rebuild `(#'kwd #'((variable ...) ...) ,@new-bodies))
                     outs))]
                   [else
                    (error 'expr-syntax-object-iterator "unexpected let(rec) expression: ~a\n" (syntax-object->datum stx))]))]) 
         (kernel:kernel-syntax-case stx #f
           [var-stx
            (identifier? (syntax var-stx))
            stx]
           [(lambda . clause)
            (let*-2vals ([(clause outs)  (lambda-clause-abstraction #'clause)])
              (2vals 
               (rebuild `(#'lambda ,@clause))
               outs))]
           [(case-lambda . clauses)
            (let*-2vals ([(clauses outs)
            (rebuild `(case-lambda ,@(map lambda-clause-abstraction (syntax->list #'clauses))))]
           [(if test then)
            (rebuild `(#'if ,(fn #'test) ,(fn #'then)))]
           [(if test then else)
            (rebuild `(#'if ,(fn #'test) ,(fn #'then) ,(fn #'else)))]
           [(begin . bodies)
            (rebuild `(#'begin ,@(map fn (syntax->list #'bodies))))]
           [(begin0 . bodies)
            (rebuild `(#'begin0 ,@(map fn (syntax->list #'bodies))))]
           [(let-values . _)
            (let-values-abstraction stx)]
           [(letrec-values . _)
            (let-values-abstraction stx)]
           [(set! var val)
            (rebuild `(#'set! #'var ,(fn #'val)))]
           [(quote _)
            stx]
           [(quote-syntax _)
            stx]
           [(with-continuation-mark key mark body)
            (rebuild `(#'with-continuation-mark ,(fn #'key) ,(fn #'mark) ,(fn #'body)))]
           [(#%app . exprs)
            (rebuild `(#'#%app ,@(map fn (syntax->list #'exprs))))]
           [(#%datum . _)
            stx]
           [(#%top . var)
            stx]
           [else
            (error 'expr-syntax-object-iterator "unknown expr: ~a\n" (syntax-object->datum stx))])))
     'expr-syntax-object-iterator
     'caller))
  
  
  )