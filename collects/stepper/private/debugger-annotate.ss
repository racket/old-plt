(module debugger-annotate mzscheme
  
  (require (prefix kernel: (lib "kerncase.ss" "syntax"))
           (lib "contracts.ss")
           "shared.ss"
           "marks.ss"
           "breakpoint-token.ss")
  
  (define (annotate-expr stx)
    (let ([result-box (box null)])
      ((f-maker null null result-box) stx #t)))
  
  (define (free-vars stx)
    (kernel:kernel-syntax-case stx #f
      [(#%top . var)
       (list #'var)]
      [var-stx
       (identifier? stx)
       (list stx)]
      [stx
       null]))
  
  (define (arglist-bindings arglist-stx)
    (syntax-case arglist-stx ()
      [var
       (identifier? arglist-stx)
       (list arglist-stx)]
      [(var ...)
       (syntax->list arglist-stx)]
      [(var . others)
       (cons #'var (arglist-bindings #'others))]))
  
  (define (tail-bound prior newly-bound we-are-tail?)
    (if we-are-tail?
        (binding-set-union (list prior newly-bound))
        null))
  
  (define (newly-bound stx)
    (kernel:kernel-syntax-case stx #f
      [(lambda arglist . rest)
       (arglist-bindings #'arglist)]
      [(case-lambda . clauses)
       (apply append
              (map (lambda (clause)
                     (syntax-case clause ()
                       [(arglist . rest)
                        (arglist-bindings #'arglist)]))
                   (syntax->list #'clauses)))]
      [(let-values (((var ...) . stuff) ...) . dont-care)
       (apply append (map syntax->list (syntax->list #'((var ...) ...))))]
      [(letrec-values (((var ...) . stuff) ...) . dont-care)
       (apply append (map syntax->list (syntax->list #'((var ...) ...))))]
      [etc
       null]))
        
        
  
  (define (f-maker prior-tail-bound newly-bound-in-parent result-box)
    (lambda (stx we-are-tail?)
      (set-box! result-box (append (free-vars stx) (unbox result-box)))
      (let* ([new-result-box (box null)]
             [my-tail-bound (tail-bound prior-tail-bound newly-bound-in-parent we-are-tail?)]
             [new-f (f-maker my-tail-bound (newly-bound stx) new-result-box)]
             [sub-annotated (expr-iterator new-f stx)] ; recursive call
             [debug-info (make-debug-info stx my-tail-bound (append (free-vars stx) (unbox result-box)) 'none #f)])
        #`(with-continuation-mark #,debug-key #,debug-info #,sub-annotated))))
  
  (define (expr-iterator fn stx)
      (let* ([tr-fn (lambda (stx) (fn stx #t))]
             [nt-fn (lambda (stx) (fn stx #f))]
             [fn-map-begin/0 
              (lambda (stx begin0?)
                (let* ([bodies (syntax->list stx)]
                       [reversed-bodies (reverse bodies)]
                       [last-body (car reversed-bodies)]
                       [all-but-last (reverse! (cdr reversed-bodies))])
                  (if begin0?
                      `(,@(map nt-fn all-but-last) ,(nt-fn last-body))
                      `(,@(map nt-fn all-but-last) ,(tr-fn last-body)))))]
             [fn-map-begin  (lambda (stx) (fn-map-begin/0 stx #f))]
             [fn-map-begin0 (lambda (stx) (fn-map-begin/0 stx #t))]
             [rebuild
              (lambda (new-stx) 
                (rebuild-stx (syntax->list new-stx) stx))]
             [lambda-clause-abstraction
              (lambda (clause)
                (kernel:kernel-syntax-case clause #f
                  [(arglist . bodies)
                   `(,#'arglist ,@(fn-map-begin #'bodies))]
                   [else
                    (error 'expr-syntax-object-iterator "unexpected (case-)lambda clause: ~a\n" (syntax-object->datum stx))]))]
              [let-values-abstraction
               (lambda (stx)
                 (kernel:kernel-syntax-case stx #f
                   [(kwd ((variable ...) ...) . bodies)
                    (rebuild #`(kwd ((variable ...) ...) #,@(fn-map-begin #'bodies)))]
                   [else
                    (error 'expr-syntax-object-iterator "unexpected let(rec) expression: ~a\n" (syntax-object->datum stx))]))]) 
         (kernel:kernel-syntax-case stx #f
           [var-stx
            (identifier? (syntax var-stx))
            stx]
           [(lambda . clause)
            (rebuild #`(lambda #,@(lambda-clause-abstraction #'clause)))]
           [(case-lambda . clauses)
            (rebuild #`(case-lambda #,@(map lambda-clause-abstraction (syntax->list #'clauses))))]
           [(if test then)
            (rebuild #`(if #,(nt-fn #'test) #,(tr-fn #'then)))]
           [(if test then else)
            (rebuild #`(if #,(nt-fn #'test) #,(tr-fn #'then) #,(tr-fn #'else)))]
           [(begin . bodies)
            (rebuild #`(begin #,@(fn-map-begin #'bodies)))]
           [(begin0 . bodies)
            (rebuild #`(begin0 #,@(fn-map-begin0 #'bodies)))]
           [(let-values . _)
            (let-values-abstraction stx)]
           [(letrec-values . _)
            (let-values-abstraction stx)]
           [(set! var val)
            (rebuild #`(set! var #,(nt-fn #'val)))]
           [(quote _)
            stx]
           [(quote-syntax _)
            stx]
           [(with-continuation-mark key mark body)
            (rebuild #`(with-continuation-mark #,(nt-fn #'key) #,(nt-fn #'mark) #,(tr-fn #'body)))]
           [(#%app . exprs)
            (rebuild #`(#%app #,@(fn-map-begin0 #'exprs)))]
           [(#%datum . _)
            stx]
           [(#%top . var)
            stx]
           [else
            (error 'expr-syntax-object-iterator "unknown expr: ~a\n" (syntax-object->datum stx))])))
  
  (define (top-level-expr-iterator stx)
    (let ([rebuild
           (lambda (new-stx) 
             (rebuild-stx (syntax->list new-stx) stx))])
      (kernel:kernel-syntax-case #f
        [(module identifier name (#%plain-module-begin . module-level-exprs))
         (rebuild #`(module identifier name (#%plain-module-begin #,@(map module-level-expr-iterator
                                                                          (syntax->list #'module-level-exprs)))))]
        [else-stx
         (general-top-level-expr-iterator stx)])))

  (define (module-level-expr-iterator stx)
    (kernel:kernel-syntax-case stx #f
      [(provide . provide-specs)
       stx]
      [else-stx
       (general-top-level-expr-iterator stx)]))
  
  (define (general-top-level-expr-iterator stx)
    (let ([rebuild 
           (lambda (new-stx) 
             (rebuild-stx (syntax->list new-stx) stx))])
      (kernel:kernel-syntax-case stx #f
        [(define-values (var ...) expr)
         (rebuild #`(define-values (var ...) #,(annotate-expr #'expr)))]
        [(define-syntaxes (var ...) expr)
         stx]
        [(begin . top-level-exprs)
         (rebuild #`(begin #,@(map top-level-expr-iterator (syntax->list #'top-level-exprs))))]
        [(require . require-specs)
         stx]
        [(require-for-syntax . require-specs)
         stx]
        [else
         (annotate-expr stx)])))
  
  
  (provide/contract [annotate (-> syntax? syntax?)])
  
  )