(module debugger-annotate mzscheme
  
  (require (prefix kernel: (lib "kerncase.ss" "syntax"))
           (lib "contracts.ss")
           "shared.ss"
           "marks.ss")
  
  (provide/contract [annotate-top-level (-> syntax? syntax?)]
                    [annotate-expr (-> syntax? syntax?)])
 
  (define (annotate-top-level stx)
    (top-level-expr-iterator stx))
  
  (define (annotate-expr stx)
    (let ([result-box (box null)])
      ((f-maker null null result-box) stx 'tail null)))
  

  ;; TEMPLATE FUNCTIONS:
  ;;  these functions' definitions follow the data definitions presented in the Syntax
  ;;  chapter of the MzScheme Manual. 
  
  (define (top-level-expr-iterator stx)
    (let ([rebuild
           (lambda (new-stx) 
             (rebuild-stx (syntax->list new-stx) stx))])
      (kernel:kernel-syntax-case stx #f
        [(module identifier name (#%plain-module-begin . module-level-exprs))
         (rebuild #`(module identifier name 
                      (#%plain-module-begin 
                       #,@(map module-level-expr-iterator
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
         (rebuild #`(begin #,@(map top-level-expr-iterator 
                                   (syntax->list #'top-level-exprs))))]
        [(require . require-specs)
         stx]
        [(require-for-syntax . require-specs)
         stx]
        [else
         (annotate-expr stx)])))
    
  (define (expr-iterator fn stx)
    (let* ([tr-fn (lambda (stx) (fn stx 'tail null))]        ; tail position
           [nt-fn (lambda (stx) (fn stx 'non-tail null))]    ; non-tail position
           [lb-fn (lambda (bindings) 
                    (lambda (stx) (fn stx 'lambda-body bindings)))] ; lambda body
           [lt-fn (lambda (bindings) 
                    (lambda (stx) (fn stx 'tail bindings)))]     ; body of a let
           [rh-fn (lambda (bindings)
                    (lambda (stx) (fn stx 'non-tail bindings)))] ; let rhs
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
                   `(,#'arglist ,@(map (lb-fn (arglist-bindings #'arglist)) 
                                       (syntax->list #'bodies)))]
                   [else
                    (error 'expr-syntax-object-iterator 
                           "unexpected (case-)lambda clause: ~a" 
                           (syntax-object->datum stx))]))]
             [let-values-abstraction
               (lambda (stx rec?)
                 (kernel:kernel-syntax-case stx #f
                   [(kwd (((variable ...) rhs) ...) . bodies)
                    (let* ([new-bindings 
                            (varref-set-union 
                             (map syntax->list 
                                  (syntax->list #'((variable ...) ...))))]
                           [rhs-new-bindings (if rec? new-bindings null)])
                      (with-syntax ([(rhs-a ...) (map (rh-fn rhs-new-bindings) 
                                                      (syntax->list #'(rhs ...)))])
                        (rebuild #`(kwd (((variable ...) rhs-a) ...) 
                                        #,@(map (lt-fn new-bindings)
                                                (syntax->list #'bodies))))))]
                   [else
                    (error 'expr-syntax-object-iterator 
                           "unexpected let(rec) expression: ~a"
                           (syntax-object->datum stx))]))]) 
         (kernel:kernel-syntax-case stx #f
           [var-stx
            (identifier? (syntax var-stx))
            stx]
           [(lambda . clause)
            (rebuild #`(lambda #,@(lambda-clause-abstraction #'clause)))]
           [(case-lambda . clauses)
            (rebuild #`(case-lambda #,@(map lambda-clause-abstraction
                                            (syntax->list #'clauses))))]
           [(if test then)
            (rebuild #`(if #,(nt-fn #'test) #,(tr-fn #'then)))]
           [(if test then else)
            (rebuild #`(if #,(nt-fn #'test) #,(tr-fn #'then) #,(tr-fn #'else)))]
           [(begin . bodies)
            (rebuild #`(begin #,@(fn-map-begin #'bodies)))]
           [(begin0 . bodies)
            (rebuild #`(begin0 #,@(fn-map-begin0 #'bodies)))]
           [(let-values . _)
            (let-values-abstraction stx #f)]
           [(letrec-values . _)
            (let-values-abstraction stx #t)]
           [(set! var val)
            (rebuild #`(set! var #,(nt-fn #'val)))]
           [(quote _)
            stx]
           [(quote-syntax _)
            stx]
           [(with-continuation-mark key mark body)
            (rebuild #`(with-continuation-mark #,(nt-fn #'key) #,(nt-fn #'mark) 
					       #,(tr-fn #'body)))]
           [(#%app . exprs)
            (rebuild #`(#%app #,@(fn-map-begin0 #'exprs)))]
           [(#%datum . _)
            stx]
           [(#%top . var)
            stx]
           [else
            (error 'expr-syntax-object-iterator "unknown expr: ~a" 
                   (syntax-object->datum stx))])))
  
  
  ;; ANNOTATION
  ;;  These functions encapsulate the domain knowledge needed by the annotater
  
  ; free-vars: (syntax? -> (listof syntax?))
  ;  if this expression is a variable reference, return it in a list. 
  ;  otherwise return null.
  
  (define (free-vars stx)
    (kernel:kernel-syntax-case stx #f
      [(#%top . var)
       (list #'var)]
      [var-stx
       (identifier? stx)
       (list stx)]
      [stx
       null]))
  
  ; arglist-bindings : (syntax? -> (listof syntax?))
  ;  return a list of the names in the arglist
  
  (define (arglist-bindings arglist-stx)
    (syntax-case arglist-stx ()
      [var
       (identifier? arglist-stx)
       (list arglist-stx)]
      [(var ...)
       (syntax->list arglist-stx)]
      [(var . others)
       (cons #'var (arglist-bindings #'others))]))
  
  ; tail-bound : (binding-set? (listof syntax?) symbol -> binding-set?)
  ;  prior: the variables that were tail-bound in the enclosing expression
  ;  newly-bound: the variables whose bindings were created in the enclosing 
  ;               expression
  ;  tailness: 'lambda-body if this expression is the body of a lambda,
  ;            'tail if this expression is tail wrt the enclosing expression, and
  ;            'non-tail otherwise
  
  (define (tail-bound prior newly-bound tailness)
    (case tailness
      ((lambda-body) 'all)
      ((tail) (binding-set-union (list prior newly-bound)))
      ((non-tail) newly-bound)
      (else (error 'tail-bound "unexpected value ~s for tailness argument" 
                   tailness))))
  
  
  (define (f-maker prior-tail-bound prior-lexically-bound result-box)
    (lambda (stx we-are-tail? newly-bound-in-parent)
      (set-box! result-box (append (free-vars stx) (unbox result-box)))
      (let* ([new-result-box (box null)]
             [my-tail-bound (tail-bound prior-tail-bound newly-bound-in-parent 
                                        we-are-tail?)]
             [my-lexically-bound (varref-set-union
                                  (list prior-lexically-bound newly-bound-in-parent))]
             [new-f (f-maker my-tail-bound my-lexically-bound new-result-box)]
             [sub-annotated (expr-iterator new-f stx)] ; recursive call
             [debug-info (make-debug-info stx my-tail-bound my-lexically-bound 
                                          'none #f)])
        #`(with-continuation-mark #,debug-key #,debug-info #,sub-annotated))))
  
  
  )