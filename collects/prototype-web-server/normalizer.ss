(module normalizer mzscheme
  (require-for-template mzscheme)
  (provide normalize-term
           normalize-definition
           )
  ;; **************************************************
  ;; SOURCE LANGUAGE
  ;;
  ;; program ::= definition* expr
  ;;
  ;; definition ::= (define-values (var ...) expr)
  ;;
  ;; expr ::= var
  ;;       |  (lambda (var ...) expr)
  ;;       |  (if expr expr)
  ;;       |  (if expr expr expr)
  ;;       |  (let-values ([(var)] expr) expr)
  ;;       |  (#%app expr ...)
  ;;       |  (#%datum . datum)
  ;;       |  (#%top . var)
  ;;       |  (begin expr ...)
  
  ;; **************************************************
  ;; TARGET LANGUAGE
  ;;
  ;; program ::= definition* expr
  ;;
  ;; definition ::= (define-values (var ...) expr)
  ;;
  ;; expr ::= w | r | (#%app (lambda (var) expr) r)
  ;;
  ;; r ::= (if w expr)
  ;;    |  (if w expr expr)
  ;;    |  (#%app w w ...)
  ;;
  ;;   w  ::= var | (#%top . var) | value
  ;;   value ::=  (#%datum . datum)
  ;;          | (lambda (var ...) expr)
  
  ;; **************************************************
  ;; **************************************************
  
  ;; id: alpha -> alpha
  ;; the identity function
  (define (id x) x)
  
  ;; normalize-definition: definition -> expr
  (define (normalize-definition def)
    (syntax-case def (define-values)
      [(define-values (ids ...) body-expr)
       #`(define-values (ids ...) #,(normalize-term #'body-expr))]
      [_else
       (raise-syntax-error #f "normalize-definition: dropped through" def)]))
  
  ;; normalize-term: source-expr -> target-expr
  ;; transform a term into an application chain
  (define (normalize-term src-expr)
    (normalize id src-expr))
  
  ;; normalize: (w -> target-expr) source-expr -> target-expr
  ;; normalize an expression given as a context and sub-expression
  (define (normalize ctxt expr)
    (syntax-case expr (lambda if let-values #%app #%datum #%top quote begin)
      [(lambda (formals ...) body)
       (ctxt #`(lambda (formals ...) #,(normalize-term #'body)))]
      [(lambda . anything)
       (raise-syntax-error #f "Not all lambda-expressions supported" expr)]
      [(if tst-expr csq-expr)
       (normalize
        (compose ctxt
                 (lambda (val)
                   #`(if #,val #,(normalize-term #'csq-expr))))
        #'tst-expr)]
      [(if tst-expr csq-expr alt-expr)
       (normalize
        (compose ctxt
                 (lambda (val)
                   #`(if #,val
                         #,(normalize-term #'csq-expr)
                         #,(normalize-term #'alt-expr))))
        #'tst-expr)]
      [(let-values ([(var) rhs-expr]) body)
       (normalize ctxt #'(#%app (lambda (var) body) rhs-expr))]
      [(let-values . anything)
       (raise-syntax-error #f "Not all let-values-expressions supported" expr)]
      [(#%app expr-rator expr-rands ...)
       (normalize
        (lambda (val0)
          (normalize*
           (compose ctxt
                    (lambda (rest-vals) #`(#%app #,val0 #,@rest-vals)))
           (syntax->list #'(expr-rands ...))))
        #'expr-rator)]
      [(#%datum . datum) (ctxt expr)]
      [(#%top . var) (ctxt expr)]
      [(begin) (normalize ctxt #'(#%app (#%top . void)))]
      [(begin last-expr) (normalize ctxt #'last-expr)]
      [(begin first-expr . rest-expr)
       (normalize ctxt #'(let-values ([(throw-away) first-expr])
                           (begin . rest-expr)))]
      [(quote datum) (ctxt expr)]
      [x (symbol? (syntax-object->datum #'x))
         (ctxt expr)]
      [_else
       (raise-syntax-error #f "normalize: unsupported form" expr)]))
  
  ;; normalize*: ((listof w) -> target-expr) (listof source-expr) -> target-expr
  ;; normalize an expression given as a context and list of sub-expressions
  (define (normalize* multi-ctxt exprs)
    (cond
      [(null? exprs) (multi-ctxt '())]
      [else
       (normalize
        (lambda (val)
          (normalize*
           (lambda (rest-vals)
             (multi-ctxt (cons val rest-vals)))
           (cdr exprs)))
        (car exprs))]))
  
  ;; a context is either
  ;;    frame
  ;;    (compose context frame)
  
  ;; a frame is either
  ;;    w -> target-redex
  ;;    (listof w) -> target-redex
  
  ;; compose: (w -> target-expr) (alpha -> target-redex) -> (alpha -> target-expr)
  ;; compose a context with a frame
  (define (compose ctxt frame)
    (if (eq? ctxt id) frame
        (lambda (val)
          (let ([x (datum->syntax-object #f (gensym 'x))])
            #`(#%app (lambda (#,x) #,(ctxt x)) #,(frame val))))))
  )

