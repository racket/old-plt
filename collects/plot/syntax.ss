(module syntax mzscheme
  
  (require
   (lib "class.ss")
   (lib "etc.ss"))
  
  ;; create a renderer from the body of a function
  ;; attemts to illiminate code-repeat
  (define-syntax (r-lambda stx)
    (define (join-identifier prefix ident)
      (datum->syntax-object 
       ident 
       (string->symbol (string-append (symbol->string prefix )(symbol->string (syntax-e ident)))) ))
    (syntax-case stx ()
      [(_ data view ((var default) ...) body)
       #'(r-lambda-internal data view ((var default) ...) () body)]
      [(_ data view (field ...) ((var default) ...) body)
       (let ((accessors (map (lambda (f) (join-identifier 'get- f)) (syntax-e #'(field ...)))))
         (with-syntax (((getter ...) accessors))
           #'(r-lambda-internal data view ((var default) ...) ((field getter) ...) body)))]))
  
  (define-syntax r-lambda-internal
    (syntax-rules ()
      [(_ data view ((var default) ...) ((value accessor) ...) body)
       (opt-lambda [data (args null)]
         (let ((var (cond [(assq 'var args) => cadr]
                          [else default]))
               ...)
           (lambda (view)
             (let ((value (send view accessor)) ...)
               body))))]))
  
  
  ; this is used for the 'plot and 'plot3d functions
  ; maybe should be replaced by instantiate
  (define-syntax lookup-lambda
    (syntax-rules ()
      [(_ ((var default) ...) body)
       (lookup-lambda unused unused2 ((var default) ...) body)]
      [(_ args items ((var default) ...) body)
       (lambda [args . items]
         (let ((var (cond [(assq 'var args) => cadr]
                          [else default]))
               ...)
           body))]))
  
  (provide lookup-lambda r-lambda ))