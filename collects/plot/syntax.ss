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
    
  (provide r-lambda ))