(module websubmit-macros mzscheme
  (require (lib "unitsig.ss"))
  (provide define-structs-and-sig/provide)
  
  (define-syntax (make-struct-defs stx)
    (syntax-case stx (struct struct/sig)
      [(_ ) #'()]
      [(_ (struct (name super) (fields ...) stuff ...) rest ...)
       (let ([more ((syntax-local-value #'make-struct-defs) #'(_ rest ...))])
         #`((define-struct (name super) (fields ...) stuff ...)
            #,@more))]
      [(_ (struct/sig (name super) (fields ...) stuff ...) rest ...)
       (let ([more ((syntax-local-value #'make-struct-defs) #'(_ rest ...))])
         #`((define-struct (name super) (fields ...) stuff ...)
            #,@more))]
      [(_ (struct name (fields ...) stuff ...) rest ...)
       (let ([more ((syntax-local-value #'make-struct-defs) #'(_ rest ...))])
         #`((define-struct name (fields ...) stuff ...)
            #,@more))]
      [(_ (struct/sig name (fields ...) stuff ...) rest ...)
       (let ([more ((syntax-local-value #'make-struct-defs) #'(_ rest ...))])
         #`((define-struct name (fields ...) stuff ...)
            #,@more))]
      [(_ bad-expr rest ...)
       (raise-syntax-error #f "Incorrect struct expression" #'bad-expr)]))
  
  (define-syntax (make-struct-provides stx)
    (syntax-case stx (struct struct/sig)
      [(_ ) #'()]
      [(_ (struct (name super) (fields ...) stuff ...) rest ...)
       (let ([more ((syntax-local-value #'make-struct-provides) #'(_ rest ...))])
         #`((provide (struct name (fields ...)))
            #,@more))]
      [(_ (struct/sig (name super) (fields ...) stuff ...) rest ...)
       (let ([more ((syntax-local-value #'make-struct-provides) #'(_ rest ...))])
         #`((provide (struct name (fields ...)))
            #,@more))]
      [(_ (struct name (fields ...) stuff ...) rest ...)
       (let ([more ((syntax-local-value #'make-struct-provides) #'(_ rest ...))])
         #`((provide (struct name (fields ...)))
            #,@more))]
      [(_ (struct/sig name (fields ...) stuff ...) rest ...)
       (let ([more ((syntax-local-value #'make-struct-provides) #'(_ rest ...))])
         #`((provide (struct name (fields ...)))
            #,@more))]))
  
  (define-syntax (make-sig-elts stx)
    (syntax-case stx (struct struct/sig)
      [(_ ) #'()]
      [(_ (struct (name super) (fields ...) stuff ...) rest ...)
       (let ([more ((syntax-local-value #'make-sig-elts) #'(_ rest ...))])
         more)]
      [(_ (struct/sig (name super) (fields ...) stuff ...) rest ...)
       (let ([more ((syntax-local-value #'make-sig-elts) #'(_ rest ...))])
         (with-syntax ([new-name (string->symbol
                                  (string-append "get-" (symbol->string (syntax-object->datum #'name))))])
           #`(new-name #,@more)))]
      [(_ (struct name (fields ...) stuff ...) rest ...)
       (let ([more ((syntax-local-value #'make-sig-elts) #'(_ rest ...))])
         more)]
      [(_ (struct/sig name (fields ...) stuff ...) rest ...)
       (let ([more ((syntax-local-value #'make-sig-elts) #'(_ rest ...))])
         (with-syntax ([new-name (string->symbol
                                  (string-append "get-" (symbol->string (syntax-object->datum #'name))))])
           #`(new-name #,@more)))]))
  
  
  (define-syntax (define-structs-and-sig/provide stx)
    (syntax-case stx ()
      [(_ sig-name struct-exprs ...)
       (let ([struct-defs ((syntax-local-value #'make-struct-defs) #'(_ struct-exprs ...))]
             [struct-provides ((syntax-local-value #'make-struct-provides) #'(_ struct-exprs ...))]
             [sig-elts ((syntax-local-value #'make-sig-elts) #'(_ struct-exprs ...))])
         #`(begin (define-signature sig-name #,sig-elts)
                  (provide sig-name)
                  #,@struct-defs
                  #,@struct-provides))]))
  )
