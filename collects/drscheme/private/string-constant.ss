(module string-constant mzscheme
  (require-for-syntax "string-constants.ss")
  
  (provide string-constant)
  
  (define-syntax (string-constant stx)
    (syntax-case stx ()
      [(string-constant name)
       (let ([datum (syntax-object->datum (syntax name))])
         (unless (symbol? datum)
           (raise-syntax-error (syntax string-constant) (format "expected name, got: ~s" datum))) 
         (with-syntax ([constant (assoc name string-constants)])
                      (syntax constant)))])))