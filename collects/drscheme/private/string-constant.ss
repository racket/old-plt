(module string-constant mzscheme
  (require-for-syntax "string-constants.ss")
  
  (provide string-constant)
  
  (define-syntax (string-constant stx)
    (syntax-case stx ()
      [(string-constant name)
       (let ([datum (syntax-object->datum (syntax name))])
         (unless (symbol? datum)
           (raise-syntax-error #f
			       (format "expected name, got: ~s" datum)
			       stx))
	 (let ([table-entry (assoc datum string-constants)])
	   (unless table-entry
	     (raise-syntax-error #f (format "couldn't find ~s in table" datum) stx))
         (with-syntax ([constant (cadr table-entry)])
	   (syntax constant))))])))