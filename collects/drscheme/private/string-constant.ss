(module string-constant mzscheme
  (require-for-syntax (lib "etc.ss"))
  
  (provide string-constant)
  
  (define-syntax (string-constant stx)
    (define english-string-constants
      (let* ([filename (build-path (this-expression-source-directory) "english-string-constants.ss")]
             [sexp (call-with-input-file filename read 'text)])
        (unless (and (list? sexp)
                     (andmap (lambda (x) 
                               (and (list? x)
                                    (= 2 (length x))
                                    (symbol? (car x))
                                    (string? (cadr x))))
                             sexp))
          (error 'english-string-constants
                 "expected ((sym str) ...), got: ~s" sexp))
        (let ([ht (make-hash-table)])
          (for-each (lambda (x) (hash-table-put! ht (car x) (cadr x)))
                    sexp)
          ht)))
    
    ;; use mred resources to save this -- framework prefs won't work
    ;; since this is elaboration time, not run-time of DrScheme
    (define language 'english)
    
    (define string-constants
      (case language
        [(english) english-string-constants]))
    
    (syntax-case stx ()
      [(_ name)
       (let ([datum (syntax-object->datum (syntax name))])
         (unless (symbol? datum)
           (raise-syntax-error #f
			       (format "expected name, got: ~s" datum)
			       stx))
	 (let ([table-entry (hash-table-get string-constants datum (lambda () #f))])
	   (unless table-entry
	     (raise-syntax-error 
              (quote-syntax string-constant)
              (format "~a is not a known string constant" datum) stx))
           (with-syntax ([constant table-entry])
                        (syntax constant))))])))