
(module exstruct mzscheme
  
  (define-syntax export-struct
    (lambda (stx)
      (syntax-case stx ()
	[(_ name (field ...))
	 (and (identifier? (syntax name))
	      (andmap identifier?
		      (syntax->list (syntax (field ...)))))
	 (let ([name (symbol->string (syntax-e (syntax name)))]
	       [fields (map symbol->string 
			    (map syntax-e 
				 (syntax->list (syntax (field ...)))))]
	       [+ string-append])
	   (with-syntax ([names
			  (map (lambda (x)
				 (datum->syntax-object (syntax name) x (syntax name)))
			       (map string->symbol
				    (append
				     (list 
				      (+ "struct:" name)
				      (+ "make-" name)
				      (+ name "?"))
				     (apply
				      append
				      (map
				       (lambda (f) 
					 (list 
					  (+ name "-" f)
					  (+ "set-" name "-" f "!")))
				       fields)))))])
	     (syntax (provide . names))))])))

  (define-syntax define-struct/export
    (lambda (stx)
      (syntax-case stx ()
	[(_ name (field ...))
	 (and (identifier? (syntax name))
	      (andmap identifier?
		      (syntax->list (syntax (field ...)))))
	 (syntax
	  (begin
	    (define-struct name (field ...))
	    (export-struct name (field ...))))]
	[(_ (name expr) (field ...))
	 (and (identifier? (syntax name))
	      (andmap identifier?
		      (syntax->list (syntax (field ...)))))
	 (syntax
	  (begin
	    (define-struct (name expr) (field ...))
	    (export-struct name (field ...))))])))

  (provide export-struct
	  define-struct/export))

