
;; Defines a language to be used by info.ss files

(module infotab mzscheme
  
  (define-syntax info-module-begin
    (lambda (stx)
      (syntax-case stx ()
	[(_ defn ...)
	 (let ([defns (syntax->list (syntax (defn ...)))])
	   (let ([names (map (lambda (defn)
			       (syntax-case defn (define)
				 [(define var val)
				  (syntax var)]
				 [_else (raise-syntax-error
					 'infotab-module
					 "not a well-formed definition"
					 stx
					 defn)]))
			     defns)])
	     (let ([dup (check-duplicate-identifier names)])
	       (when dup
		 (raise-syntax-error
		  'infotab-module
		  "duplicate definition"
		  stx
		  dup)))
	     (with-syntax ([(name ...) names])
	       (syntax
		(#%module-begin
		 defn ...
		 (define #%info-lookup
		   (case-lambda
		    [(n) (#%info-lookup n (lambda () (error 'info.ss "no info for ~a" n)))]
		    [(n fail)
		     (case n
		       [(name) name]
		       ...
		       [else (fail)])]))
		 (provide #%info-lookup))))))])))

  (provide (rename info-module-begin #%module-begin)
	  #%app #%datum #%unbound
	  define quote
	  list cons car cdr quasiquote unquote unquote-splicing
	  build-path collection-path))


	
