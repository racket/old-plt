
;; Defines a language to be used by info.ss files

(module infotab mzscheme
  (define-syntax info-module-begin
    (lambda (stx)
      (syntax-case stx ()
	[(mod-beg defn ...)
	 (let ([defns (syntax->list (syntax (defn ...)))])
	   (let ([names (map (lambda (defn)
			       (syntax-case defn (define)
				 [(define var val)
				  (syntax var)]
				 ;; In case it gets expanded:
				 [(define-values (var) val)
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
	     (with-syntax ([(name ...) names]
			   [#%info-lookup (datum->syntax-object
					   (syntax mod-beg) ; target module's context
					   '#%info-lookup)])
	       (syntax
		(#%plain-module-begin
		 defn ...
		 (define #%info-lookup
		   (case-lambda
		    [(n) (#%info-lookup n (lambda () (error 'info.ss "no info for ~a" n)))]
		    [(n fail)
		     (unless (and (procedure? fail)
				  (procedure-arity-includes? fail 0))
		       (error
			'info.ss
			"expected second argument to be a procedure that takes no arguments, got: ~e"
			fail))
		     (case n
		       [(name) name]
		       ...
		       [else (fail)])]))
		 (provide #%info-lookup))))))])))

  (provide (rename info-module-begin #%module-begin)
	   #%app #%datum #%top
	   define quote
	   list cons car cdr quasiquote unquote unquote-splicing
	   list* append reverse
	   path->string build-path collection-path
	   system-library-subpath))
