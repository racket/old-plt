
(module invoke mzscheme

  (import "sigutil.ss")

  (define do-define-values/invoke-unit
    (lambda (stx)
      (syntax-case stx ()
	[(global? exports unite prefix imports orig)
	 (let* ([badsyntax (lambda (s why)
			     (raise-syntax-error
			      (if (syntax-e (syntax global?))
				  'global-define-values/invoke-unit
				  'define-values/invoke-unit)
			      (format "bad syntax (~a)" why)
			      (syntax orig)
			      s))]
		[symcheck (lambda (s)
			    (or (symbol? s)
				(badsyntax s "not an identifier")))])
	   (unless (stx-list? (syntax exports))
	     (badsyntax (syntax exports) "not a sequence of identifiers"))
	   (for-each symcheck (syntax->list (syntax exports)))
	   (unless (or (not (syntax-e (syntax prefix)))
		       (identifier? (syntax prefix)))
	     (badsyntax (syntax prefix) "prefix is not an identifier"))
	   (for-each symcheck (syntax->list (syntax imports)))
	   
	   (with-syntax ([(tagged-export ...) 
			  (if (syntax-e (syntax prefix))
			      (let ([prefix (string-append
					     (symbol->string 
					      (syntax-e (syntax prefix)))
					     ":")])
				(map (lambda (s)
				       (datum->syntax
					(string->symbol
					 (string-append
					  prefix
					  (symbol->string s)))
					s s))
				     (syntax->list (syntax exports)))
				(syntax exports)))]
			 [extract-unit (syntax (unit 
						 (import . exports)
						 (export)
						 (values . exports)))])
	     (with-syntax ([invoke-unit (syntax (invoke-unit
						 (compound-unit
						  (import . imports)
						  (link [unit-to-invoke (unite . imports)]
							[export-extractor 
							 (extract-unit (unit-to-invoke . exports))])
						  (export))
						 . imports))])
	       (if (syntax-e (syntax global?))
		   (syntax (let-values ([(tagged-export ...) invoke-unit])
			     (global-defined-value 'tagged-export tagged-export)
			     ...
			     (void)))
		   (syntax (define-values (tagged-export...) invoke-unit))))))])))
    
    (define-syntax define-values/invoke-unit
      (lambda (stx)
	(with-syntax ([orig stx])
	  (syntax-case stx ()
	    [(_ exports unit name . imports) 
	     (syntax (do-define-values/invoke-unit #f exports unit name imports orig))]
	    [(_ exports unit) 
	     (syntax (do-define-values/invoke-unit #f exports unit #f () orig))]))))
    
    (define-syntax global-define-values/invoke-unit
      (lambda (stx)
	(with-syntax ([orig stx])
	  (syntax-case stx ()
	    [(_ exports unit name . imports) 
	     (syntax (do-define-values/invoke-unit #t exports unit name imports orig))]
	    [(_ exports unit) 
	     (syntax (do-define-values/invoke-unit #t exports unit #f () orig))]))))
    
    (define do-define-values/invoke-unit/sig
      (lambda (stx)
	(syntax-case stx ()
	  [(_ global? signame unite prefix imports orig)
	   (let* ([formname (if (syntax-e (syntax global?))
				'global-define-values/invoke-unit/sig
				'define-values/invoke-unit/sig)]
		  [badsyntax (lambda (s why)
			       (raise-syntax-error
				formname
				(format "bad syntax (~a)" why)
				(syntax orig)
				s))])
	     (unless (or (not (syntax-e (syntax prefix)))
			 (identifier? (syntax prefix)))
	       (badsyntax (syntax prefix) "prefix is not an identifier"))
	     (let ([ex-sig (get-sig formname (syntax orig) #f (syntax signame))])
	       (let ([ex-exploded (explode-sig ex-sig)]
		     [ex-flattened (flatten-signatures (list ex-sig))])
		 (let ([im-sigs
			(map
			 (lambda (s)
			   (get-sig formname (syntax orig) #f s))
			 (syntax->list (syntax imports)))])
		   (let ([im-explodeds (explode-named-sigs im-sigs)]
			 [im-flattened (flatten-signatures im-sigs)])
		     (with-syntax ([dv/iu (if (syntax-e (syntax global?))
					      (quote-syntax global-define-values/invoke-unit)
					      (quote-syntax define-values/invoke-unit))]
				   [ex-flattened ex-flattened]
				   [ex-exploded ex-exploded]
				   [im-explodeds im-explodeds]
				   [im-flattened im-flattened]
				   [formname formname])
		       (syntax
			(dv/iu
			 ex-flattened
			 (let ([unit-var unite])
			   (verify-linkage-signature-match
			    'formname
			    '(invoke)
			    (list unit-var)
			    '(ex-exploded)
			    '(im-explodeds))
			   (unit/sig->unit unit-var))
			 prefix
			 im-flatteneds))))))))])))
    
    (define define-values/invoke-unit/sig
      (lambda (stx)
	(with-syntax ([orig stx])
	  (syntax-case stx ()
	    [(_ signame unit prefix . imports)
	     (syntax (do-define-values/invoke-unit/sig #f signame unit prefix imports orig))]
	    [(_ signame unit)
	     (syntax (do-define-values/invoke-unit/sig #f signame unit #f () orig))]))))

    (define global-define-values/invoke-unit/sig
      (lambda (stx)
	(with-syntax ([orig stx])
	  (syntax-case stx ()
	    [(_ signame unit prefix . imports)
	     (syntax (do-define-values/invoke-unit/sig #t signame unit prefix imports orig))]
	    [(_ signame unit)
	     (syntax (do-define-values/invoke-unit/sig #t signame unit #f () orig))]))))

    (export define-values/invoke-unit
	    define-values/invoke-unit/sig
	    global-define-values/invoke-unit
	    global-define-values/invoke-unit/sig))
