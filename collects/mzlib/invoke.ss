
(module invoke mzscheme

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
	     (badsyntax exports "not a sequence of identifiers"))
	   (for-each symcheck (syntax->list (syntax exports)))
	   (unless (or (not (syntax-e (syntax prefix)))
		       (identifier? (syntax prefix)))
	     (badsyntax prefix "prefix is not an identifier"))
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
						 . imports)]])
	       (if (syntax-e (syntax global?))
		   (syntax (let-values ([(tagged-export ...) invoke-unit])
			     (global-defined-value 'tagged-export tagged-export)
			     ...
			     (void)))
		   (define-values (tagged-export...) invoke-unit)))))])))
    
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
    
    (define (do-define-values/invoke-unit/sig global? signame unit prefix imports)
      (let* ([formname (if global?
			   'global-define-values/invoke-unit/sig
			   'define-values/invoke-unit/sig)]
	     [badsyntax (lambda (s why)
			  (raise-syntax-error
			   formname
			   (format "bad syntax (~a)" why)
			   `(,formname
			     ,signame ,unit ,prefix ,@imports)
			   s))]
	     [unit-var (gensym)])
	(let-values ([(ex-exploded ex-flattened) (extract-signature signame badsyntax)]
		       [(im-explodeds im-flatteneds)
			(let loop ([l imports][el null][fl null])
			  (if (null? l)
			      (values (reverse! el) (reverse! fl))
			      (let-values ([(e f) (extract-named-signature (car l) badsyntax)])
				(loop (cdr l) (cons e el) (cons f fl)))))])
	  `(,(if global?
		 'global-define-values/invoke-unit 
		 'define-values/invoke-unit)
	    ,ex-flattened
	    (let ([,unit-var ,unit])
	      (verify-linkage-signature-match
	       ',formname
	       '(invoke)
	       (list ,unit-var)
	       '(,ex-exploded)
	       '(,im-explodeds))
	      (unit/sig->unit ,unit-var))
	    ,(if (or (eq? prefix #f)
		     (symbol? prefix))
		 prefix
		 (badsyntax prefix "prefix is not #f or a symbol"))
	    ,@(apply append im-flatteneds)))))
    
    (define define-values/invoke-unit/sig
      (case-lambda 
       [(signame unit prefix . imports)
	(do-define-values/invoke-unit/sig #f signame unit prefix imports)]
       [(signame unit)
	(do-define-values/invoke-unit/sig #f signame unit #f null)]))

    (define global-define-values/invoke-unit/sig
      (case-lambda 
       [(signame unit prefix . imports)
	(do-define-values/invoke-unit/sig #t signame unit prefix imports)]
       [(signame unit)
	(do-define-values/invoke-unit/sig #t signame unit #f null)]))

    (values define-values/invoke-unit
	    define-values/invoke-unit/sig
	    global-define-values/invoke-unit
	    global-define-values/invoke-unit/sig)))

;; ized so it can be loaded in the "R5RS" namespace

(begin-elaboration-time
 (define-values (define-values/invoke-unit
		    define-values/invoke-unit/sig
		    global-define-values/invoke-unit
		    global-define-values/invoke-unit/sig)
   (invoke-unit (require-library "invoker.ss"))))

(define-macro define-values/invoke-unit define-values/invoke-unit)
(define-macro define-values/invoke-unit/sig define-values/invoke-unit/sig)
(define-macro global-define-values/invoke-unit global-define-values/invoke-unit)
(define-macro global-define-values/invoke-unit/sig global-define-values/invoke-unit/sig)
