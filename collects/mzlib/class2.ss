
(module class2 mzscheme

  (require-for-syntax (lib "kerncase.ss" "syntax")
		      (lib "stx.ss" "syntax")
		      "private/classidmap.ss")

  (define-syntax class*/names
    (lambda (stx)
      (syntax-case stx ()
	[(_  (this-id super-id) super-expression (interface-expr ...)
	     defn-or-expr
	     ...)
	 (let ([defn-and-exprs (syntax->list (syntax (defn-or-expr ...)))]
	       [this-id (syntax this-id)]
	       [super-id (syntax super-id)])
	   (unless (identifier? this-id)
	     (raise-syntax-error
	      'class*/names
	      "not an identifier for `this'"
	      stx
	      this-id))
	   (unless (identifier? super-id)
	     (raise-syntax-error
	      'class*/names
	      "not an identifier for `super-init'"
	      stx
	      super-id))

	   ;; ----- Expand definitions -----
	   (let ([defn-and-exprs (map
				  (lambda (defn-or-expr)
				    (local-expand
				     defn-or-expr
				     (append
				      (kernel-form-identifier-list (quote-syntax here))
				      (list 
				       (quote-syntax init)
				       (quote-syntax field)
				       (quote-syntax init-field)
				       (quote-syntax public)
				       (quote-syntax override)
				       (quote-syntax rename)
				       (quote-syntax inherit)
				       this-id
				       super-id))))
				  defn-and-exprs)]
		 [bad (lambda (msg expr)
			(raise-syntax-error 'class* msg stx expr))])

	     ;; ------ Basic syntax checks -----
	     (for-each (lambda (stx)
			 (syntax-case stx (init field init-field public override rename inherit)
			   [(form idp ...)
			    (ormap (lambda (f) (module-identifier=? (syntax form) f))
				   (list (quote-syntax init)
					 (quote-syntax init-field)))
			    (let ([form (syntax-e (syntax form))])
			      (for-each 
			       (lambda (idp)
				 (syntax-case idp ()
				   [id (identifier? (syntax id)) 'ok]
				   [(id expr) (identifier? (syntax id)) 'ok]
				   [else
				    (bad 
				     (format
				      "~a element is not an identifier or identifier-expression pair"
				      form)
				     idp)]))
			       (syntax->list (syntax (idp ...)))))]
			   [(init . rest)
			    (bad "ill-formed init clause" stx)]
			   [(init-field . rest)
			    (bad "ill-formed init-field clause" stx)]
			   [(field idp ...)
			    (for-each (lambda (idp)
					(syntax-case idp ()
					  [(id expr) (identifier? (syntax id)) 'ok]
					  [else
					   (bad 
					    "field element is not an identifier-expression pair"
					    idp)]))
				      (syntax->list (syntax (idp ...))))]
			   [(field . rest)
			    (bad "ill-formed field clause" stx)]
			   [(form idp ...)
			    (ormap (lambda (f) (module-identifier=? (syntax form) f))
				   (list (quote-syntax public)
					 (quote-syntax override)
					 (quote-syntax inherit)))
			    (let ([form (syntax-e (syntax form))])
			      (for-each
			       (lambda (idp)
				 (syntax-case idp ()
				   [id (identifier? (syntax id)) 'ok]
				   [(iid eid) (and (identifier? (syntax id)) (identifier? (syntax eid))) 'ok]
				   [else
				    (bad 
				     (format
				      "~a element is not an identifier or pair of identifiers"
				      form)
				     idp)]))
			       (syntax->list (syntax (idp ...)))))]
			   [(public . rest)
			    (bad "ill-formed public clause" stx)]
			   [(override . rest)
			    (bad "ill-formed override clause" stx)]
			   [(inherit . rest)
			    (bad "ill-formed inherit clause" stx)]
			   [(rename idp ...)
			    (for-each 
			     (lambda (idp)
			       (syntax-case idp ()
				 [(iid eid) (and (identifier? (syntax id)) (identifier? (syntax eid))) 'ok]
				 [else
				  (bad 
				   "rename element is not a pair of identifiers"
				   idp)]))
			     (syntax->list (syntax (idp ...))))]
			   [(rename . rest)
			    (bad "ill-formed rename clause" stx)]
			   [_ 'ok]))
		       defn-and-exprs)

	     ;; ----- Sort body into different categories -----
	     (let ([extract (lambda (kws reverse?)
			      (let loop ([l defn-and-exprs])
				(syntax-case l ()
				  [() null]
				  [((kw . body) . rest)
				   (ormap (lambda (k) (module-identifier=? k (syntax kw))) kws)
				   (if reverse?
				       (loop (stx-cdr l))
				       (cons (stx-car l) (loop (stx-cdr l))))]
				  [_else
				   (if reverse?
				       (cons (stx-car l) (loop (stx-cdr l)))
				       (loop (stx-cdr l)))])))]
		   [flatten (lambda (alone l)
			      (apply append
				     (map (lambda (i)
					    (let ([l (cdr (syntax->list i))])
					      (map (lambda (i)
						     (if (identifier? i)
							 (alone i)
							 (cons (stx-car i)
							       (stx-car (stx-cdr i)))))
						   l)))
					  l)))]
		   [pair (lambda (i) (cons i i))])
	       (let ([inits (flatten values (extract (list (quote-syntax init)
							   (quote-syntax init-field))
						     #f))]
		     [plain-inits (flatten values (extract (list (quote-syntax init))
							   #f))]
		     [plain-fields (flatten values (extract (list (quote-syntax field)) #f))]
		     [plain-init-fields (flatten values (extract (list (quote-syntax init-field)) #f))]
		     [publics (flatten pair (extract (list (quote-syntax public)) #f))]
		     [overrides (flatten pair (extract (list (quote-syntax override)) #f))]
		     [renames (flatten pair (extract (list (quote-syntax rename)) #f))]
		     [inherits (flatten pair (extract (list (quote-syntax inherit)) #f))]
		     [exprs (extract (list (quote-syntax public)
					   (quote-syntax override)
					   (quote-syntax rename)
					   (quote-syntax inherit))
				     #t)])
		 
		 ;; --- Check initialization on inits: ---
		 (let loop ([inits inits])
		   (unless (null? inits)
		     (if (identifier? (car inits))
			 (loop (cdr inits))
			 (let loop ([inits (cdr inits)])
			   (unless (null? inits)
			     (if (identifier? (car inits))
				 (bad "initializer without default follows an initializer with default"
				      (car inits))
				 (loop (cdr inits))))))))
		 
		 ;; ----- Extract method definitions; check that they look like procs -----
		 ;;  Optionally transform them, can expand even if not transforming.
		 (let ([local-public-names (map car (append publics overrides))]
		       [proc-shape (lambda (expr xforms)
				     (define (vars-ok? vars)
				       (or (identifier? vars)
					   (stx-null? vars)
					   (and (stx-pair? vars)
						(identifier? (stx-car vars))
						(vars-ok? (stx-cdr vars)))))
				     (let loop ([stx expr])
				       (syntax-case stx (lambda case-lambda letrec-values let-values)
					 [(lambda vars body1 body ...)
					  (vars-ok? (syntax vars))
					  (if xforms
					      (with-syntax ([this-id this-id]
							    [xforms xforms])
						(syntax/loc stx 
						    (lambda (this-id . vars) 
						      (letrec-syntax xforms
							  body1 body ...))))
					      stx)]
					 [(lambda . _)
					  (bad "ill-formed lambda expression for method" stx)]
					 [(case-lambda [vars body1 body ...] ...)
					  (andmap vars-ok? (syntax->list (syntax (vars ...))))
					  (if xforms
					      (with-syntax ([this-id this-id]
							    [xforms xforms])
						(syntax/loc stx
						    (case-lambda [(this-id . vars) 
								  (letrec-syntax xforms
								      body1 body ...)] ...)))
					      stx)]
					 [(case-lambda . _)
					  (bad "ill-formed case-lambda expression for method" stx)]
					 [(let- ([(id1) expr]) id2)
					  (and (or (module-identifier=? (syntax let-) 
									(quote-syntax let-values))
						   (module-identifier=? (syntax let-) 
									(quote-syntax letrec-values)))
					       (identifier? (syntax id1))
					       (identifier? (syntax id2))
					       (bound-identifier=? (syntax id1) (syntax id2)))
					  (let ([proc (loop (local-expand
							     (syntax expr)
							     (append
							      (kernel-form-identifier-list
							       (quote-syntax here))
							      (list 
							       this-id
							       super-id))))])
					    (syntax/loc stx (let- ([(id1) proc]) id2)))]
					 [_else 
					  (bad "bad form for method definition" stx)])))])
		   ;; Do the extraction:
		   (let-values ([(methods exprs)
				 (let loop ([exprs exprs][ms null][es null])
				   (if (null? exprs)
				       (values (reverse! ms) (reverse! es))
				       (syntax-case (car exprs) (define-values)
					 [(define-values (id ...) expr)
					  ;; ethod defn if any id in the list of publics/overrides
					  (ormap (lambda (id)
						   (unless (identifier? id)
						     (bad "not an identifier for definition" id))
						   (ormap (lambda (i) (bound-identifier=? i id))
							  local-public-names))
						 (syntax->list (syntax (id ...))))
					  (let ([ids (syntax->list (syntax (id ...)))])
					    (unless (null? (cdr ids))
					      (bad "each method variable needs its own definition"
						   (car exprs)))
					    (let ([expr (proc-shape (syntax expr) #f)])
					      (loop (cdr exprs) 
						    (cons (cons (car ids) expr) ms)
						    es)))]
					 [(define-values (id ...) expr)
					  ;; Non-method defn:
					  (andmap identifier? (syntax->list (syntax (id ...))))
					  (loop (cdr exprs) ms (cons (car exprs) es))]
					 [(define-values . _)
					  (bad "ill-formed definition" (car exprs))]
					 [_else
					  (loop (cdr exprs) ms (cons (car exprs) es))])))])
		     
		     ;; ---- Extract all defined names, including field accessors and mutators ---
		     (let ([defined-method-names (map car methods)]
			   [private-field-names (let loop ([l exprs])
						  (if (null? l)
						      null
						      (syntax-case (car l) (define-values)
							[(define-values (id ...) expr)
							 (append (syntax->list (syntax (id ...)))
								 (loop (cdr l)))]
							[_else (loop (cdr l))])))]
			   [field-names (map
					 (lambda (i)
					   (if (identifier? i)
					       i
					       (stx-car i)))
					 (append plain-fields plain-init-fields))]
			   [plain-init-names (map
					      (lambda (i)
						(if (identifier? i)
						    i
						    (stx-car i)))
					      plain-inits)])
		       (let ([field-accessors (map (lambda (i)
						     (datum->syntax-object
						      i
						      (string->symbol
						       (format "get-~a" (syntax-e i)))
						      i))
						   field-names)]
			     [field-mutators (map (lambda (i)
							(datum->syntax-object
							 i
							 (string->symbol
							  (format "set-~a!" (syntax-e i)))
							 i))
						      field-names)])
			 ;; -- Look for duplicates --
			 (let ([dup (check-duplicate-identifier
				     (append defined-method-names
					     private-field-names
					     field-names
					     plain-init-names
					     field-accessors
					     field-mutators
					     (map car inherits)
					     (map car renames)
					     (list this-id super-id)))])
			   (when dup
			     (bad "duplicate declared identifier" dup)))
			 
			 ;; -- Could still have duplicates within public/override --
			 (let ([dup (check-duplicate-identifier local-public-names)])
			   (when dup
			     (bad "duplicate declared identifier" dup)))

			 ;; -- Check that public/override are defined --
			 (let ([ht (make-hash-table)])
			   (for-each
			    (lambda (defined-name)
			      (let ([l (hash-table-get ht (syntax-e defined-name) (lambda () null))])
				(hash-table-put! ht (syntax-e defined-name) (cons defined-name l))))
			    defined-method-names)
			   (for-each
			    (lambda (pubovr-name)
			      (let ([l (hash-table-get ht (syntax-e pubovr-name) (lambda () null))])
				(unless (ormap (lambda (i) (bound-identifier=? i pubovr-name)) l)
				  (bad 
				   "method not defined for public or override declaration"
				   pubovr-name))))
			    local-public-names))
			 
			 ;; ---- Convert expressions ----
			 ;;  Non-method definitions to set!
			 ;;  Initializations args access/set!
			 (let ([exprs (map (lambda (e)
					     (syntax-case e (define-values field)
					       [(define-values (id ...) expr)
						(syntax/loc e (set!-values (id ...) expr))]
					       [(init idp ...)
						(ormap (lambda (it) (module-identifier=? it (syntax init)))
						       (list (quote-syntax init) (quote-syntax init-field)))
						(with-syntax ([(id ...)
							       (map 
								(lambda (idp)
								  (if (identifier? idp)
								      idp
								      (stx-car idp)))
								(syntax->list (syntax (idp ...))))]
							      [(defval ...) 
							       (map (lambda (idp)
								      (if (identifier? idp)
									  (syntax (void))
									  (stx-car (stx-cdr idp))))
								    (syntax->list (syntax (idp ...))))])
						  (syntax/loc e 
						      (begin 
							(set! id (extract-arg init-args 'id
									      (lambda () defval)))
							...)))]
					       [(field idp ...)
						(syntax/loc e (begin 
								(set! . idp)
								...))]
					       [_else e]))
					   exprs)])

			   ;; ---- set up field and method mappings ----
			   (with-syntax ([(rename-orig ...) (map car renames)]
					 [(rename-temp ...) (generate-temporaries (map car renames))]
					 [(method-name ...) local-public-names]
					 [(method-accessor ...) (generate-temporaries
								 (map car
								      (append publics
									      overrides)))]
					 [(field-accessor ...) (generate-temporaries
								(map (lambda (id)
								       (format "get-~a"
									       (syntax-e id)))
								     (append plain-init-names
									     field-names
									     private-field-names)))]
					 [(field-mutator ...) (generate-temporaries
							       (map (lambda (id)
								      (format "set-~a!"
									      (syntax-e id)))
								    (append plain-init-names
									    field-names
									    private-field-names)))]
					 [(all-field ...) (append plain-init-names
								  field-names
								  private-field-names)])
			     (let ([mappings
				    ;; make-XXX-map is supplied by private/classidmap.ss
				    (with-syntax ([this-id this-id])
				      (syntax 
				       ([all-field
					 (make-field-map (quote-syntax this-id)
							 (quote-syntax field-accessor)
							 (quote-syntax field-mutator))]
					...
					[rename-orig
					 (make-rename-map (quote-syntax rename-temp))]
					...
					[method-name
					 (make-method-map (quote-syntax this-id)
							  (quote-syntax method-accessor))]
					...)))])

			       (let ([find-method (lambda (name)
						    (ormap (lambda (m)
							     (and (bound-identifier=? (car m) name)
								  (proc-shape (cdr m) mappings)))
							   methods))])
				 
				 ;; ---- build final result ----
				 (with-syntax ([public-names (map cdr publics)]
					       [override-names (map cdr overrides)]
					       [rename-names (map cdr renames)]
					       [inherit-names (map cdr inherits)]
					       [num-fields (datum->syntax-object
							    (quote-syntax here)
							    (+ (length private-field-names)
							       (length inits)
							       (length plain-fields)))]
					       [field-names (map (lambda (i)
								   (if (identifier? i)
								       i
								       (car i)))
								 (append
								  plain-fields
								  plain-init-fields))]
					       [public-methods (map find-method (map car publics))]
					       [override-methods (map find-method (map car overrides))]
					       [mappings mappings]
					       [exprs exprs]
					       [this-id this-id]
					       [super-id super-id])

				   (syntax 
				    (let ([superclass super-expression]
					  [interfaces (list interface-expr ...)])
				      (compose-class 
				       name superclass interfaces
				       ;; Field count:
				       num-fields
				       ;; Public field names:
				       (quote field-names)
				       ;; Method names:
				       (quote public-names)
				       (quote override-names)
				       (quote rename-names)
				       (quote inherit-names)
				       ;; Methods (when given needed super-methods, etc.):
				       (lambda (rename-temp ...
							    method-accessor ...
							    field-accessor ...
							    field-mutator ...)
					 (list
					  (list . public-methods)
					  (list . override-methods)
					  ;; Initialization
					  (lambda (this-id super-id init-args)
					    (letrec-syntax mappings
						. exprs))))))))))))))))))))])))
  
  (provide class*/names))


