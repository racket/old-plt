
;; Unit system

(module unit mzscheme
  (require-for-syntax (lib "kerncase.ss" "syntax")
		      (lib "stx.ss" "syntax")
		      "private/unitidmap.ss")

  (define undefined (letrec ([x x]) x))

  (define-struct unit (num-imports exports go))
  (define-struct (exn:unit struct:exn) ())

  (define-syntax unit
    (lambda (stx)
      (syntax-case stx (import export)
	[(_ (import ivar ...)
	    (export evar ...)
	    defn&expr ...)
	 (let ([check-id (lambda (v)
			   (unless (identifier? v)
			     (raise-syntax-error
			      'unit
			      "import is not an identifier"
			      stx
			      v)))]
	       [check-renamed-id 
		(lambda (v)
		  (syntax-case v ()
		    [id (identifier? (syntax id)) 'ok]
		    [(lid eid) (and (identifier? (syntax lid))
				    (identifier? (syntax eid))) 'ok]
		    [else (raise-syntax-error
			   'unit
			   "export is not an identifier or renamed identifier"
			   stx
			   v)]))]
	       [ivars (syntax->list (syntax (ivar ...)))]
	       [evars (syntax->list (syntax (evar ...)))])
	   (for-each check-id ivars)
	   (for-each check-renamed-id evars)
	   
	   ;; Get import/export declared names:
	   (let* ([exported-names
		   (map (lambda (v)
			  (syntax-case v ()
			    [(lid eid) (syntax lid)]
			    [id (syntax id)]))
			evars)]
		  [extnames (map (lambda (v)
				   (syntax-case v ()
				     [(lid eid) (syntax eid)]
				     [id (syntax id)]))
				 evars)]
		  [imported-names ivars]
		  [declared-names (append imported-names exported-names)])
	     ;; Check that all exports are distinct (as symbols)
	     (let ([ht (make-hash-table)])
	       (for-each (lambda (name)
			   (when (hash-table-get ht (syntax-e name) (lambda () #f))
			     (raise-syntax-error
			      'unit
			      "duplicate export"
			      stx
			      name))
			   (hash-table-put! ht (syntax-e name) #t))
			 extnames))

	     ;; Expand all body expressions
	     ;; so that all definitions are exposed.
	     (letrec ([expand-all
		       (lambda (defns&exprs)
			 (let ([expanded
				(map
				 (lambda (defn-or-expr)
				   (local-expand
				    defn-or-expr
				    (append
				     (kernel-form-identifier-list (quote-syntax here))
				     declared-names)))
				 defns&exprs)])
			   (apply
			    append
			    (map
			     (lambda (defn-or-expr)
			       (syntax-case defn-or-expr (begin)
				 [(begin . l)
				  (let ([l (syntax->list (syntax l))])
				    (unless l
				      (raise-syntax-error
				       'begin
				       "bad syntax (illegal use of `.')"
				       stx
				       defn-or-expr))
				    (expand-all l))]
				 [else (list defn-or-expr)]))
			     expanded))))])
	       (let ([all-expanded (expand-all (syntax->list (syntax (defn&expr ...))))])
		 ;; Get all the defined names
		 (let ([all-defined-names
			(apply
			 append
			 (map
			  (lambda (defn-or-expr)
			    (syntax-case defn-or-expr (define-values define-syntax)
			      [(define-values (id ...) expr)
			       (let ([l (syntax->list (syntax (id ...)))])
				 (for-each (lambda (i)
					     (unless (identifier? i)
					       (raise-syntax-error
						'unit
						"not an identifier in definition"
						stx
						i)))
					   l)
				 l)]
			      [(define-values . l)
			       (raise-syntax-error
				'unit
				"bad definition form"
				stx
				defn-or-expr)]
			      [(define-syntax . l)
			       (raise-syntax-error
				'unit
				"misplaced syntax definition"
				stx
				defn-or-expr)]
			      [else null]))
			  all-expanded))])
		   ;; Check that all defined names are distinct:
		   (let ([name (check-duplicate-identifier
				(append imported-names all-defined-names))])
		     (when name
		       (raise-syntax-error 
			'unit
			"variable imported and/or defined twice"
			stx
			name)))
		   ;; Check that all exported names are defined:
		   (let ([ht (make-hash-table)])
		     (for-each
		      (lambda (name)
			(let ([l (hash-table-get ht (syntax-e name) (lambda () null))])
			  (hash-table-put! ht (syntax-e name) (cons name l))))
		      all-defined-names)
		     (for-each 
		      (lambda (n)
			(let ([v (hash-table-get ht (syntax-e n) (lambda () null))])
			  (unless (ormap (lambda (i) (bound-identifier=? i n)) v)
			    (raise-syntax-error
			     'unit
			     "exported variable is not defined"
			     stx
			     n))))
		      exported-names))

		   ;; Compute defined but not exported:
		   (let ([ht (make-hash-table)])
		     (for-each
		      (lambda (name)
			(let ([l (hash-table-get ht (syntax-e name) (lambda () null))])
			  (hash-table-put! ht (syntax-e name) (cons name l))))
		      exported-names)
		     (let ([internal-names
			    (let loop ([l all-defined-names])
			      (cond
			       [(null? l) null]
			       [(let ([v (hash-table-get ht (syntax-e (car l)) (lambda () null))])
				  (ormap (lambda (i) (bound-identifier=? i (car l))) v))
				(loop (cdr l))]
			       [else (cons (car l) (loop (cdr l)))]))])
		       ;; Generate names for import/export boxes, etc:
		       (with-syntax ([(iloc ...) (generate-temporaries (syntax (ivar ...)))]
				     [(eloc ...) (generate-temporaries evars)]
				     [(extname ...) extnames]
				     [(expname ...) exported-names]
				     [(intname ...) internal-names])
			 ;; Change all definitions to set!s. Convert evars to set-box!,
			 ;; because set! on exported variables is not allowed.
			 (with-syntax ([(defn&expr ...) 
					(let ([elocs (syntax->list (syntax (eloc ...)))])
					  (map (lambda (defn-or-expr)
						 (syntax-case defn-or-expr (define-values)
						   [(define-values ids expr)
						    (let* ([ids (syntax->list (syntax ids))])
						      (if (null? ids)
							  (syntax/loc defn-or-expr (set!-values ids expr))
							  (let ([do-one
								 (lambda (id tmp name)
								   (let loop ([evars exported-names]
									      [elocs elocs])
								     (cond
								      [(null? evars)
								       ;; not an exported id
								       (with-syntax ([id id][tmp tmp])
									 (syntax/loc
									  defn-or-expr
									  (set! id tmp)))]
								      [(bound-identifier=? (car evars) id)
								       ;; set! exported id:
								       (with-syntax 
									   ([loc (car elocs)]
									    [tmp 
									     (if name
										 (with-syntax 
										     ([tmp tmp]
										      [name name])
										   (syntax 
										    (let ([name tmp])
										      name)))
										 tmp)])
									 (syntax/loc
									  defn-or-expr
									  (set-box! loc tmp)))]
								      [else (loop (cdr evars) 
										  (cdr elocs))])))])
							    (if (null? (cdr ids))
								(do-one (car ids) (syntax expr) (car ids))
								(let ([tmps (generate-temporaries ids)])
								  (with-syntax ([(tmp ...) tmps]
										[(set ...)
										 (map (lambda (id tmp)
											(do-one id tmp #f))
										      ids tmps)])
								    (syntax/loc
								     defn-or-expr
								     (let-values ([(tmp ...) expr])
								       set ...))))))))]
						   [else defn-or-expr]))
					       all-expanded))])
			   ;; Build up set! redirection chain:
			   (with-syntax ([redirections
					  (map
					   (lambda (varloc)
					     (with-syntax ([(var loc) varloc])
					       (syntax
						[var (make-id-mapper (quote-syntax (unbox loc)))])))
					   (syntax->list 
					    (syntax ((ivar iloc) ... (expname eloc) ...))))]
					 [num-imports (datum->syntax 
						       (length (syntax->list (syntax (iloc ...))))
						       #f (quote-syntax here))])
			     (syntax/loc
			      stx
			      (make-unit
			       num-imports
			       (list (quote extname) ...)
			       (lambda ()
				 (let ([eloc (box undefined)] ...)
				   (list (vector eloc ...)
					 (lambda (iloc ...)
					   (let ([intname undefined] ...)
					     (letrec-syntax redirections
						 (void) ; in case the body would be empty
					       defn&expr ...))))))))))))))))))])))
  
  (define (check-expected-interface tag unit num-imports exports)
    (unless (unit? unit)
      (raise
       (make-exn:unit
	(format "compound-unit: result of expression for tag ~s not a unit: ~e" tag unit)
	(current-continuation-marks))))
    (unless (= num-imports (unit-num-imports unit))
      (raise
       (make-exn:unit
	(format "compound-unit: unit for tag ~s expects ~a imports, given ~a" 
		tag
		(unit-num-imports unit)
		num-imports)
	(current-continuation-marks))))
    (list->vector
     (map (lambda (ex)
	    (let loop ([l (unit-exports unit)][i 0])
	      (cond
	       [(null? l)
		(raise
		 (make-exn:unit
		  (format "compount-unit: unit for tag ~s has no ~s export" 
			  tag ex)
		  (current-continuation-marks)))]
	       [(eq? (car l) ex)
		i]
	       [else (loop (cdr l) (add1 i))])))
	  exports)))

  (define-syntax compound-unit
    (lambda (stx)
      (syntax-case stx (import export link)
	[(_ (import ivar ...)
	    (link [tag (unit-expr linkage ...)] ...)
	    (export exportage ...))
	 (let ([check-id (lambda (v)
			   (unless (identifier? v)
			     (raise-syntax-error
			      'compound-unit
			      "import is not an identifier"
			      stx
			      v)))]
	       [check-tag (lambda (v)
			   (unless (identifier? v)
			     (raise-syntax-error
			      'compound-unit
			      "tag is not an identifier"
			      stx
			      v)))]
	       [check-linkage (lambda (v)
				(syntax-case v ()
				  [id (identifier? (syntax id)) #t]
				  [(tag id ...)
				   (for-each (lambda (v)
					       (unless (identifier? v)
						 (raise-syntax-error
						  'compound-unit
						  "non-identifier in linkage"
						  stx
						  v)))
					     (syntax->list v))]
				  [else
				   (raise-syntax-error
				    'compound-unit
				    "ill-formed linkage"
				    stx
				    v)]))]
	       [check-exportage (lambda (v)
				  (syntax-case v ()
				    [(tag ex ...)
				     (begin
				       (unless (identifier? (syntax tag))
					 (raise-syntax-error
					  'compound-unit
					  "export tag is not an identifier"
					  stx
					  (syntax tag)))
				       (for-each 
					(lambda (e)
					  (syntax-case e ()
					    [id (identifier? (syntax id)) #t]
					    [(iid eid)
					     (begin
					       (unless (identifier? (syntax iid))
						 (raise-syntax-error
						  'compound-unit
						  "export internal name is not an identifier"
						  stx
						  (syntax iid)))
					       (unless (identifier? (syntax eid))
						 (raise-syntax-error
						  'compound-unit
						  "export internal name is not an identifier"
						  stx
						  (syntax eid))))]
					    [else
					     (raise-syntax-error
					      'compound-unit
					      (format "ill-formed export with tag ~a" 
						      (syntax-e (syntax tag)))
					      stx
					      e)]))
					(syntax->list (syntax (ex ...)))))]
				    [else
				     (raise-syntax-error
				      'compound-unit
				      "ill-formed export"
				      stx
				      v)]))]
	       [imports (syntax->list (syntax (ivar ...)))]
	       [tags (syntax->list (syntax (tag ...)))]
	       [linkages (map syntax->list (syntax->list (syntax ((linkage ...) ...))))]
	       [exports (syntax->list (syntax (exportage ...)))])
	   ;; Syntax checks:
	   (for-each check-id imports)
	   (for-each check-tag tags)
	   (for-each (lambda (l) (for-each check-linkage l)) linkages)
	   (for-each check-exportage exports)
	   ;; Check for duplicate imports
	   (let ([dup (check-duplicate-identifier imports)])
	     (when dup
	       (raise-syntax-error
		'compound-unit
		"duplicate import"
		stx
		dup)))
	   ;; Check for duplicate tags
	   (let ([dup (check-duplicate-identifier tags)])
	     (when dup
	       (raise-syntax-error
		'compound-unit
		"duplicate tag"
		stx
		dup)))
	   ;; Check referenced imports and tags
	   (let ([check-linkage-refs (lambda (v)
				       (syntax-case v ()
					 [(tag . exs)
					  (unless (ormap (lambda (t)
							   (bound-identifier=? t (syntax tag)))
							 tags)
					    (raise-syntax-error
					     'compound-unit
					     "linkage tag is not bound"
					     stx
					     (syntax tag)))]
					 [id (unless (ormap (lambda (i)
							      (bound-identifier=? i (syntax id)))
							    imports)
					       (raise-syntax-error
						'compound-unit
						"no imported identified for linkage"
						stx
						(syntax id)))]))]
		 [check-export-refs (lambda (v)
				      (syntax-case v ()
					[(tag . r)
					 (unless (ormap (lambda (t)
							  (bound-identifier=? t (syntax tag)))
							tags)
					   (raise-syntax-error
					    'compound-unit
					    "export tag is not bound"
					    stx
					    (syntax tag)))]))])
	     (for-each (lambda (l) (for-each check-linkage-refs l))
		       linkages)
	     (for-each check-export-refs exports)
	     ;; Get all export names, and check for duplicates
	     (let ([export-names
		    (apply
		     append
		     (map
		      (lambda (v)
			(syntax-case v ()
			  [(tag . exs)
			   (map
			    (lambda (e)
			      (syntax-case e ()
				[(iid eid) (syntax eid)]
				[id e]))
			    (syntax->list (syntax exs)))]))
		      exports))])
	       (let ([dup (check-duplicate-identifier export-names)])
		 (when dup
		   (raise-syntax-error
		    'compound-unit
		    "duplicate export"
		    stx
		    dup)))

	       (let ([constituents (generate-temporaries tags)]
		     [unit-export-positionss (generate-temporaries tags)]
		     [unit-setups (generate-temporaries tags)]
		     [unit-export-lists
		      ;; For each tag, get all expected exports
		      (let* ([hts (map (lambda (x) (make-hash-table)) tags)]
			     [get-add-name 
			      (lambda (tag)
				(ormap (lambda (t ht)
					 (and (bound-identifier=? t tag)
					      (lambda (name)
						(hash-table-put! ht (syntax-e name) name))))
				       tags hts))])
			;; Walk though linkages
			(for-each
			 (lambda (linkage-list)
			   (for-each 
			    (lambda (linkage)
			      (syntax-case linkage ()
				[(tag . ids)
				 (let ([add-name (get-add-name (syntax tag))])
				   (for-each add-name (syntax->list (syntax ids))))]
				[else (void)]))
			    linkage-list))
			 linkages)
			;; Walk through exports
			(for-each
			 (lambda (v)
			   (syntax-case v ()
			     [(tag . exs)
			      (let ([add-name (get-add-name (syntax tag))])
				(for-each 
				 (lambda (e)
				   (syntax-case e ()
				     [(iid eid) (add-name (syntax iid))]
				     [id (add-name (syntax id))]))
				 (syntax->list (syntax exs))))]))
			 exports)
			;; Extract names from hash tables
			(map (lambda (ht)
			       (hash-table-map ht (lambda (k v) v)))
			     hts))])
		 ;; Map exports to imports and indices based on expected unit exports
		 (let ([map-tag (lambda (t l)
				  (let loop ([tags tags][l l])
				    (if (bound-identifier=? (car tags) t)
					(car l)
					(loop (cdr tags) (cdr l)))))]
		       [unit-export-hts (map (lambda (export-list)
					       (let ([ht (make-hash-table)])
						 (let loop ([l export-list][p 0])
						   (unless (null? l)
						     (hash-table-put! ht (syntax-e (car l)) p)
						     (loop (cdr l) (add1 p))))
						 ht))
					     unit-export-lists)])
		   (let ([make-mapping
			  (lambda (v)
			    (syntax-case v ()
			      [(tag . exs)
			       (let ([ex-poss (map-tag (syntax tag)
						       unit-export-positionss)]
				     [setup (map-tag (syntax tag)
						     unit-setups)]
				     [ht (map-tag (syntax tag)
						  unit-export-hts)])
				 (map
				  (lambda (e)
				    (let ([pos (hash-table-get
						ht
						(syntax-e
						 (syntax-case e ()
						   [(iid eid) (syntax iid)]
						   [id e])))])
				      (with-syntax ([ex-poss ex-poss]
						    [setup setup]
						    [pos (datum->syntax 
							  pos
							  #f
							  (quote-syntax here))])
					(syntax 
					 (vector-ref (car setup)
						     (vector-ref ex-poss pos))))))
				  (syntax->list (syntax exs))))]
			      [import (list v)]))])
		     (let ([export-mapping (apply append (map make-mapping exports))]
			   [import-mappings (map (lambda (linkage-list)
						   (apply append 
							  (map make-mapping linkage-list)))
						 linkages)])
		       (with-syntax ([(constituent ...) constituents]
				     [(unit-export-positions ...) unit-export-positionss]
				     [(unit-setup ...) unit-setups]
				     [(unit-export-list ...) unit-export-lists]
				     [(import-mapping ...) import-mappings]
				     [(unit-import-count ...) 
				      (map (lambda (l) 
					     (datum->syntax (apply
							     +
							     (map (lambda (v)
								    (if (identifier? v)
									1
									(length (cdr (syntax->list v)))))
								  l))
							    #f
							    (quote-syntax here)))
					   linkages)]
				     [num-imports (datum->syntax (length imports)
								 #f (quote-syntax here))]
				     [export-names export-names]
				     [export-mapping export-mapping])
			 (syntax/loc
			  stx
			  (let ([constituent unit-expr]
				...)
			    (let ([unit-export-positions
				   (check-expected-interface 
                                    'tag
				    constituent
				    unit-import-count
				    'unit-export-list)]
				  ...)
			      (make-unit
			       num-imports
			       (quote export-names)
			       (lambda ()
				 (let ([unit-setup ((unit-go constituent))] ...)
				   (list (vector . export-mapping)
					 (lambda (ivar ...)
					   (void) ;; in case there are no units
					   ((list-ref unit-setup 1) . import-mapping)
					   ...))))))))))))))))])))

  (define (check-unit u n)
    (unless (unit? u)
      (raise
       (make-exn:unit
	(format "invoke-unit: result of unit expression was not a unit: ~e" u)
	(current-continuation-marks))))
    (unless (= (unit-num-imports u) n)
      (raise
       (make-exn:unit
	(format "invoke-unit: expected a unit with ~a imports, given one with ~a imports"
		n (unit-num-imports u))
	(current-continuation-marks)))))

  (define-syntax invoke-unit
    (lambda (stx)
      (syntax-case stx (import export)
	[(_ unit-expr expr ...)
	 (let ([exprs (syntax (expr ...))])
	   (with-syntax ([(bx ...) (generate-temporaries (syntax (expr ...)))]
			 [num (datum->syntax (length (syntax->list exprs)) 
					     #f 
					     (quote-syntax here))])
	     (syntax/loc
	      stx
	      (let ([u unit-expr])
		(check-unit u num)
		(let ([bx (box expr)] ...)
		  ((list-ref ((unit-go u)) 1)
		   bx ...))))))])))

  (define-syntax do-define-values/invoke-unit
    (lambda (stx)
      (syntax-case stx ()
	[(_ global? exports unite prefix imports orig)
	 (let* ([badsyntax (lambda (s why)
			     (raise-syntax-error
			      (if (syntax-e (syntax global?))
				  'global-define-values/invoke-unit
				  'define-values/invoke-unit)
			      (format "bad syntax (~a)" why)
			      (syntax orig)
			      s))]
		[symcheck (lambda (s)
			    (or (identifier? s)
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
				     (syntax->list (syntax exports))))
			      (syntax exports))]
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
		   (syntax (define-values (tagged-export ...) invoke-unit))))))])))
    
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
  
  (provide unit compound-unit invoke-unit unit?
	  exn:unit? struct:exn:unit make-exn:unit

	  define-values/invoke-unit
	  global-define-values/invoke-unit))
