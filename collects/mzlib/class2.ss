
(module class2 mzscheme

  (require-for-syntax (lib "kerncase.ss" "syntax")
		      (lib "stx.ss" "syntax")
		      "private/classidmap.ss")

  (define insp (make-inspector)) (print-struct #t)

  ;;--------------------------------------------------------------------
  ;;  main class macro                                                 
  ;;--------------------------------------------------------------------

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
		       ;; -- Look for duplicates --
		       (let ([dup (check-duplicate-identifier
				   (append defined-method-names
					   private-field-names
					   field-names
					   plain-init-names
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
					     [(-init idp ...)
					      (ormap (lambda (it) (module-identifier=? it (syntax -init)))
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
									(syntax #f)
									(with-syntax ([defexp (stx-car (stx-cdr idp))])
									  (syntax (lambda () defexp)))))
								  (syntax->list (syntax (idp ...))))])
						(syntax/loc e 
						  (begin 
						    (set! id (extract-arg 'id init-args argorder defval))
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
				       [(method-name ...) (append local-public-names
								  (map car inherits))]
				       [(method-accessor ...) (generate-temporaries
							       (map car
								    (append publics
									    overrides
									    inherits)))]
				       [(field-accessor ...) (generate-temporaries
							      (map (lambda (id)
								     (format "get-~a"
									     (syntax-e id)))
								   (append field-names
									   private-field-names)))]
				       [(field-mutator ...) (generate-temporaries
							     (map (lambda (id)
								    (format "set-~a!"
									    (syntax-e id)))
								  (append field-names
									  private-field-names)))]
				       [(all-field ...) (append field-names
								private-field-names)]
				       [(plain-init-name ...) (map (lambda (i)
								     (if (identifier? i)
									 i
									 (car i)))
								   plain-inits)])
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
				       (make-rename-map (quote-syntax rename-temp)
							(quote-syntax this-id))]
				      ...
				      [method-name
				       (make-method-map (quote-syntax this-id)
							(quote-syntax method-accessor))]
				      ...)))]
				 [extra-init-mappings
				  (with-syntax ([this-id this-id])
				    (syntax 
				     ([plain-init-name init-error-map]
				      ...)))])

			     (let ([find-method (let ([all-mappings
						       (with-syntax ([(mapping ...) mappings]
								     [extra-init-mappings extra-init-mappings])
							 (syntax (mapping ... . extra-init-mappings)))])
						  (lambda (name)
						    (ormap (lambda (m)
							     (and (bound-identifier=? (car m) name)
								  (proc-shape (cdr m) all-mappings)))
							   methods)))])
			       
			       ;; ---- build final result ----
			       (with-syntax ([public-names (map cdr publics)]
					     [override-names (map cdr overrides)]
					     [rename-names (map cdr renames)]
					     [inherit-names (map cdr inherits)]
					     [num-fields (datum->syntax-object
							  (quote-syntax here)
							  (+ (length private-field-names)
							     (length plain-init-fields)
							     (length plain-fields)))]
					     [field-names (map (lambda (i)
								 (if (identifier? i)
								     i
								     (car i)))
							       (append
								plain-fields
								plain-init-fields))]
					     [init-names (map (lambda (i)
								(if (identifier? i)
								    i
								    (car i)))
							      inits)]
					     [public-methods (map find-method (map car publics))]
					     [override-methods (map find-method (map car overrides))]
					     [mappings mappings]
					     [exprs exprs]
					     [this-id this-id]
					     [super-id super-id]
					     [name (syntax-local-name)])

				 (syntax 
				  (let ([superclass super-expression]
					[interfaces (list interface-expr ...)]
					[argorder 'init-names])
				    (compose-class 
				     'name superclass interfaces
				     ;; Field count:
				     num-fields
				     ;; Public field names:
				     (quote field-names)
				     ;; Method names:
				     (quote rename-names)
				     (quote public-names)
				     (quote override-names)
				     (quote inherit-names)
				     ;; Methods (when given needed super-methods, etc.):
				     (lambda (field-accessor ...
							     field-mutator ...
							     rename-temp ...
							     method-accessor ...) ; public, override, inherit
				       (values
					(list . public-methods)
					(list . override-methods)
					;; Initialization
					(lambda (this-id super-id init-args)
					  (let ([plain-init-name undefined]
						...)
					    (letrec-syntax mappings
					      . exprs))))))))))))))))))))])))

  ;;--------------------------------------------------------------------
  ;;  class implementation
  ;;--------------------------------------------------------------------

  (define-struct class (name
			pos supers     ; pos is subclass depth, supers is vector
			interface      ; self interface

			method-width   ; total number of methods
			method-ht      ; maps public names to vector positions
			method-ids     ; ordered list of public method names

			methods        ; vector of methods

			field-width    ; total number of fields
			field-ht       ; maps public field names to (cons accessor mutator)
			field-ids      ; list of public field names

			struct:object  ; structure type for instances
			object?        ; predicate
			make-object    ; constructor

			init           ; initializer
			)
                        insp)

  (define (compose-class name               ; symbol
			 super              ; class
			 interfaces         ; list of interfaces

			 num-fields         ; total fields (public & private)
			 public-field-names ; list of symbols (shorter than num-fields)
			 
			 rename-names       ; list of symbols
			 public-names
			 override-names
			 inherit-names
			 
			 make-methods)      ; takes field and method accessors
    ;; -- Check superclass --
    (unless (class? super)
      (obj-error 'class* "superclass expression returned a non-class: ~a~a" 
		 super
		 (for-class name)))
    ;; -- Create new class's name --
    (let ([name (or name
		    (let ([s (class-name super)])
		      (and s 
			   (not (eq? super object%))
			   (if (symbol? s)
			       (format "derived-from-~a" s)
			       s))))])
      ;; -- Check interfaces ---
      '(for-each
       (lambda (intf)
	 (unless (interface? intf)
	   (obj-error 'class*/names "interface expression returned a non-interface: ~a~a" 
		      intf
		      (for-class name))))
       interfaces)

      ;; -- Match method and field names to indices --
      (let ([method-ht (make-hash-table)]
	    [field-ht (make-hash-table)]
	    [super-method-ids (class-method-ids super)]
	    [super-field-ids (class-field-ids super)]
	    [super-field-ht (class-field-ht super)])

	;; Put superclass ids in tables, with pos
	(let loop ([ids super-method-ids][p 0])
	  (unless (null? ids)
	    (hash-table-put! method-ht (car ids) p)
	    (loop (cdr ids) (add1 p))))
	(let loop ([ids super-field-ids])
	  (unless (null? ids)
	    (hash-table-put! field-ht (car ids) (hash-table-get super-field-ht (car ids)))
	    (loop (cdr ids))))

	;; Put new ids in table, with pos (replace field pos with accessor later)
	(let loop ([ids public-names][p (class-method-width super)])
	  (unless (null? ids)
	    (when (hash-table-get method-ht (car ids) (lambda () #f))
	      (obj-error 'class*/names "superclass already contains method: ~a~a" 
			 (car ids)
			 (for-class name)))
	    (hash-table-put! method-ht (car ids) p)
	    (loop (cdr ids) (add1 p))))
	(let loop ([ids public-field-names][p (class-field-width super)])
	  (unless (null? ids)
	    (when (hash-table-get field-ht (car ids) (lambda () #f))
	      (obj-error 'class*/names "superclass already contains field: ~a~a" 
			 (car ids)
			 (for-class name)))
	    (hash-table-put! field-ht (car ids) p)
	    (loop (cdr ids) (add1 p))))

	;; Check that superclass has expected methods, and get indices
	(let ([get-indices
	       (lambda (ids)
		 (map
		  (lambda (id)
		    (hash-table-get 
		     method-ht id
		     (lambda ()
		       (obj-error 'class*/names 
				  "superclass does not provide an expected method: ~a~a" 
				  id
				  (for-class name)))))
		  ids))]
	      [method-width (+ (class-method-width super) (length public-names))]
	      [field-width (+ (class-field-width super) num-fields)])
	  (let ([rename-indices (get-indices rename-names)]
		[inherit-indices (get-indices inherit-names)]
		[replace-indices (get-indices override-names)]
		[new-indices (get-indices public-names)])

	    ;; -- Check that all interfaces are satisfied --
	    '(for-each
	     (lambda (intf)
	       (for-each
		(lambda (var)
		  (unless (hash-table-get method-ht var (lambda () #f))
		    (obj-error 'class*/names 
			       "interface-required method missing: ~a~a~a" 
			       var
			       (for-class name)
			       (for-intf (interface-name intf)))))
		(interface-public-ids intf)))
	     interfaces)
	    '(let ([c (get-implement-requirement interfaces 'class*/names (for-class name))])
	      (when (and c (not (subclass? super c)))
		(obj-error 'class*/names 
			   "interface-required implementation not satisfied~a~a"
			   (for-class name)
			   (let ([r (class-name c)])
			     (if r
				 (format " required class: ~a" r)
				 "")))))

	    ;; ---- Make the class and its interface ----
	    (let* ([class-make (if name
				   (make-naming-constructor 
				    struct:class 
				    (string->symbol (format "class:~a" name)))
				   make-class)]
		   [interface-make (if name
				       (make-naming-constructor 
					struct:interface
					(string->symbol (format "interface:~a" name)))
				       make-interface)]
		   [method-names (append super-method-ids public-names)]
		   [field-names (append super-field-ids public-field-names)]
		   [super-interfaces (cons (class-interface super) interfaces)]
		   [i (interface-make name super-interfaces method-names #f)]
		   [methods (make-vector method-width)]
		   [c (class-make name
				  (add1 (class-pos super))
				  (list->vector (append (vector->list (class-supers super)) (list #f)))
				  i
				  method-width method-ht method-names
				  methods
				  field-width field-ht field-names
				  'struct:object 'object? 'make-object
				  'init)]
		   [obj-name (if name
				 (string->symbol (format "object:~a" name))
				 'object)])

	      ;; --- Mane the new object struct ---
	      (let*-values ([(struct:object object-make object? object-field-ref object-field-set!)
			     (make-struct-type obj-name
					       (class-struct:object super)
					       0 ;; No init fields
					       ;; Fields for new slots:
					       num-fields undefined
					       null
					       insp)]
			    [(struct:tagged-object tagged-object-make tagged-object? -ref -set!)
			     (make-struct-type obj-name
					       struct:object
					       0 0 #f
					       ;; Map object property to class:
					       (list (cons prop:object c))
					       insp)])
		(set-class-struct:object! c struct:object)
		(set-class-object?! c object?)
		(set-class-make-object! c tagged-object-make)

		;; --- Build field accessors and mutators ---
		(let ([accessors (let loop ([n num-fields][l null])
				   (if (zero? n)
				       l
				       (loop (sub1 n)
					     (cons (make-struct-field-accessor object-field-ref (sub1 n))
						   l))))]
		      [mutators  (let loop ([n num-fields][l null])
				   (if (zero? n)
				       l
				       (loop (sub1 n)
					     (cons (make-struct-field-mutator object-field-set! (sub1 n))
						   l))))])
		  ;; -- Reset field table to register accessors and mutators --
		  ;;  There are more accessors and mutators than public fields...
		  (let loop ([ids public-field-names][accessors accessors][mutators mutators])
		    (unless (null? ids)
		      (hash-table-put! field-ht (car ids) (cons (car accessors) (car mutators)))
		      (loop (cdr ids) (cdr accessors) (cdr mutators))))

		  ;; -- Extract superclass methods ---
		  (let ([renames (map (lambda (index)
					(vector-ref (class-methods super) index))
				      rename-indices)])
		    ;; -- Create method accessors --
		    (let ([method-accessors (map (lambda (index)
						   (lambda (obj)
						     (vector-ref (class-methods (object-ref obj)) index)))
						 (append new-indices
							 replace-indices
							 inherit-indices))])
		      
		      ;; -- Get new methods and initializers --
		      (let-values ([(new-methods override-methods init)
				    (apply make-methods
					   (append accessors
						   mutators
						   renames
						   method-accessors))])
			;; -- Fill in method tables --
			;;  First copy old methods
			(hash-table-for-each
			 (class-method-ht super)
			 (lambda (name index)
			   (vector-set! methods index (vector-ref (class-methods super) index))))
			;; Add new methods:
			(for-each (lambda (index method)
				    (vector-set! methods index method))
				  new-indices
				  new-methods)
			;; Override old methods:
			(for-each (lambda (index method)
				    (vector-set! methods index method))
				  replace-indices
				  override-methods)

			;; --- Install initializer into class ---
			(set-class-init! c init)
				    
			;; -- result is the class ---
			c)))))))))))

  (define-values (prop:object object? object-ref) (make-struct-type-property 'object))

  ;;--------------------------------------------------------------------
  ;;  interfaces
  ;;--------------------------------------------------------------------
  
  (define-struct interface (name supers public-ids class) insp)

  ;;--------------------------------------------------------------------
  ;;  object%
  ;;--------------------------------------------------------------------
  
  (define object% (make-class 'object%
			      0 (vector #f) 
			      'object<%>

			      0 (make-hash-table) null
			      (vector)
			      
			      0 (make-hash-table) null
			      
			      'struct:object object? 'make-object
			      (lambda (this super-init args)
				;; FIXME: check that args is empty
				(void))))
  (vector-set! (class-supers object%) 0 object%)
  (let*-values ([(struct:obj make-obj obj? -get -set!)
		 (make-struct-type 'object #f 0 0 #f null insp)]
		[(struct:tagged-obj make-tagged-obj tagged-obj? -get -set!)
		 (make-struct-type 'object struct:obj 0 0 #f (list (cons prop:object object%)) insp)])
    (set-class-struct:object! object% struct:obj)
    (set-class-object?! object% obj?)
    (set-class-make-object! object% make-tagged-obj))

  ;;--------------------------------------------------------------------
  ;;  instantiation
  ;;--------------------------------------------------------------------
  
  (define (make-object class . args)
    (unless (class? class)
      (raise-type-error 'make-object "class" class))
    (let ([o ((class-make-object class))])
      ;; Initialize it:
      ((class-init class) o 'super-init args)
      o))

  (define (extract-arg name arguments by-pos-names default)
    (let loop ([arguments arguments][by-pos-names by-pos-names])
      (cond
       [(null? arguments) (if default
			      (default)
			      (obj-error "make-object" "no argument for required init variable: ~a" name))]
       [(eq? (car by-pos-names) name) (car arguments)]
       [else (loop (cdr arguments) (cdr by-pos-names))])))
	  

  ;;--------------------------------------------------------------------
  ;;  misc utils
  ;;--------------------------------------------------------------------
  
  (define undefined (letrec ([x x]) x))
  
  (define (make-naming-constructor type name)
    (let-values ([(struct: make- ? -accessor -mutator)
		  (make-struct-type name type 0 0 #f null insp)])
      make-))

  (define-struct (exn:object struct:exn) () insp)

  (define (obj-error where . msg)
    (raise
     (make-exn:object
      (string-append
       (format "~a: " where)
       (apply format msg))
      (current-continuation-marks))))

  (define (for-class name)
    (if name (format " for class: ~a" name) ""))
  (define (for-intf name)
    (if name (format " for interface: ~a" name) ""))


  (provide class*/names 
	   make-object 
	   object%))


