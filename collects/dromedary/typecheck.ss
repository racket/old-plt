#cs(module typecheck mzscheme
	   (require "prims.ss"
		    (prefix ast: "ast.ss")
		    (lib "match.ss")
		    (lib "pretty.ss")
		    (lib "list.ss")
		    )
	   (provide typecheck-all convert-tvars)

	   (define cur-var #\a)

	   (define (typecheck-all stmt)
	     (begin	       (set! cur-var 0)
	       (typecheck-ml stmt (empty-context))))

	   (define (typecheck-ml stmt context)
	     (match stmt
		    [($ ast:expression desc src)
		     (typecheck-expr desc context src)]
		    [(a . b)
		     (if (null? b)
			 (typecheck-ml a context)
			 (cons (typecheck-ml a context) (typecheck-ml b context)))]
		    [($ ast:structure_item desc src)
		     (typecheck-structure desc src context)]
		    [else
		     (pretty-print (list "Cannot typecheck: " stmt))]))

	   (define (typecheck-structure desc src context)
	     (match desc
		    [($ ast:pstr_value rec_flag pelist)
		     (typecheck-defines rec_flag pelist)]
		    [($ ast:pstr_type stdlist)
		     (map typecheck-typedecl stdlist)]
		    [($ ast:pstr_eval expr)
		     (typecheck-ml expr context)]
		    [else
		     (pretty-print (list "Unrecognized structure:" desc))]))
		     
	   (define (typecheck-expr desc context src)
	     (match desc
		    [($ ast:pexp_constant const)
		     (constant-check (eval const))]

		    [($ ast:pexp_tuple xlist)
		     (make-tuple (map typecheck-ml xlist (repeat context (length xlist))))]

		    [($ ast:pexp_ident name)
		     (let ([type (get-type (unlongident name) context)])
		       (if type
			   (begin (pretty-print (list "type:" type))
			   (let ([itype (instantiate type)])
				    (begin (pretty-print (list "itype:" itype)) itype)))
			   (let ([rtype (lookup-ident name)])
			     (if rtype
				 (car (convert-tvars rtype null))
				 (pretty-print (list "No type found for " name))))))]


			   
		    [($ ast:pexp_ifthenelse test ifexp elseexp)
		     (let ([testt (typecheck-ml test context)]
			   [ifexpt (typecheck-ml ifexp context)]
			   [elseexpt (typecheck-ml elseexp context)])
		       (if (not (unify testt "bool"))
			   (pretty-print (list "Expected bool but got: " testt))
			   (if (not (unify ifexpt elseexpt))
			       (pretty-print (list "Expected " ifexpt " but got " elseexpt))
			       ifexpt)))]
		    
		    [($ ast:pexp_match expr pelist)
		     (let ([totest (typecheck-ml expr context)])
		       (typecheck-match pelist totest context))]
		       ;; totest needs to match the beginning of the arrow type of a test
		       ;; all of the types of test need to match
		    [($ ast:pexp_apply proc lelist)
		     (let ([funt (typecheck-ml proc context)]
			   [argst (typecheck-lelist lelist context)])
		       (typecheck-application funt argst))]

		     

		    [($ ast:pexp_let rec bindings expr)
		     (typecheck-let rec bindings expr context)]
;		     (let ([all-bindings (typecheck-bindings rec bindings context src)])
;		       (typecheck-ml expr (append all-bindings context src)))]

		    [($ ast:pexp_function label expr pelist) 
		     (typecheck-function label expr pelist context)]

		    [($ ast:pexp_construct name expr bool)
		     (let ([fconstructor (hash-table-get constructors (unlongident name) (lambda () #f))])
		       (if fconstructor
			   (let ([constructor (car (convert-tvars (car fconstructor) null))])
			     (if (tconstructor? constructor)
				 (if (null? (tconstructor-argtype constructor))
				     (if (null? expr)
					 (tconstructor-result constructor)
					 (pretty-print "Wrong number of arguments for constructor"))
				     (if (unify (tconstructor-argtype constructor) (typecheck-ml expr context))
					 (tconstructor-result constructor)
					 (pretty-print "Type of argument does not equal type of constructor")))
				 constructor))
				 ;; Special case for cons
;				 (if (equal? (cdr fconstructor) cons)
;				     (let* ([listtype (typecheck-ml (car (ast:pexp_tuple-expression-list (ast:expression-pexp_desc expr))) context)]
;					    [gtype (make-tlist listtype)]
;					    [nextexp (typecheck-ml (cadr (ast:pexp_tuple-expression-list (ast:expression-pexp_desc expr))) context)])
;					      (if (unify gtype nextexp)
;						  gtype))

;			       (if (arrow? (car constructor))
;				   (let ([argtype (typecheck-ml expr context)])
;				     (if (unify (car (arrow-arglist (car constructor))) argtype)
;					 (arrow-result (car constructor))
;					 (pretty-print (list "Expected " (car (arrow-arglist (car constructor))) "but found" argtype))))
;				   (pretty-print (list "Expected no arguments for constructor" (unlongident name) "but found" expr))))
			   
			   (pretty-print (list "Constructor not found" (unlongident name)))))]
			   
		    [else
		     (pretty-print (list "Cannot typecheck expression: " desc))]))

		       
	   (define (typecheck-typedecl td)
	     (let ([name (eval (car td))]
		   [typedecl (cdr td)])
	       (match (ast:type_declaration-kind typedecl)
		      [($ ast:ptype_abstract dummy)
		       (let ([rtype (typecheck-type (ast:type_declaration-manifest typedecl))])
			 (begin
			   (hash-table-put! constructors name (cons rtype "some error"))
			   (format "type ~a = ~a" name rtype)))]
		      [($ ast:ptype_variant scll)
		       (let* ([tscll (typecheck-scll name scll)]
			      [ntv (make-tvariant tscll)])
			 (begin
			   (hash-table-put! constructors name (cons ntv "some error"))
			   ntv))])))

	   (define (typecheck-scll sname scll)
	     (if (null? scll)
		 null
		 (let* ([current (car scll)]
			[name (eval (car current))]
			[ttypes (map typecheck-type (cdr current))])
		   (begin
		     (hash-table-put! constructors name (cons (make-tconstructor ttypes sname) #`#,(string->symbol (format "make-~a" name))))
		     (cons (make-tconstructor (if (> (length ttypes) 1)
						 (make-tuple ttypes)
						 (car ttypes)) sname) (typecheck-scll sname (cdr scll)))))))

	   (define (typecheck-match pelist testt context)
	     (let* ([patenvs (map patcheck (repeat context (length pelist)) (repeat testt (length pelist)) (map car pelist))]
		    [expts (map typecheck-ml (map cdr pelist) patenvs)])
	       (if (same-types? expts)
;; Should also be checking that the patterns are exhaustive for t and don't overlap
		   (car expts)
		   'argle)))

	   (define (typecheck-defines rec bindings context)
	     (let ([contextwithbindings (typecheck-bindings rec bindings context)])
	       (begin 
		 (set! cur-var #\a)
		 (let ([definedtypes (map car (map unconvert-tvars contextwithbindings (repeat null (length contextwithbindings))))])
		   (begin
		     (for-each (lambda (name type)
				 (hash-table-put! built-in-and-user-funcs name type #`#,(string->symbol name)))
			       (map car contextwithbindings)
			       definedtypes)
		     definedtypes)))))

	   (define (typecheck-bindings rec bindings context)
	     
	     
	     (if rec
;; get all variables and bind them to a function type
		 (letrec ([bind-vars (lambda (bindings)
					(if (null? bindings)
					    context
					    (let* ([cur-bind (car bindings)]
						   [constraint (ast:ppat_constraint? (ast:pattern-ppat_desc (car cur-bind)))]
						   [rpat (if constraint
							     (ast:ppat_constraint-pat (ast:pattern-ppat_desc (car cur-bind)))
							     (car cur-bind))]
						   [tf (if constraint
							   (typecheck-type (ast:ppat_constraint-type (ast:pattern-ppat_desc (car cur-bind))))
							   (make-arrow (list (fresh-type-var)) (fresh-type-var)))])
					      (update (ast:ppat_var-name (ast:pattern-ppat_desc rpat)) (cons tf null) (bind-vars (cdr bindings))))))])
			  (let ([cprime (bind-vars bindings)])
			    (foldl (lambda (next init)
				     (update (car next) (cdr next) init))
				   context
				   (map (lambda (mapping)
					  (cons (car mapping) (schema (cdr mapping) context)))
					(map typecheck-binding bindings (repeat rec (length bindings)) (repeat cprime (length bindings)))))))
		 (let ([ncontexts (map typecheck-binding bindings (repeat rec (length bindings)) (repeat context (length bindings)))])
		   (foldl union-envs (car ncontexts) (cdr ncontexts)))))

	   (define (typecheck-binding binding rec context)
	     (let* ([rpat (if (ast:ppat_constraint? (ast:pattern-ppat_desc (car binding)))
			      (ast:ppat_constraint-pat (ast:pattern-ppat_desc (car binding)))
			      (car binding))])
	       (if rec
		   (if (and (ast:pexp_function? (ast:expression-pexp_desc (cdr binding)))
			    (ast:ppat_var? (ast:pattern-ppat_desc rpat)))
		       (let ([te (typecheck-ml (cdr binding) context)]
			     [tf (car (get-type (ast:ppat_var-name (ast:pattern-ppat_desc rpat)) context))])
			 (if (unify (get-result te) (get-result tf))
			     (cons (ast:ppat_var-name (ast:pattern-ppat_desc rpat)) tf)))
		       (pretty-print "This kind of expression is not allowed as right-hand side of 'let rec'"))
		   (patcheck context (typecheck-ml (cdr binding) context) rpat))))

;; Note, I should be checking for no duplicate binding
;; I'm also not doing anything with contraints here
	   (define (typecheck-let rec bindings finalexpr context)
	     (typecheck-ml finalexpr (typecheck-bindings rec bindings context)))
;	     (if (null? bindings)
;		 (typecheck-ml finalexpr context)
;		 (let ([cur-bind (car bindings)])
;; Check for a constraint
;		   (let* ([constraint (ast:ppat_constraint? (ast:pattern-ppat_desc (car cur-bind)))]
;			  [rpat (if constraint
;				    (ast:ppat_constraint-pat (ast:pattern-ppat_desc (car cur-bind)))
;				    (car cur-bind))])
;		     (if (and rec
;			      (ast:pexp_function? (ast:expression-pexp_desc (cdr cur-bind)))
;			      (ast:ppat_var? (ast:pattern-ppat_desc rpat)))
;			 (if constraint
;			     (typecheck-let rec (cdr bindings) finalexpr (update (ast:ppat_var-name (ast:pattern-ppat_desc rpat)) (cons (typecheck-ml (cdr cur-bind) (update (ast:ppat_var-name (ast:pattern-ppat_desc rpat)) (typecheck-type (ast:ppat_constraint-type (ast:pattern-ppat_desc (car cur-bind)))) context)) null) context))
;			     (let* ([t1 (fresh-type-var)]
;				    [t2 (fresh-type-var)]
;				    [tf (make-arrow (list t1) t2)]
;				    [cprime (update (ast:ppat_var-name (ast:pattern-ppat_desc rpat)) (cons tf null) context)]
;				    [te (typecheck-ml (cdr cur-bind) cprime)])
;			       (if (unify (get-result te) t2)
;;			       (if (unify te tf)
;				   (typecheck-let rec (cdr bindings) finalexpr (update (ast:ppat_var-name (ast:pattern-ppat_desc rpat)) (schema tf context) context)))))
;
;			 (typecheck-let rec (cdr bindings) finalexpr (patcheck context (typecheck-ml (cdr cur-bind) context) rpat)))))))

	   (define (typecheck-lelist lelist context)
	     (if (null? lelist)
		 null
		 (cons (typecheck-ml (cdar lelist) context) (typecheck-lelist (cdr lelist) context))))

	   (define (typecheck-function label expr pelist context)
	     ;; Assumes no label and no expr
;	     (let* ([patenvs (map patcheck (repeat context (length pelist)) (map typecheck-type (map ast:ppat_constraint-type (map ast:pattern-ppat_desc (map car pelist))) (repeat context (length pelist))) (map car pelist))]
	     (let* (
		    [funvarenvs (map funvarcheck (repeat context (length pelist)) (map car pelist))]
		    [expts (map typecheck-ml (map cdr pelist) (map cdr funvarenvs))])
	     
	       (if (and (same-types? (map car funvarenvs)) (same-types? expts))
;; Should also be checking that the patterns are exhaustive for t and don't overlap
		   (make-arrow (list (car (car funvarenvs))) (car expts)))))


	   (define (typecheck-type asttype)
	     (match (ast:core_type-desc asttype)
		    [($ ast:ptyp_any dummy)
		     (make-tvar (box null))]
		    [($ ast:ptyp_var f)
;; Fix this! Should be similar to reading in the premade functions - or do we let the type checker figure it out later. Let's try that.
		     
		     (make-tvar (box null))]
		    [($ ast:ptyp_arrow label ct1 ct2)
		     (make-arrow (list (typecheck-type ct1)) (typecheck-type ct2))]
		    [($ ast:ptyp_tuple ctlist)
		     (make-tuple (map typecheck-type ctlist))]
		    [($ ast:ptyp_constr name ctlist)
		     (let ([constructor (hash-table-get constructors (unlongident name) (lambda () #f))])
		       (if constructor
			   (let ([fconstructor (car (convert-tvars (car constructor) null))])
			     
			     (cond
			      [(tconstructor? fconstructor)
			       (if (null? (tconstructor-argtype fconstructor))
				   (if (null? ctlist)
				       (tconstructor-result fconstructor)
				       (pretty-print (list "Constructor expects no arguments but got:" ctlist)))
				   (if (unify (typecheck-type (car ctlist)) (tconstructor-argtype))
				       (tconstructor-result fconstructor)))]
				       
			       
			      [(and (istype? "arrow" fconstructor)
				    (eval `(and ,@(map unify (map typecheck-type ctlist) (get-arglist fconstructor)))))
			       fconstructor]
			      [(equal? (unlongident name) "list")
			       
			       (make-tlist (typecheck-type (car ctlist)))]
			      [(null? ctlist)
			       fconstructor]
			      [else
			       (pretty-print (list (car constructor) "takes no arguments but was given" ctlist))]))
			   (pretty-print (list "Unknown constructor" name))))]
		    [($ ast:ptyp_variant rfl abool ll)
		     'comeslater]
		    [else
		     (pretty-print "Cannot handle type")]))
		    

	   (define (typecheck-application funt argst)
	     (if (not (unify (make-arrow (list (fresh-type-var)) (fresh-type-var)) funt))
		 (pretty-print (list "Expected function but got: " funt))
		 (let ([arglist (get-arglist funt)])
		   (if (= (length arglist) (length argst))
		       (begin (pretty-print (list "funt:" funt "argst:" argst))
		       (if (eval `(and ,@(map unify arglist argst)))
			   (begin
			     (pretty-print (list "funt:" funt "argst:" argst))
			   (get-result funt))))
		       (if (> (length argst) (length arglist))
			   (foldl uncurry (if (unify (car (get-arglist funt)) (car argst))
					      (get-result funt)) 
				  (cdr argst))
			   (pretty-print (list "Expected " (length arglist) " arguments but found " (length argst))))))))


	   (define (uncurry nextt init)
	     (if (unify init (make-arrow (list nextt) (fresh-type-var)))
		 (let ([arglist (get-arglist init)])
		   (if (> (length arglist) 1)
		       (pretty-print (list "This should never happen: uncurry"))
;		       (if (unify (car arglist) nextt)
		       (get-result init)))))

		    
	   (define (funvarcheck context pat)
	     (if (unique-var pat null)
		 (let ([varenv (funenv pat)])
		   (cons (car varenv) (union-envs (cdr varenv) context)))))

	   (define (patcheck context type pat) 
	     (if (unique-var pat null)
		 (union-envs (patenv pat type context) context)
		 'fuzzle))

	   (define (unique-var pat curvars)
	     (match (ast:pattern-ppat_desc pat)
		    [($ ast:ppat_any dummy)
		     null]
		    [($ ast:ppat_var name)
		     (if (present? name curvars)
			 (pretty-print (list "This variable is bound several times in this matching: " name))
			 (cons name curvars))]
		    [($ ast:ppat_constant const)
		     null]
		    [($ ast:ppat_tuple plist)
		     (unique-var-list plist curvars)]
		    [($ ast:ppat_construct longident dpat bool)
		     (unique-var dpat curvars)]
		    [($ ast:ppat_constraint pat ct)
		     (unique-var pat curvars)]))

	   (define (unique-var-list plist curvars)
	     (if (null? plist)
		 curvars
		 (unique-var-list (cdr plist) (unique-var (car plist) curvars))))

	   (define (funenv pat)
	     (match (ast:pattern-ppat_desc pat)
		    [($ ast:ppat_any dummy)
		       (cons (fresh-type-var) (empty-context))]
		    [($ ast:ppat_var name)
		     (let* ([t (fresh-type-var)]
			    [ts (cons t null)])
		       (cons t (update name ts (empty-context))))]
		    [($ ast:ppat_constant const)
		     (cons (constant-check const) (empty-context))]
		    [($ ast:ppat_tuple plist)
		     (let ([varenvs (map funenv plist)])
		       (cons (make-tuple (map car varenvs)) (map cdr varenvs)))]
		    [($ ast:ppat_construct longident cpat bool)
		       (let* ([pat-type (hash-table-get constructors (unlongident longident) (lambda () #f))])
			 (if (and pat-type (tconstructor? pat-type))
			     (let ([varenv (funenv cpat)]
				   [cpat-type (car (convert-tvars pat-type null))])
			       (if (unify varenv (tconstructor-argtype cpat-type))
				   (cons (tconstructor-result cpat-type) (cdr varenv))))
			     (cons pat-type (empty-context))))]
		    [($ ast:ppat_constraint pat ct)
		     ;Assume a constraint must immediately follow a variable
		     (let ([type (typecheck-type ct)]
			   [varname (ast:ppat_var-name (ast:pattern-ppat_desc pat))])
		       (cons type (update varname (cons type null) (empty-context))))]
		    [else (pretty-print "No match found")]))
		     
	   (define (patenv pat type context)
	     (match (ast:pattern-ppat_desc pat)
		    [($ ast:ppat_any dummy)
		     (empty-context)]
		    [($ ast:ppat_var name)
		     (update name (schema type context) (empty-context))]
		    [($ ast:ppat_constant const)
		     (let ([t2 (constant-check const)])
		       (if (unify type t2)
			   (empty-context)
			   (pretty-print (list ("Expected " type " but found " t2)))))]
		    [($ ast:ppat_tuple plist)
		     (if (istype? "tuple" type)
			 (let ([tlist (tuple-list type)])
			   (if (= (length tlist) (length plist))
			       (let ([nenvs (map patenv plist tlist (repeat context (length plist)))])
				 (foldl union-envs (car nenvs) (cdr nenvs)))
			       (pretty-print (list ("Expected tuple of length " (length tlist) " but found tuple of length " (length plist))))))
			 (pretty-print (list ("Expected " type " but found tuple"))))]
		    [($ ast:ppat_construct longident cpat bool)
		     (let* ([pat-type (hash-table-get constructors (unlongident longident) (lambda () #f))])
		       (if pat-type
			   (let ([cpat-type (car (convert-tvars pat-type null))])
			     (if (unify type cpat-type)
			       (if (tconstructor? cpat-type)
				   (patenv (cons (tconstructor-argtype cpat-type)) (cons cpat))

				   (empty-context))
			       (pretty-print (list "Expected " type " but found " pat-type)))
			   (pretty-print (list "No such constructor" (unlongident longident))))))]
		    [($ ast:ppat_constraint pat ct)
		     (if (unify type (typecheck-type ct))
			 (update (ast:ppat_var-name (ast:pattern-ppat_desc pat)) (schema type context) (empty-context)))]
		    [else
		     (pretty-print "Pattern not recognized")]))

	     
	   (define (same-types? tlist)
	     (if (= (length tlist) 1)
		 #t
		 (let ([tocompare (cdr tlist)])
		   (and (eval `(and ,@(map unify (repeat (car tlist) (length tocompare)) tocompare))) (same-types? tocompare)))))

	   (define (union tvl1 tvl2)
	     (foldl (lambda (r l)
		      (if (null? (filter (lambda (rprime) (eqv? r rprime)) l))
			  (cons r l)
			  l))
		    tvl1 tvl2))

	   (define (diff l1 l2)
	     (filter (lambda (r) (null? (filter (lambda (rprime) (equal? r rprime)) l2))) l1))


	   (define (unsolved type)
	     (cond
	      [(string? type) null]
	      [(arrow? type) (union (unsolved (car (arrow-arglist type)))
				    (unsolved (arrow-result type)))]
	      [(tuple? type) (foldl (lambda (ut sl)
				      (union (unsolved ut) sl)) (unsolved (car (tuple-list type))) (cdr (tuple-list type)))]
	      [(tlist? type) (unsolved (tlist-type type))]
	      [(tvar? type) (let ([r (tvar-tbox type)])
				  (if (null? (unbox r))
				      (list r)
				      (unsolved (unbox r))))]))

	   (define (env-unsolved r)
	     (foldl (lambda (mapping tlist)
		      (let ([l1 (unsolved (cadr mapping))])
			(union tlist l1))) null r))
					      
	   (define (instantiate type-tvarlist)
	     (let ([type (car type-tvarlist)]
		   [tvarlist (cdr type-tvarlist)])
	       (letrec ([tm (foldl (lambda (tv tm)
				     (cons (cons tv (fresh-type-var)) tm))
				   null
				   tvarlist)]
			[instVar (lambda (tv tm)
				   (if (null? tm)
				       (make-tvar tv)
				       (if (eqv? (caar tm) tv)
					   (cdar tm)
					   (instVar tv (cdr tm)))))]
			[inst (lambda (t)
				(cond
				 [(string? t) t]
				 [(arrow? t) (make-arrow (list (inst (car (arrow-arglist t)))) (inst (arrow-result t)))]
				 [(tuple? t) (make-tuple (map inst (tuple-list t)))]
				 [(tlist? t) (make-tlist (inst (tlist-type t)))]
				 [(tvar? t) (if (null? (unbox (tvar-tbox t)))
						(instVar (tvar-tbox t) tm)
						(inst (unbox (tvar-tbox t))))]))])
		 (inst type))))
		 
	   (define (schema type env)
	     (let* ([uv (unsolved type)]
		    [ev (env-unsolved env)]
		    [uvprime (diff uv ev)])
	       (cons type uvprime)))

	   (define (unify t1 t2)
	     (pretty-print (list "unify:" t1 t2))
	     (cond
;	      [(eq? t1 t2) #t]
	      [(tvar? t1) (unify-var t2 (tvar-tbox t1))]
	      [(string? t1)
	       (cond
		[(and (string? t2) (equal? t1 t2)) #t]
		[(tvar? t2) (unify-var t1 (tvar-tbox t2))]
		[else (begin (pretty-print (list "Expected: " t1)) #f)])]
	      [(arrow? t1)
	       (cond
		[(arrow? t2) (and (unify (car (arrow-arglist t1)) (car (arrow-arglist t2))) (unify (arrow-result t1) (arrow-result t2)))]
		[(tvar? t2) (unify-var t1 (tvar-tbox t2))]
		[else (begin (pretty-print "Expected an arrow type") #f)])]
	      [(tuple? t1)
	       (cond
		[(tuple? t2) (eval `(and ,@(map unify (tuple-list t1) (tuple-list t2))))]
		[(tvar? t2) (unify-var t1 (tvar-tbox t2))]
		[else (begin (pretty-print "Expected a tuple type") #f)])]
	      [(tlist? t1)
	       (cond
		[(tlist? t2) (unify (tlist-type t1) (tlist-type t2))]
		[(tvar? t2) (unify-var t1 (tvar-tbox t2))]
		[else (begin (pretty-print "Expected a list type") #f)])]))

	   (define (unify-var type tbox)
	     (pretty-print (list "unify-var:" type tbox))
	     (if (null? (unbox tbox))
		 (if (and (tvar? type) (eqv? (tvar-tbox type) tbox))
		     #t
		     (if (and (tvar? type) (not (null? (unbox (tvar-tbox type)))))
			 (unify-var (unbox (tvar-tbox type)) tbox)
			 (begin
			   (pretty-print (list "Setting" tbox "to" type))
			   (set-box! tbox type)
			   #t)))
		 (unify (unbox tbox) type)))

	   (define (lookup-ident uname)
	     (match uname
		    [($ ast:lident name)
		     (let ([result (hash-table-get built-in-and-user-funcs name (lambda () #f))])
		       (if result
			   (car result)
			   (begin
			     (pretty-print (list "Error: Unbound variable " name))
			     #f)))]
		    [($ ast:ldot longident name)

		     (if (ast:lident? longident)
			 (let ([lib-map (hash-table-get library-names (ast:lident-name longident) (lambda () #f))])
			   (if lib-map
			       (let ([function (hash-table-get lib-map (syntax-object->datum name) (lambda () #f))])
				 (if function
				     (car function)
				     (begin
				       (pretty-print (list "Error: " (syntax-object->datum name) "not found in" (ast:lident-name longident)))
				       #f)))
			       (begin
				 (pretty-print (list "Error: " (ast:lident-name longident) "not found"))
				 #f))))]))

	   (define (convert-tvars type mappings)
	     (pretty-print (list "convert-tvars" type mappings))
	     (cond
	      [(string? type) (cons type mappings)]
	      [(tuple? type) (let ([tlist (convert-list (tuple-list type) mappings null)])
			       (cons (make-tuple (reverse (car tlist))) (cdr tlist)))]
	      [(arrow? type) (let* ([tlist (convert-list (arrow-arglist type) mappings null)]
				    [restype (convert-tvars (arrow-result type) (cdr tlist))])
			       (cons (make-arrow (reverse (car tlist)) (car restype)) (cdr restype)))]
	      [(tconstructor? type) (let* ([argtype (convert-tvars (tconstructor-argtype type) mappings)]
					   [restype (convert-tvars (tconstructor-result type) (cdr argtype))])
				      (cons (make-tconstructor (car argtype) (car restype)) (cdr restype)))]
					   
	      [(tlist? type) (let ([ltype (convert-tvars (tlist-type type) mappings)])
			       (cons (make-tlist (car ltype)) (cdr ltype)))]
	      [(tvar? type) (let ([mapped-type (get-type (tvar-tbox type) mappings)])
			      (if mapped-type
				  (cons (make-tvar (box mapped-type)) mappings)
				  (let ([newvar (make-tvar (box null))])
				    (cons newvar (cons (cons (tvar-tbox type) newvar) mappings)))))]
	      [else (pretty-print "Bad type to convert!")]))

	   (define (unconvert-tvars type mappings)
	     (cond
	      [(string? type) (cons type mappings)]
	      [(tuple? type) (let ([tlist (unconvert-list (tuple-list type) mappings null)])
			       (cons (make-tuple (reverse (car tlist))) (cdr tlist)))]
	      [(arrow? type) (let* ([tlist (unconvert-list (arrow-arglist type) mappings null)]
				   [restype (unconvert-tvars (arrow-result type) (cdr tlist))])
			      (cons (make-arrow (reverse (car tlist)) (car restype)) (cdr restype)))]
	      [(tlist? type) (let ([ltype (unconvert-tvars (tlist-type type) mappings)])
			       (cons (make-tlist (car ltype)) (cdr ltype)))]
	      [(tvar? type) (let ([dbox (tvar-tbox type)])
			      (if (null? (unbox dbox))
				  (letrec ([tfunc (lambda (maplist)
						    (if (null? maplist)
							#f
							(if (eqv? (caar maplist) dbox)
							    (cdar maplist)
							    (tfunc (cdr maplist)))))])
				    (let ([res (tfunc mappings)])
				      (if res
					  res
					  (begin
					    (set! cur-var (integer->char (+ 1 (char->integer cur-var))))
					    (let ([old-var (format "~a" (integer->char (- (char->integer cur-var) 1)))])
					      (cons old-var (cons (cons dbox old-var) mappings)))))))
				  (unconvert-tvars (unbox dbox) mappings)))]))
	   
	   (define (unconvert-list tlist mappings results)
	     (if (null? tlist)
		 (cons results mappings)
		 (let ([firstres (unconvert-tvars (car tlist) mappings)])
		   (unconvert-list (cdr tlist) (cdr firstres) (cons (car firstres) results)))))

	   (define (convert-list tlist mappings results)
	     (if (null? tlist)
		 (cons results mappings)
		 (let ([firstres (convert-tvars (car tlist) mappings)])
		   (convert-list (cdr tlist) (cdr firstres) (cons (car firstres) results)))))
			       
	   (define (fresh-type-var)
	     (make-tvar (box null)))
;	     (begin
;	       (set! cur-var (+ cur-var 1))
;	       (format "<tvar~a>" (- cur-var 1))))

	   (define (constant-check const)
	     (cond
	      [(integer? const) "int"]
	      [(float? const) "float"]
	      [(char? const) "char"]
	      [(boolean? const) "bool"]
	      [(string? const) "string"]
	      [else "unit"]))

	   (define (get-arglist type)
	     (cond
	      [(arrow? type) (arrow-arglist type)]
	      [(tvar? type) (if (null? (unbox (tvar-tbox type)))
				(pretty-print "Non-unified type variable!")
				(get-arglist (unbox (tvar-tbox type))))]
	      [else (pretty-print (list "Expected arrow but found" type))]))

	   (define (get-result type)
	     (cond
	      [(arrow? type) (arrow-result type)]
	      [(tvar? type) (if (null? (unbox (tvar-tbox type)))
				(pretty-print "Non-unified type variable!")
				(get-result (unbox (tvar-tbox type))))]
	      [else (pretty-print (list "Expected arrow but found" type))]))

	   (define (istype? typename-as-symbol rtype)
	     (pretty-print (list "istype?:" typename-as-symbol rtype))
	     (let ([pred (datum->syntax-object (syntax rtype) (string->symbol (format "~a?" typename-as-symbol)))])
	       (eval #`(cond
			[(#,pred #,rtype) #t]
			[(tvar? #,rtype) (if (null? (unbox (tvar-tbox #,rtype)))
					     (begin (pretty-print "istype?: ununified type-var") #f)
					  (istype? #,typename-as-symbol (unbox (tvar-tbox #,rtype))))]
			[else #f]))))
	     
;	   (define-syntax (istype? stx)
;	     (syntax-case stx ()
;			  ((_ tname type)
;			   (with-syntax ((act-name (datum->syntax-object
;						    (syntax type)
;						    (string->symbol
;						     (format "~a?"
;							     (syntax-object->datum (syntax tname)))))))
;					(syntax
;					 (cond
;					  [(act-name type) #t]
;					  [(tvar? type) (if (null? (unbox (tvar-tbox type)))
;							    (pretty-print "Non-unified type variable: istype?")
;							    (istype? tname (unbox (tvar-tbox type))))]
;					  [else (pretty-print (list "Expected" tname "but found" type))]))))))


	   
	   (define (unlongident uname)
	     (match uname
		    [($ ast:lident name) name]
		    [($ ast:ldot longident name) (format "~a.~a" (unlongident longident) name)]))
	   

	   (define (repeat x n)
	     (if (<= n 0)
		 null
		 (cons x (repeat x (- n 1)))))

	   (define-struct tcontext (context constraints))

	   (define (union-envs c1 c2)
	     (cond
	      [(null? c1) c2]
	      [(null? c2) c1]
	      [(equal? (car c1) (car c2)) c2]
	      [else (cons (car c1) (union-envs (cdr c1) c2))]))

	   
	   (define (present? var vlist)
	     (if (null? vlist)
		 #f
		 (if (equal? (car vlist) var)
		     #t
		     (present? var (cdr vlist)))))

	   (define (get-type var context)
	     (if (null? context)
		 #f
		 (if (equal? (caar context) var)
		     (cdar context)
		     (get-type var (cdr context)))))

	   (define (update var type-tvarlist context)
	     (cons (cons var type-tvarlist) context))

	   (define (empty-context) null)

)
