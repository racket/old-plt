#cs(module typecheck mzscheme
	   (require "prims.ss"
		    (prefix ast: "ast.ss")
		    (lib "match.ss")
		    (lib "pretty.ss")
		    (lib "list.ss")
		    )
	   (provide typecheck-all convert-tvars)

	   (define cur-var #\a)

;;;; Stolen from Matthew
	   (define (at expr src)
	     (datum->syntax-object (current-compile-context) expr
				   (list loc
					 (ast:src-line src)
					 (ast:src-col src)
					 (ast:src-pos src)
					 (ast:src-span src))))

	   (define current-compile-context (make-parameter #f))

;;;; Stolen from Kathy Section
	   (define loc #f)

	   (define stx-for-original-property (read-syntax #f (open-input-string "original")))
;;;; End stolen from Kathy Section

	   (define (typecheck-all stmt location)
	     (begin	       
	       (set! cur-var #\a)
	       (set! loc location)
	       (let ([progtype (typecheck-ml stmt (empty-context))])
		 (begin (pretty-print (format "initial progtype: ~a" progtype))
		 (if (list? progtype)
		     (map car (map unconvert-tvars progtype (repeat null (length progtype))))
		     (list (car (unconvert-tvars progtype null))))))))

	   (define (typecheck-ml stmt context)
;	     (pretty-print (format "typecheck-ml ~a" stmt))
	     (match stmt
		    [($ ast:expression desc src)
		     (typecheck-expr desc context src)]
		    [(a . b)
;		     (begin (pretty-print "typechecking a list")
		     (map typecheck-ml stmt (repeat context (length stmt)))]
;		     (if (null? b)
;			 (let ([v (typecheck-ml a context)])
;			   (begin (pretty-print (format "typechecking last item of list: ~a" v))
;				  v))
;			 (let ([f (typecheck-ml a context)]
;			       [l (typecheck-ml b context)])
;			   (begin
;			     (pretty-print (format "typechecking a list: ~a ~a" f l))
;			 (cons (typecheck-ml a context) (typecheck-ml b context)))))]
		    [($ ast:structure_item desc src)
		     (typecheck-structure desc src context)]
		    [else
		     (raise-syntax-error #f (format "Cannot typecheck: ~a" stmt) (at stmt (ast:make-src 0 0 0 0)))]))

	   (define (typecheck-structure desc src context)
;	     (pretty-print (format "typecheck-structure ~a" desc))
	     (match desc
		    [($ ast:pstr_value rec_flag pelist)
		     (typecheck-defines rec_flag pelist context)]
		    [($ ast:pstr_type stdlist)
		     (map typecheck-typedecl stdlist)]
		    [($ ast:pstr_eval expr)
		     (typecheck-ml expr context)]
		    [($ ast:pstr_exception name decl)
		     (let ([nconst (make-tconstructor (make-<tuple> (map typecheck-ml decl)) "exception")])
		       (begin
			 (hash-table-put! constructors (eval name) (make-tconstructor (make-<tuple> (map typecheck-ml decl))) "exception")
			 (make-mlexn (eval name) nconst)))]
		       
		    [else
		     (raise-syntax-error #f (format "Unrecognized structure: ~a") (at desc src))]))

		     
	   (define (typecheck-expr desc context src)
	     (match desc
		    [($ ast:pexp_constant const)
		     (constant-check (eval const))]

		    [($ ast:pexp_tuple xlist)
		     (make-<tuple> (map typecheck-ml xlist (repeat context (length xlist))))]

		    [($ ast:pexp_ident name)
		     (let ([type (get-type (unlongident name) context)])
		       (if type
			   (let ([itype (instantiate type)])
			     itype)
			   (let ([rtype (lookup-ident name (at desc src))])
			     (if rtype
				 (car (convert-tvars rtype null))
				 (raise-syntax-error #f (format "No type found for ~a" name) (at name src))))) )]


			   
		    [($ ast:pexp_ifthenelse test ifexp elseexp)
		     (let ([testt (typecheck-ml test context)]
			   [ifexpt (typecheck-ml ifexp context)]
			   [elseexpt (typecheck-ml elseexp context)])
		       (if (unify "bool" testt (at test (ast:expression-pexp_src test)))
			   (if (unify ifexpt elseexpt (at elseexp (ast:expression-pexp_src elseexp)))
			       ifexpt)))]
		    
		    [($ ast:pexp_match expr pelist)
		     (let ([totest (typecheck-ml expr context)])
		       (typecheck-match pelist totest context))]
		       ;; totest needs to match the beginning of the arrow type of a test
		       ;; all of the types of test need to match
		    [($ ast:pexp_apply proc lelist)
		     (let ([funt (typecheck-ml proc context)]
			   [argst (typecheck-lelist lelist context)])
		       (typecheck-application funt argst (at proc (ast:expression-pexp_src proc)) (map at (map cdr lelist) (map ast:expression-pexp_src (map cdr lelist)))))]

		     

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
					 (raise-syntax-error #f "Wrong number of arguments for constructor" (at expr (ast:expression-pexp_src expr))))
				     (if (unify (tconstructor-argtype constructor) (typecheck-ml expr context) (at expr (ast:expression-pexp_src expr)))
					 (tconstructor-result constructor)))
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
			   
			   (raise-syntax-error #f (format "Constructor not found: ~a" (unlongident name) ) name)))]
			   
		    [($ ast:pexp_try tryexp pelist)
		     (let ([tryt (typecheck-ml tryexp)]
			   [exnt (typecheck-match pelist "exception" context)])
		       (if (unify tryt exnt)
			   tryt))]

		    [else
		     (raise-syntax-error #f (format "Cannot typecheck expression: ~a" desc) (at desc src))]) )

		       
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
			      [ntv (make-tvariant name (map eval (map car scll)) tscll)])
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
		     (hash-table-put! constructors name (cons (make-tconstructor (if (> (length ttypes) 1)
										 (make-<tuple> ttypes)
										 (car ttypes)) (make-usertype sname)) #`#,(string->symbol (format "make-~a" name))))
		     (cons (make-tconstructor (if (> (length ttypes) 1)
						 (make-<tuple> ttypes)
						 (car ttypes)) (make-usertype sname)) (typecheck-scll sname (cdr scll)))))))

	   (define (typecheck-match pelist testt context)
	     (let* ([patenvs (map patcheck (repeat context (length pelist)) (repeat testt (length pelist)) (map car pelist))]
		    [expts (map typecheck-ml (map cdr pelist) patenvs)])
	       (if (same-types? expts (map at (map cdr pelist) (map ast:expression-pexp_src (map cdr pelist))))
;; Should also be checking that the patterns are exhaustive for t and don't overlap
		   (car expts))))

	   (define (typecheck-defines rec bindings context)
	     (let ([contextwithbindings (typecheck-bindings rec bindings context)])
	       (begin 
		 (set! cur-var #\a)
		 (let ([definedtypes (reverse (map car (map unconvert-tvars (map car (map cdr contextwithbindings)) (repeat null (length contextwithbindings)))))])
		   (begin
		     (for-each (lambda (name type)
				 (hash-table-put! built-in-and-user-funcs name (cons type #`#,(string->symbol name))))
			       (reverse (map car contextwithbindings))
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
					      (update (eval (ast:ppat_var-name (ast:pattern-ppat_desc rpat)) (cons tf null) (bind-vars (cdr bindings)))))))])
			  (let ([cprime (bind-vars bindings)])
			    (foldl (lambda (next init)
				     (update (eval (car next)) (cdr next) init))
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
			     [tf (car (get-type (eval (ast:ppat_var-name (ast:pattern-ppat_desc rpat)) context)))])
			 (if (unify (get-result tf) (get-result te) (at (cdr binding) (ast:expression-pexp_src (cdr binding))))
			     (cons (eval (ast:ppat_var-name (ast:pattern-ppat_desc rpat))) tf)))
		       (raise-syntax-error #f "This kind of expression is not allowed as right-hand side of 'let rec'" (at (cdr binding) (ast:expression-pexp_src (cdr binding)))))
		   (patcheck context (typecheck-ml (cdr binding) context) rpat))))

;; Note, I should be checking for no duplicate binding
;; I'm also not doing anything with contraints here
	   (define (typecheck-let rec bindings finalexpr context)
	     (typecheck-ml finalexpr (typecheck-bindings rec bindings context)))

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
	     
	       (if (and (same-types? (map car funvarenvs) (map grab-syntax pelist)) (same-types? expts (map at (map cdr pelist) (map ast:expression-pexp_src (map cdr pelist)))))
;; Should also be checking that the patterns are exhaustive for t and don't overlap
		   (make-arrow (list (car (car funvarenvs))) (car expts)))))


	   (define (grab-syntax pe)
	     (at (car pe) (ast:pattern-ppat_src (car pe))))

	   
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
		     (make-<tuple> (map typecheck-type ctlist))]
		    [($ ast:ptyp_constr name ctlist)
		     (let ([constructor (hash-table-get constructors (unlongident name) (lambda () #f))])
		       (if constructor
			   (let ([fconstructor (car (convert-tvars (car constructor) null))])
			     
			     (cond
			      [(tconstructor? fconstructor)
			       (if (null? (tconstructor-argtype fconstructor))
				   (if (null? ctlist)
				       (tconstructor-result fconstructor)
				       (raise-syntax-error #f (format "Constructor expects no arguments but got:~a" ctlist) (at asttype (ast:core_type-src asttype))) )
				   (if (unify (tconstructor-argtype) (typecheck-type (car ctlist)) (at (car ctlist) (ast:core_type-src (car ctlist))))
				       (tconstructor-result fconstructor)))]
				       
			       
;			      [(and (istype? "arrow" fconstructor)
;				    (eval `(and ,@(map unify (map typecheck-type ctlist) (get-arglist fconstructor)))))
;			       fconstructor]
			      [(equal? (unlongident name) "list")
			       
			       (make-tlist (typecheck-type (car ctlist)))]
			      [(null? ctlist)
			       fconstructor]
			      [else
			       (raise-syntax-error #f (format "~a takes no arguments but was given ~a" (car constructor)  ctlist) (at asttype (ast:core_type-src asttype)))]) )
			   (raise-syntax-error #f (format "Unknown constructor: ~a" name) (at asttype (ast:core_type-src asttype)))) )]
		    [($ ast:ptyp_variant rfl abool ll)
		     'comeslater]
		    [else
		     (raise-syntax-error #f (format "Cannot handle type: ~a" asttype) (at asttype (ast:core_type-src asttype)))]))

		    

	   (define (typecheck-application funt argst funsyn argssyn)
	     (if (unify (make-arrow (list (fresh-type-var)) (fresh-type-var)) funt funsyn)
		 (let ([arglist (get-arglist funt)])
		   (if (= (length arglist) (length argst))
		       (if (eval `(and ,@(map unify arglist argst argssyn)))
			   (get-result funt))
		       (if (> (length argst) (length arglist))
			   (foldl uncurry (if (unify (car (get-arglist funt)) (car argst) (car argssyn))
					      (get-result funt)) 
				  (cdr argst)
				  (cdr argssyn))
			   (raise-syntax-error #f (format "Expected ~a arguments but found ~a" (length arglist) (length argst)) funsyn))))) )


	   (define (uncurry nextt argsyn init)
	     (if (unify init (make-arrow (list nextt) (fresh-type-var)) argsyn)
		 (let ([arglist (get-arglist init)])
		   (if (> (length arglist) 1)
		       (raise-syntax-error #f (format "This should never happen: uncurry") nextt)
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
			 (raise-syntax-error #f (format "This variable is bound several times in this matching: ~a" name) (at pat (ast:pattern-ppat_src pat)))
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
		       (cons t (update (eval name) ts (empty-context))))]
		    [($ ast:ppat_constant const)
		     (cons (constant-check const) (empty-context))]
		    [($ ast:ppat_tuple plist)
		     (let ([varenvs (map funenv plist)])
		       (cons (make-<tuple> (map car varenvs)) (map cdr varenvs)))]
		    [($ ast:ppat_construct longident cpat bool)
		       (let* ([pat-type (hash-table-get constructors (unlongident longident) (lambda () #f))])
			 (if pat-type
			     (let ([cpat-type (car (convert-tvars (car pat-type) null))])
			       (if (tconstructor? cpat-type)
				   (if (null? cpat)
				       (if (null? (tconstructor-argtype cpat-type))
					   (cons (tconstructor-result cpat-type) (empty-context))
					   (raise-syntax-error #f "Constructor expects an argument" (at pat (ast:pattern-ppat_src pat))))
				       (if (null? (tconstructor-argtype cpat-type))
					   (raise-syntax-error #f "Constructor expects no arguments" (at pat (ast:pattern-ppat_src pat)))
					   (cons (tconstructor-result cpat-type) (cdr (funenv cpat)))))
				   (if (null? cpat)
				       (cons cpat-type (empty-context))
				       (raise-syntax-error #f "Constructor expects no arguments" (at pat (ast:pattern-ppat_src pat))))))
			     (raise-syntax-error #f (format "No such constructor ~a" (unlongident longident)) (at pat (ast:pattern-ppat_src pat)))) )]

		    [($ ast:ppat_constraint pat ct)
		     ;Assume a constraint must immediately follow a variable
		     (let ([type (typecheck-type ct)]
			   [varname (eval (ast:ppat_var-name (ast:pattern-ppat_desc pat)))])
		       (cons type (update varname (cons type null) (empty-context))))]
		    [else (raise-syntax-error #f (format "No such pattern found: ~a" pat) (at pat (ast:pattern-ppat_src pat)))]))
		     
	   (define (patenv pat type context)
	     (let ([patsyn (at pat (ast:pattern-ppat_src pat))])
	     (match (ast:pattern-ppat_desc pat)
		    [($ ast:ppat_any dummy)
		     (empty-context)]
		    [($ ast:ppat_var name)
		     (update (eval name) (schema type context) (empty-context))]
		    [($ ast:ppat_constant const)
		     (let ([t2 (constant-check const)])
		       (if (unify type t2 patsyn)
			   (empty-context)
			   (raise-syntax-error #f (format "Expected ~a but found ~a" type t2) patsyn))) ]
		    [($ ast:ppat_tuple plist)
		     (if (istype? "<tuple>" type)
			 (let ([tlist (<tuple>-list type)])
			   (if (= (length tlist) (length plist))
			       (let ([nenvs (map patenv plist tlist (repeat context (length plist)))])
				 (foldl union-envs (car nenvs) (cdr nenvs)))
			       (raise-syntax-error #f (format "Expected tuple of length ~a but found tuple of length " (length tlist) (length plist)) patsyn)) )
			 (raise-syntax-error #f (format "Expected ~a but found tuple" type) patsyn) )]
		    [($ ast:ppat_construct longident cpat bool)
		     (let* ([pat-type (hash-table-get constructors (unlongident longident) (lambda () #f))])
		       (if pat-type
			   (let ([cpat-type (car (convert-tvars (car pat-type) null))])
;			     (if (unify type (tconstructor-result cpat-type))
				    (if (tconstructor? cpat-type)
					(if (unify type (tconstructor-result cpat-type) patsyn)
					    (if (null? cpat)
						(if (null? (tconstructor-argtype cpat-type))
						    (empty-context)
						    (raise-syntax-error #f "Constructor expects an argument" patsyn))
						(if (null? (tconstructor-argtype cpat-type))
						    (raise-syntax-error #f "Constructor expects no arguments" patsyn)
						    (patenv cpat (tconstructor-argtype cpat-type) context))))
					(if (unify type cpat-type patsyn)
					    (if (null? cpat)
						(empty-context)
						(raise-syntax-error #f "Constructor expects no arguments" patsyn)))))
			   (raise-syntax-error #f (format "No such constructor ~a" (unlongident longident)) patsyn)) )]
		    [($ ast:ppat_constraint pat ct)
		     (if (unify type (typecheck-type ct) patsyn)
			 (update (eval (ast:ppat_var-name (ast:pattern-ppat_desc pat))) (schema type context) (empty-context)))]
		    [else
		     (raise-syntax-error #f "Pattern not recognized" patsyn)])))

	     
	   (define (same-types? tlist synlist)
	     (if (= (length tlist) 1)
		 #t
		 (let ([tocompare (cdr tlist)])
		   (and (eval `(and ,@(map unify (repeat (car tlist) (length tocompare)) tocompare (cdr synlist)))) (same-types? tocompare (cdr synlist))))))

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
	      [(<tuple>? type) (foldl (lambda (ut sl)
				      (union (unsolved ut) sl)) (unsolved (car (<tuple>-list type))) (cdr (<tuple>-list type)))]
	      [(tlist? type) (unsolved (tlist-type type))]
	      [(tvar? type) (let ([r (tvar-tbox type)])
				  (if (null? (unbox r))
				      (list r)
				      (unsolved (unbox r))))]
	      [(option? type) (unsolved (option-type type))]
	      [(ref? type) (unsolved (ref-type type))]))

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
				 [(<tuple>? t) (make-<tuple> (map inst (<tuple>-list t)))]
				 [(tlist? t) (make-tlist (inst (tlist-type t)))]
				 [(tvar? t) (if (null? (unbox (tvar-tbox t)))
						(instVar (tvar-tbox t) tm)
						(inst (unbox (tvar-tbox t))))]
				 [(option? t) (make-option (inst (option-type t)))]
				 [(ref? t) (make-ref (inst (ref-type t)))]))])
		 (inst type))))
		 
	   (define (schema type env)
	     (let* ([uv (unsolved type)]
		    [ev (env-unsolved env)]
		    [uvprime (diff uv ev)])
	       (cons type uvprime)))

	   (define (unify t1 t2 syn)
	     (cond
;	      [(eq? t1 t2) #t]
	      [(tvar? t1) (unify-var t2 (tvar-tbox t1) syn)]
	      [(string? t1)
	       (cond
		[(and (string? t2) (equal? t1 t2)) #t]
		[(tvar? t2) (unify-var t1 (tvar-tbox t2) syn)]
		[else (begin (raise-syntax-error #f (format "Expected ~a but found ~a" t1 t2) syn ) #f)])]

	      [(arrow? t1)
	       (cond
		[(arrow? t2) (and (unify (car (arrow-arglist t1)) (car (arrow-arglist t2)) syn) (unify (arrow-result t1) (arrow-result t2) syn))]
		[(tvar? t2) (unify-var t1 (tvar-tbox t2) syn)]
		[else (begin (raise-syntax-error #f "Expected an arrow type" syn) #f)])]
	      [(<tuple>? t1)
	       (cond
		[(<tuple>? t2) (eval `(and ,@(map unify (<tuple>-list t1) (<tuple>-list t2) (repeat syn (length (<tuple>-list t1))))))]
		[(tvar? t2) (unify-var t1 (tvar-tbox t2) syn)]
		[else (begin (raise-syntax-error #f "Expected a <tuple> type" syn) #f)])]
	      [(tlist? t1)
	       (cond
		[(tlist? t2) (unify (tlist-type t1) (tlist-type t2) syn)]
		[(tvar? t2) (unify-var t1 (tvar-tbox t2) syn)]
		[else (begin (raise-syntax-error #f "Expected a list type" syn) #f)])]
	      [(option? t1)
	       (cond
		[(option? t2) (unify (option-type t1) (option-type t2) syn)]
		[(tvar? t2) (unify-var t1 (tvar-tbox t2) syn)]
		[else (raise-syntax-error #f "Expected an option type" syn)])]
	      [(ref? t1)
	       (cond
		[(ref? t2) (unify (ref-type t1) (ref-type t2) syn)]
		[(tvar? t2) (unify-var t1 (tvar-tbox t2) syn)]
		[else (raise-syntax-error #f "Expected an option type" syn)])]))

	   (define (unify-var type tbox syn)
	     (if (null? (unbox tbox))
		 (if (and (tvar? type) (eqv? (tvar-tbox type) tbox))
		     #t
		     (if (and (tvar? type) (not (null? (unbox (tvar-tbox type)))))
			 (unify-var (unbox (tvar-tbox type)) tbox syn)
			 (begin
			   (set-box! tbox type)
			   #t)))
		 (unify (unbox tbox) type syn)))

	   (define (lookup-ident uname syntax)
	     (match uname
		    [($ ast:lident name)
		     (let ([result (hash-table-get built-in-and-user-funcs (eval name) (lambda () #f))])
		       (if result
			   (car result)
			   (begin
			     (raise-syntax-error #f (format "Error: Unbound variable ~a" name) (if syntax
												   syntax
												   uname ))
			     #f)))]
		    [($ ast:ldot longident name)

		     (if (ast:lident? longident)
			 (let ([lib-map (hash-table-get library-names (eval (ast:lident-name longident)) (lambda () #f))])
			   (if lib-map
			       (let ([function (hash-table-get lib-map (syntax-object->datum name) (lambda () #f))])
				 (if function
				     (car function)
				     (begin
				       (raise-syntax-error #f (format "Error: ~a not found in ~a" (syntax-object->datum name) (eval (ast:lident-name longident))) uname)
				       #f)))
			       (begin
				 (raise-syntax-error #f (format "Error: ~a not found" (eval (ast:lident-name longident))) uname)
				 #f))))]))

	   (define (convert-tvars type mappings)
	     (cond
	      [(string? type) (cons type mappings)]
	      [(<tuple>? type) (let ([tlist (convert-list (<tuple>-list type) mappings null)])
			       (cons (make-<tuple> (reverse (car tlist))) (cdr tlist)))]
	      [(arrow? type) (let* ([tlist (convert-list (arrow-arglist type) mappings null)]
				    [restype (convert-tvars (arrow-result type) (cdr tlist))])
			       (cons (make-arrow (reverse (car tlist)) (car restype)) (cdr restype)))]
	      [(usertype? type) (cons (usertype-name type) mappings)]
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
	      [(option? type) (let ([mtype (convert-tvars (option-type type)  mappings)])
				(cons (make-option (car mtype)) (cdr mtype)))]
	      [(ref? type) (let ([rtype (convert-tvars (ref-type type) mappings)])
			     (cons (make-ref (car rtype)) (cdr rtype)))]
	      [else (raise-syntax-error #f "Bad type to convert!" type)]))

	   (define (unconvert-tvars type mappings)
	     (cond
	      [(tvariant? type) (cons type mappings)]
	      [(list? type) (map unconvert-tvars type (repeat null (length type)))]
	      [(string? type) (cons type mappings)]
	      [(<tuple>? type) (let ([tlist (unconvert-list (<tuple>-list type) mappings null)])
			       (cons (make-<tuple> (reverse (car tlist))) (cdr tlist)))]
	      [(arrow? type) (let* ([tlist (unconvert-list (arrow-arglist type) mappings null)]
				   [restype (unconvert-tvars (arrow-result type) (cdr tlist))])
			      (cons (make-arrow (reverse (car tlist)) (car restype)) (cdr restype)))]
	      [(tlist? type) (let ([ltype (unconvert-tvars (tlist-type type) mappings)])
			       (cons (make-tlist (car ltype)) (cdr ltype)))]
	      [(option? type) (let ([otype (unconvert-tvars (option-type type) mappings)])
				(cons (make-option (car otype)) (cdr otype)))]
	      [(ref? type) (let ([rtype (unconvert-tvars (ref-type type) mappings)])
			     (cons (make-ref (car rtype)) (cdr rtype)))]
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
					  (cons res mappings)
					  (begin
					    (set! cur-var (integer->char (+ 1 (char->integer cur-var))))
					    (let ([old-var (format "'~a" (integer->char (- (char->integer cur-var) 1)))])
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
				(raise-syntax-error #f "Non-unified type variable!" type)
				(get-arglist (unbox (tvar-tbox type))))]
	      [else (raise-syntax-error #f (format "Expected arrow but found ~a" type) type)]) )

	   (define (get-result type)
	     (cond
	      [(arrow? type) (arrow-result type)]
	      [(tvar? type) (if (null? (unbox (tvar-tbox type)))
				(raise-syntax-error #f "Non-unified type variable!" type)
				(get-result (unbox (tvar-tbox type))))]
	      [else (raise-syntax-error #f (format "Expected arrow but found ~a" type) type)]) )

	   (define (istype? typename-as-symbol rtype)
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
		    [($ ast:lident name) (eval name)]
		    [($ ast:ldot longident name) (format "~a.~a" (unlongident longident) (eval name))]))
	   

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
		 (begin (pretty-print (format "Comparing ~a and ~a" (caar context) var))
		 (if (equal? (caar context) var)
		     (cdar context)
		     (get-type var (cdr context))))))

	   (define (update var type-tvarlist context)
	     (cons (cons var type-tvarlist) context))

	   (define (empty-context) null)

	   (define (expr-location expr)
	     (if (syntax? expr)
		 expr
		 (match (ast:expression-pexp_desc expr)
			[($ ast:pexp_constant dcon) dcon]
			[($ ast:pexp_ident name) (longident-location name)]
			[($ ast:pexp_let rec pelist expr) (pat-location (caar pelist))]
			[($ ast:pexp_function label expr pelist) (pat-location (caar pelist))]
			[($ ast:pexp_apply expr lelist) (expr-location expr)]
			[($ ast:pexp_match expr pelsit) (expr-location expr)]
			[($ ast:pexp_try expr pelist) (expr-location expr)]
			[($ ast:pexp_tuple elist) (expr-location (car elist))]
			[($ ast:pexp_construct name expr bool) (longident-location name)]
			[($ ast:pexp_ifthenelse test tesp fexp) (expr-location test)]
			[_ 'waggle])))
	   
	   (define (longident-location longident)
	     (if (syntax? longident)
		 longident
		 (match longident
			[($ ast:lident name) name]
			[($ ast:ldot first name) (longident-location first)]
			[($ ast:lapply rand rator) (longident-location rand)])))

	   (define (pat-location pat)
	     (if (syntax? pat)
		 pat
		 (match (ast:expression-pexp_desc pat)
			[($ ast:ppat_any x) #`#,x]
			[($ ast:ppat_var name) name]
			[($ ast:ppat_constant const) const]
			[($ ast:ppat_tuple plist) (pat-location (car plist))]
			[($ ast:ppat_construct longident pat bool) (longident-location longident)]
			[($ ast:ppat_constraint pat type) (pat-location pat)])))


)
