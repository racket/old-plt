#cs(module typecheck mzscheme
	   (require "prims.ss"
		    (prefix ast: "ast.ss")
		    (lib "match.ss")
		    (lib "pretty.ss")
		    (lib "list.ss")
		    )
	   (provide typecheck-all convert-tvars)

	   (define cur-var #\a)

	   (define (hash-table-put!print ht key val)
	       (pretty-print (format "hash-table-put! ~a ~a ~a" ht key val))
	       (hash-table-put! ht key val))

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
		 (begin ;(pretty-print (format "initial progtype: ~a" progtype))
		 (if (list? progtype)
		     (let ([fprogtype (<flatten> progtype)])
		       (map car (map unconvert-tvars fprogtype (repeat null (length fprogtype)))))
;		     (letrec ([ucvert (lambda (ttlist)
;					(if (null? ttlist)
;					    null
;					    (if (list? (car ttlist))
;						(append (ucvert (car ttlist)) (ucvert (cdr ttlist)))
;						(cons (car (unconvert-tvars (car ttlist) null)) (ucvert (cdr ttlist))))))])
;		       (<flatten> (ucvert progtype)))
;		     (map car (map unconvert-tvars progtype (repeat null (length progtype))))
		     (list (car (unconvert-tvars progtype null))))))))
;)

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
;			   (begin ;(pretty-print (format "typechecking last item of list: ~a" v))
;				  v))
;			 (let ([f (typecheck-ml a context)]
;			       [l (typecheck-ml b context)])
;			   (begin
;			     ;(pretty-print (format "typechecking a list: ~a ~a" f l))
;			 (cons (typecheck-ml a context) (typecheck-ml b context)))))]
		    [($ ast:structure_item desc src)
		     (typecheck-structure desc src context)]
		    [else
		     (raise-syntax-error #f (format "Cannot typecheck: ~a" stmt) (at stmt (ast:make-src 1 1 1 1)))]))

	   (define (newboundtypes tdlist)
;	     (pretty-print (format "current newboundtype: ~a" (car tdlist)))
	     (if (null? tdlist)
		 null
		 (cons 
		  (cons 
		   (syntax-object->datum (car (car tdlist))) 
		   (let ([tkind (ast:type_declaration-kind (cdr (car tdlist)))])
		     (cond
		      [(ast:ptype_abstract? tkind) (syntax-object->datum (car (car tdlist)))]
		      [(ast:ptype_variant? tkind) (make-usertype (syntax-object->datum (car (car tdlist))) (car (convert-list (map make-tvar (ast:type_declaration-params (cdr (car tdlist)))) null null)))]
		      [else (raise-syntax-error "Bad kind: ~a" tkind)])))
		  (newboundtypes (cdr tdlist)))))

;		 (cons (cons (syntax-object->datum (car (car tdlist))) (make-usertype (syntax-object->datum (car (car tdlist))) (car (convert-list (map make-tvar (ast:type_declaration-params (cdr (car tdlist)))) null null)))) (newboundtypes (cdr tdlist)))))

	   (define (typecheck-structure desc src context)
;	     ;(pretty-print (format "typecheck-structure ~a" desc))
	     (match desc
		    [($ ast:pstr_value rec_flag pelist lsrc)
		     (typecheck-defines rec_flag pelist context)]
		    [($ ast:pstr_type stdlist)
		     (begin ;(pretty-print (format "Length of stdlist is ~a" (length stdlist)))
		     (map typecheck-typedecl stdlist (repeat (newboundtypes stdlist) (length stdlist))))]
		    [($ ast:pstr_eval expr lsrc)
;		     (begin (pretty-print "Typecheck-ml called for pstr_eval from typecheck-structure")
		     (typecheck-ml expr context)]
		    [($ ast:pstr_exception name decl)
		     (let ([nconst (make-tconstructor (make-<tuple> (map typecheck-type decl (repeat null (length decl)))) "exception")])
		       (begin
			 (hash-table-put! <constructors> (syntax-object->datum name) (cons (make-tconstructor 
											    (let ([args (map typecheck-type decl (repeat null (length decl)))])
											      (cond 
											       [(null? args) null]
											       [(= (length args) 1) (car args)]
											       [else (make-<tuple> args)])) "exception" ) #`#,(string->symbol (format "make-~a" (syntax-object->datum name)))))
			 (make-mlexn (syntax-object->datum name) nconst)))]
		       
		    [else
		     (raise-syntax-error #f (format "Unrecognized structure: ~a") (at desc src))]))

		     
	   (define (typecheck-expr desc context src)
	     (match desc
		    [($ ast:pexp_constant const)
		     (constant-check (syntax-object->datum const))]

		    [($ ast:pexp_tuple xlist)
		     (make-<tuple> (map typecheck-ml xlist (repeat context (length xlist))))]

		    [($ ast:pexp_array xlist)
		     (let ([xtypes (map typecheck-ml xlist (repeat context (length xlist)))])
		       (if (same-types? xtypes (map at (map ast:expression-pexp_desc xlist) (map ast:expression-pexp_src xlist)))
			   (make-tarray (car xtypes))
			   (raise-syntax-error #f "Not all same types" #f)))]

		    [($ ast:pexp_ident name)
		     ;(pretty-print (format "Looking up pexp_ident: ~a" (unlongident name)))
		     (let ([type (get-type (unlongident name) context)])
		       (begin ;(pretty-print (format "pexp_ident type: ~a" type))
		       (if type
			   (let ([itype (instantiate type)])
			     itype)
			   (let ([rtype (lookup-ident name (at desc src))])
			     (begin ;(pretty-print (format "pexp_ident rtype: ~a" rtype))
			     (if rtype
				 (let ([fres (car (convert-tvars rtype null))])
				   (begin
				     ;(pretty-print (format "pexp_ident fres: ~a" fres))
				     fres))
				 (raise-syntax-error #f (format "No type found for ~a" name) (at name src)))
			     )
)
			   ))
 )]


			   
		    [($ ast:pexp_ifthenelse test ifexp elseexp isrc tsrc esrc)
;		     (pretty-print (format "test: ~a, ifexp: ~a, elseexp: ~a" test ifexp elseexp))
		     (let* ([testt (typecheck-ml test context)]
			   [ifexpt (typecheck-ml ifexp context)]
			   [elseexpt (if (null? elseexp)
					 "unit"
					 (typecheck-ml elseexp context))]
			   [elseexpsrc (if (null? elseexp)
					   (at ifexp (ast:expression-pexp_src ifexp))
					   (at elseexp (ast:expression-pexp_src elseexp)))])
		       (if (unify "bool" testt (at test (ast:expression-pexp_src test)))
			   (if (unify ifexpt elseexpt elseexpsrc)
			       ifexpt
			       (raise-syntax-error #f (format "Expected type ~a but found ~a" ifexpt elseexpt) elseexpsrc))
			   
			   (raise-syntax-error #f "Test wasn't boolean" (at ifexp (ast:expression-pexp_src ifexp)))))]
		    
		    [($ ast:pexp_match expr pelist)
		     (let ([totest (typecheck-ml expr context)])
		       (typecheck-match pelist totest context))]
		       ;; totest needs to match the beginning of the arrow type of a test
		       ;; all of the types of test need to match
		    [($ ast:pexp_apply proc lelist)
		     (let ([funt (typecheck-ml proc context)]
			   [argst (typecheck-lelist lelist context)])
		       (typecheck-application funt argst (at proc (ast:expression-pexp_src proc)) (map at (map cdr lelist) (map ast:expression-pexp_src (map cdr lelist)))))]

		     

		    [($ ast:pexp_let rec bindings expr key-src in-src)
		     (typecheck-let rec bindings expr context)]
;		     (let ([all-bindings (typecheck-bindings rec bindings context src)])
;		       (typecheck-ml expr (append all-bindings context src)))]

		    [($ ast:pexp_function label expr pelist) 
		     (typecheck-function label expr pelist context)]

		    [($ ast:pexp_construct name expr bool)
;		     (pretty-print (format "name: ~a" (unlongident name)))
		     (let ([constructor (constructor-lookup (unlongident name))])
		       (if constructor
			   (if (tconstructor? constructor)
			       (if (null? (tconstructor-argtype constructor))
				   (if (null? expr)
				       (tconstructor-result constructor)
				       (raise-syntax-error #f "Wrong number of arguments for constructor" (at expr (ast:expression-pexp_src expr))))
				   (begin
				     ;(pretty-print (format "tconstructor-argtype is: ~a" (tconstructor-argtype constructor)))
				   (if (unify (tconstructor-argtype constructor) (typecheck-ml expr context) (at expr (ast:expression-pexp_src expr)))
				       (tconstructor-result constructor)
				       )
				   )
				   )
			       constructor)
			   (raise-syntax-error #f (format "Constructor not found: ~a" (unlongident name) ) name)))]
			   
		    [($ ast:pexp_try tryexp pelist)
		     (let ([tryt (typecheck-ml tryexp context)]
			   [exnt (typecheck-match pelist "exception" context)])
		       (if (unify tryt exnt (at pelist src))
			   tryt))]

		    [($ ast:pexp_while testexpr bodyexpr)
		     (let ([testt (typecheck-ml testexpr context)]
			   [bodyt (typecheck-ml bodyexpr context)])
		       (if (and (unify "bool" testt (at testexpr (ast:expression-pexp_src testexpr))) (unify "unit" bodyt (at bodyexpr (ast:expression-pexp_src bodyexpr))))
			   "unit"))]

		    [($ ast:pexp_for var init test up body)
		     (let ([initt (typecheck-ml init context)]
			   [testt (typecheck-ml test context)])
		       (if (and (unify "int" initt (at init (ast:expression-pexp_src init))) (unify "int" testt (at test (ast:expression-pexp_src test))))
			   (if (unify "unit" 
                                      (typecheck-ml body (update (syntax-object->datum var) (cons "int" null) context))
                                      (at body (ast:expression-pexp_src body)))
			       "unit")))]
		    [($ ast:pexp_sequence firstexpr restexpr)
		     (let ([firstt (typecheck-ml firstexpr context)]
			   [restt (typecheck-ml restexpr context)])
		       ;; Warn if type of firstt is not unit
		       restt)]
		    [else
		     (raise-syntax-error #f (format "Cannot typecheck expression: ~a" desc) (at desc src))]) )



		       
	   (define (typecheck-typedecl td boundlist)
	     (let ([name (syntax-object->datum (car td))]
		   [typedecl (cdr td)])
	       (match (ast:type_declaration-kind typedecl)
		      [($ ast:ptype_abstract dummy)
		       (let ([rtype (typecheck-type (ast:type_declaration-manifest typedecl) boundlist)])
			 (begin
			   (hash-table-put! <constructors> name (cons rtype "some error"))
			   (format "type ~a ~a = ~a" (if (not (null? (ast:type_declaration-params typedecl)))
							 (map typecheck-type (ast:type_declaration-params typedecl))
							 "") name rtype)))]
		      [($ ast:ptype_variant scll)
		       (let* ([name-type (make-usertype name (map make-tvar (map syntax-object->datum (ast:type_declaration-params typedecl))))]
			      [tscll (typecheck-scll name-type (ast:type_declaration-params typedecl) scll boundlist)]
			      [ntv (make-tvariant name (map syntax-object->datum (map car scll)) tscll)])
			 (begin
			   (hash-table-put! <constructors> name (cons name-type "some error"))
			   ntv))]
		      [else
		       (raise-syntax-error #f (format "Unkown typedecl found: ~a" td))]
		       )))

	   (define (create-mapping params mapping)
	     (if (null? params)
		 mapping
		 (if (get-type (syntax-object->datum (car params)) mapping)
		     (create-mapping (cdr params) mapping)
		     (create-mapping (cdr params) (cons (cons (syntax-object->datum (car params)) (fresh-type-var)) mapping)))))

	   (define (convert-param mapping current)
	     (let* ([name (car current)]
		    [ttypes (map car (map convert-ttypes (map cons (cdr current) (repeat mapping (length (cdr current))))))])
	       (cons name ttypes)))

	   (define (convert-ttypes type-mapping)
	     (match (ast:core_type-desc (car type-mapping))
		    [($ ast:ptyp_arrow label ct1 ct2)
		     (let* ([convert1 (convert-ttypes (cons ct1 (cdr type-mapping)))]
			    [convert2 (convert-ttypes (cons ct2 (cdr convert1)))])
		       (cons (ast:make-core_type (ast:make-ptyp_arrow label (car convert1) (car convert2)) (ast:core_type-src (car type-mapping))) (cdr convert2)))]
		     [($ ast:ptyp_tuple ctlist)
		      (let ([finallist (foldl (lambda (n r)
					       (let ([cres (convert-ttypes (cons n (cdr r)))])
						 (cons (append (car r) (list (car cres))) (cdr cres)))) 
					      (cons null (cdr type-mapping)) 
					      ctlist)])
			(cons (ast:make-core_type (ast:make-ptyp_tuple (car finallist)) (ast:core_type-src (car type-mapping))) (cdr finallist)))]
		     [($ ast:ptyp_constr name ctlist)
		      (let ([finallist (foldl (lambda (n r)
					       (let ([cres (convert-ttypes (cons n (cdr r)))])
						 (cons (append (car r) (list (car cres))) (cdr cres)))) 
					      (cons null (cdr type-mapping)) 
					      ctlist)])
		      (cons (ast:make-core_type (ast:make-ptyp_constr name (car finallist)) (ast:core_type-src (car type-mapping))) (cdr finallist)))]
		     [($ ast:ptyp_var f)
		      (let* ([varname
			      (cond
			       [(syntax? f) (syntax-object->datum f)]
			       [(string? f) f]
			       [else (raise-syntax-error #f (format "Bad ptyp_var: ~a" f))])]
			     [result (get-type varname (cdr type-mapping))])
			(if result
			    (cons (ast:make-core_type (ast:make-ptyp_var result) (ast:core_type-src (car type-mapping))) (cdr type-mapping))
			    (let ([ntb (tvar-tbox (fresh-type-var))])
			       (cons (ast:make-core_type (ast:make-ptyp_var ntb) (ast:core_type-src (car type-mapping))) (update varname ntb (cdr type-mapping))))))]
		     [x (cons (ast:make-core_type x (ast:core_type-src (car type-mapping))) (cdr type-mapping))]))
	       

		    
	   (define (typecheck-scll sname params scll boundlist)
	     (if (null? scll)
		 null
		 (let* ([current (car scll)]
			[name (syntax-object->datum (car current))]
			[sname-type sname]
			[ttypes (map typecheck-type (cdr current) (repeat boundlist (length (cdr current))))]
)
		   (begin
		     (hash-table-put! <constructors> name (cond
							   [(> (length ttypes) 1)
							    (cons (make-tconstructor (make-<tuple> ttypes) sname-type)
								  (string->symbol (format "make-~a" name)))]
							   [(= (length ttypes) 1)
							    (cons (make-tconstructor (car ttypes) sname-type)
								  (string->symbol (format "make-~a" name)))]
							   [(= (length ttypes) 0)
							    (cons (make-tconstructor null sname-type)
								  (string->symbol (format "make-~a" name)))]))
		     (cons (cond
			    [(> (length ttypes) 1)
			     (make-tconstructor (make-<tuple> ttypes) sname-type)]
			    [(= (length ttypes) 1)
			     (make-tconstructor (car ttypes) sname-type)]
			    [(= (length ttypes) 0)
			     sname-type]) (typecheck-scll sname params (cdr scll) boundlist))))))

	   (define (typecheck-match pelist testt context)
	     (let* ([patenvs (map patcheck (repeat context (length pelist)) (repeat testt (length pelist)) (map car pelist))]
		    [expts (letrec ([typecheckem (lambda (exp envs)
						   (if (null? exp)
						       null
						       (cons (let ([result (typecheck-ml (car exp) (car envs))])
							       result)
							     (typecheckem (cdr exp) (cdr envs)))))])
			     (typecheckem (map cdr pelist) patenvs))])
	       (if (same-types? expts (map at (map cdr pelist) (map ast:expression-pexp_src (map cdr pelist))))
;; Should also be checking that the patterns are exhaustive for t and don't overlap
		   (car expts)
		   (raise-syntax-error #f "Not all same types" #f))))

	   (define (typecheck-defines rec bindings context)
	     ;(pretty-print "typecheckdefines called")
	     (let ([contextwithbindings (typecheck-bindings rec bindings context)])
	       (begin 
;		 (pretty-print (format "contextwithbindings: ~a" contextwithbindings))
		 (set! cur-var #\a)
		 (let ([definedtypes (reverse (map car (map unconvert-tvars (map car (map cdr contextwithbindings)) (repeat null (length contextwithbindings)))))])
		   (begin
		     (for-each (lambda (name type)
				 (when name
				     (hash-table-put! built-in-and-user-funcs name (cons type (string->symbol name)))))
			       (reverse (map car contextwithbindings))
			       definedtypes)
		     (map make<maybe>-value-set (reverse (map car contextwithbindings)) definedtypes))))))

	   (define (make<maybe>-value-set name type)
	     (if name
		 (make-value-set name type)
		 type))

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
							   (typecheck-type (car (convert-ttypes (ast:ppat_constraint-type (ast:pattern-ppat_desc (car cur-bind))))) null)
							   (make-arrow (list (fresh-type-var)) (fresh-type-var)))])
					      (update (syntax-object->datum (ast:ppat_var-name (ast:pattern-ppat_desc rpat))) (cons tf null) (bind-vars (cdr bindings))))))])
			  (let ([cprime (bind-vars bindings)])
			    (foldl (lambda (next init)
				     (update (car next) (cdr next) init))
				   context
				   (map (lambda (mapping)
					  (cons (car mapping) (schema (cdr mapping) context)))
					(map typecheck-binding bindings (repeat rec (length bindings)) (repeat cprime (length bindings)))))))

		 (let ([ncontexts (map typecheck-binding bindings (repeat rec (length bindings)) (repeat context (length bindings)))])
		   
		   (let ([res (foldl union-envs (car ncontexts) (cdr ncontexts))])
		     (begin
		       ;(pretty-print (format "foldl result: ~a" res))
		       res))

)))

	   (define (typecheck-binding binding rec context)
;	     (pretty-print (format "typecheck-binding"))
	     (let* ([rpat (if (ast:ppat_constraint? (ast:pattern-ppat_desc (car binding)))
			      (ast:ppat_constraint-pat (ast:pattern-ppat_desc (car binding)))
			      (car binding))])
	       (if rec
		   (if (and (ast:pexp_function? (ast:expression-pexp_desc (cdr binding)))
			    (ast:ppat_var? (ast:pattern-ppat_desc rpat)))
		       (let ([te (typecheck-ml (cdr binding) context)]
			     [tf (car (get-type (syntax-object->datum (ast:ppat_var-name (ast:pattern-ppat_desc rpat))) context))])
;			 (begin (pretty-print (format "te: ~a, tf: ~a" te tf))
			 (if (unify tf te (at (cdr binding) (ast:expression-pexp_src (cdr binding))))
			     (cons (syntax-object->datum (ast:ppat_var-name (ast:pattern-ppat_desc rpat))) tf))
			 )
		       (raise-syntax-error #f "This kind of expression is not allowed as right-hand side of 'let rec'" (at (cdr binding) (ast:expression-pexp_src (cdr binding)))))
		   (begin ;(pretty-print "not rec in typecheck-binding")
		   (let* ([btype (typecheck-ml (cdr binding) context)]
			  [newcont
			  (patcheck context btype rpat)])
;		     (pretty-print (format "btype is: ~a, newtype is: ~a" btype newcont))
		     (if (null? newcont)
		       ;(pretty-print (format "patcheck result in typecheck-binding ~a" newcont))
			 `(,(cons #f (cons btype #f)))
		       newcont))))))

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
		    [expts (begin ;(pretty-print (format "funvarenvs in typecheck-function: ~a" funvarenvs))
				  ;(pretty-print (format "env after funvarenv ~a" (cdr (car funvarenvs))))
				  (map typecheck-ml (map cdr pelist) (map cdr funvarenvs)))])
	     
	       (if (and (same-types? (map car funvarenvs) (map grab-syntax pelist)) (same-types? expts (map at (map cdr pelist) (map ast:expression-pexp_src (map cdr pelist)))))
;; Should also be checking that the patterns are exhaustive for t and don't overlap
;		   (begin (pretty-print (format "argument: ~a, result: ~a" (car (car funvarenvs)) (car expts)))
		   (make-arrow (list (car (car funvarenvs))) (car expts))
		   (raise-syntax-error #f "Not all same types" #f)))
	     )

	   (define (grab-syntax pe)
	     (at (car pe) (ast:pattern-ppat_src (car pe))))



	   (define (typecheck-type asttype boundlist)
;	     (pretty-print (format "typecheck-type ~a ~a" asttype boundlist))
	     (match (ast:core_type-desc asttype)
		    [($ ast:ptyp_any dummy)
		     (make-tvar (box null))]
		    [($ ast:ptyp_var f)
		     (make-tvar (cond 
				 [(syntax? f)  (syntax-object->datum f)]
				 [(string? f) f]
				 [(box? f) f]
				 [else (raise-syntax-error #f (format "Bad ptyp_var: ~a" f))]))]
		    [($ ast:ptyp_arrow label ct1 ct2)
		     (make-arrow (list (typecheck-type ct1 boundlist)) (typecheck-type ct2 boundlist))]
		    [($ ast:ptyp_tuple ctlist)
		     (make-<tuple> (map typecheck-type ctlist (repeat boundlist (length ctlist))))]
		    [($ ast:ptyp_constr name ctlist)
;		     (pretty-print (format "ptyp_constr ~a ~a" (unlongident name) ctlist))
		     (let* ([cexists (get-type (unlongident name) boundlist)]
			    [fconstructor (if cexists
					      (car (convert-tvars cexists null))
					      (constructor-lookup (unlongident name)))]
;			    [foobar (pretty-print (format "constructor-lookup: ~a" (unlongident name)))]
			    )
			   
		       (if fconstructor
			   (cond
			    [(tconstructor? fconstructor)
			     (if (null? (tconstructor-argtype fconstructor))
				 (if (null? ctlist)
				     (tconstructor-result fconstructor)
				     (raise-syntax-error #f (format "Constructor expects no arguments but got:~a" ctlist) (at asttype (ast:core_type-src asttype))) )
				 (if (unify (tconstructor-argtype) (typecheck-type (car ctlist) boundlist) (at (car ctlist) (ast:core_type-src (car ctlist))))
				     (tconstructor-result fconstructor)))]
				       

			    [(usertype? fconstructor)
			     (if (null? (usertype-params fconstructor))
				 (if (null? ctlist)
				     fconstructor
				     (raise-syntax-error #f (format "Constructor ~a(~a) expects no arguments but got:~a" (usertype-name fconstructor) (usertype-params fconstructor) ctlist) (at asttype (ast:core_type-src asttype))))
				 (if (andmap unify (usertype-params fconstructor) (car (convert-list (map typecheck-type ctlist (repeat boundlist (length ctlist))) null null)) (repeat (at (car ctlist) (ast:core_type-src (car ctlist))) (length ctlist)))
				     fconstructor))]
			    [(equal? (unlongident name) "list")
			       
			       (make-tlist (typecheck-type (car ctlist) boundlist))]
			    [(equal? (unlongident name) "ref")
			     (make-ref (typecheck-type (car ctlist) boundlist))]
			    [(equal? (unlongident name) "array")
			     (make-tarray (typecheck-type (car ctlist) boundlist))]
			    [(null? ctlist)
			     fconstructor
			       ]
			    [else
			     (raise-syntax-error #f (format "~a takes no arguments but was given ~a" fconstructor  ctlist) (at asttype (ast:core_type-src asttype)))])
			   (raise-syntax-error #f (format "Unknown constructor: ~a. Boundlist: ~a" (unlongident name) boundlist) (at asttype (ast:core_type-src asttype)))))]
		    [($ ast:ptyp_variant rfl abool ll)
		     'comeslater
		     ;; Need to implement this!!!
]
		    [else
		     (raise-syntax-error #f (format "Cannot handle type: ~a" asttype) (at asttype (ast:core_type-src asttype)))]))

		    

	   (define (typecheck-application funt argst funsyn argssyn)
;	     (pretty-print (format "typecheck-application ~a ~a" funt argst))
	     (if (unify (make-arrow (list (fresh-type-var)) (fresh-type-var)) funt funsyn)
		 (let ([arglist (get-arglist funt)])
		   (if (= (length arglist) (length argst))
		       (begin
;			 (pretty-print (format "arglist: ~a argst: ~a" arglist argst))
		       (if (andmap unify arglist argst argssyn)
			   (get-result funt)
			   (raise-syntax-error #f "Arguments do not match types" #f))
		       )
		       (if (> (length argst) (length arglist))
			   (foldl uncurry (if (unify (car (get-arglist funt)) (car argst) (car argssyn))
					      (get-result funt)) 
				  (cdr argst)
				  (cdr argssyn))
			   (raise-syntax-error #f (format "Expected ~a arguments but found ~a" (length arglist) (length argst)) funsyn))))
		 (raise-syntax-error #f "Failed to unify application" #f)) )


	   (define (uncurry nextt argsyn init)
;	     (pretty-print (format "uncurry: ~a ~a ~a" nextt argsyn init))
	     (if (unify init (make-arrow (list nextt) (fresh-type-var)) argsyn)
		 (let ([arglist (get-arglist init)])
		   (if (> (length arglist) 1)
		       (raise-syntax-error #f (format "This should never happen: uncurry") nextt)
;		       (if (unify (car arglist) nextt)
		       (get-result init)))))

		    
	   (define (funvarcheck context pat)
	     (if (unique-var pat null)
		 (let ([varenv (funenv pat)])
		   (begin ;(pretty-print (format "var-env from funvarcheck: ~a" varenv))
			  (cons (car varenv) (union-envs (cdr varenv) context))))))

	   (define (patcheck context type pat) 
	     ;(pretty-print (format "patcheck: ~a" pat))
	     (if (unique-var pat null)
		 (union-envs (patenv pat type context) context)
		 'fuzzle))

	   (define (unique-var pat curvars)
	     ;(pretty-print (format "unique-var: ~a" pat))
	     (match (ast:pattern-ppat_desc pat)
		    [($ ast:ppat_any dummy)
		     curvars]
		    [($ ast:ppat_var name)
		     (if (present? name curvars)
			 (raise-syntax-error #f (format "This variable is bound several times in this matching: ~a" name) (at pat (ast:pattern-ppat_src pat)))
			 (cons name curvars))]
		    [($ ast:ppat_constant const)
		     curvars]
		    [($ ast:ppat_tuple plist)
		     (unique-var-list plist curvars)]
		    [($ ast:ppat_construct longident dpat bool)
		     (if (null? dpat)
			 curvars
			 (unique-var dpat curvars))]
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
		       (cons t (update (syntax-object->datum name) ts (empty-context))))]
		    [($ ast:ppat_constant const)
;		     (begin (pretty-print (format "const is ~a" const))
		     (cons (constant-check const) (empty-context))
		     
		     ]
		    [($ ast:ppat_tuple plist)
		     (let ([varenvs (map funenv plist)])
		       (begin ;(pretty-print (format "varenvs from funenv: ~a" varenvs))
		       (cons (make-<tuple> (map car varenvs)) (foldl union-envs (car (map cdr varenvs)) (cdr (map cdr varenvs))))))]
		    [($ ast:ppat_construct longident cpat bool)
		       (let ([cpat-type (constructor-lookup (unlongident longident))]) 
			 (if cpat-type
			     (if (tconstructor? cpat-type)
				 (if (null? cpat)
				     (if (null? (tconstructor-argtype cpat-type))
					 (cons (tconstructor-result cpat-type) (empty-context))
					 (raise-syntax-error #f "Constructor expects an argument" (at pat (ast:pattern-ppat_src pat))))
				     (if (null? (tconstructor-argtype cpat-type))
					 (raise-syntax-error #f "Constructor expects no arguments" (at pat (ast:pattern-ppat_src pat)))
					 (cons (tconstructor-result cpat-type) (let ([fenvpat (funenv cpat)])
										 (when (unify (tconstructor-argtype cpat-type) (car fenvpat) (at cpat (ast:pattern-ppat_src cpat)))
										       (cdr fenvpat))))))
				 (if (null? cpat)
				     (cons cpat-type (empty-context))
				     (raise-syntax-error #f "Constructor expects no arguments" (at pat (ast:pattern-ppat_src pat)))))
			     (raise-syntax-error #f (format "No such constructor ~a" (unlongident longident)) (at pat (ast:pattern-ppat_src pat)))) )]

		    [($ ast:ppat_constraint pat ct)
		     ;Assume a constraint must immediately follow a variable
		     (let ([type (typecheck-type (car (convert-ttypes (cons ct null))) null)]
			   [varname (syntax-object->datum (ast:ppat_var-name (ast:pattern-ppat_desc pat)))])
		       (cons type (update varname (cons type null) (empty-context))))]
		    [else (raise-syntax-error #f (format "No such pattern found: ~a" pat) (at pat (ast:pattern-ppat_src pat)))]))
		     
	   (define (patenv pat type context)
	     (let ([patsyn (at pat (ast:pattern-ppat_src pat))])
	     (match (ast:pattern-ppat_desc pat)
		    [($ ast:ppat_any dummy)
		     (empty-context)]
		    [($ ast:ppat_var name)
		     (update (syntax-object->datum name) (schema type context) (empty-context))]
		    [($ ast:ppat_constant const)
		     (let ([t2 (constant-check const)])
		       (if (unify type t2 patsyn)
			   (empty-context)
			   (raise-syntax-error #f (format "Expected ~a but found ~a (patenv)" type t2) patsyn))) ]
		    [($ ast:ppat_tuple plist)
		     (begin ;(pretty-print (format "plist: ~a" plist))
			    
			    (when (unify type (make-<tuple> (letrec 
							      ([ftvarlist (lambda (len)
										(if (= len 0)
								
										    null
										    (cons (fresh-type-var) (ftvarlist (- len 1)))))])
							    (ftvarlist (length plist))))
				       (at (car plist) (ast:pattern-ppat_src (car plist))))
				(let* ([tlist (letrec ([get-list
							(lambda (type)
							  (cond
							   [(tvar? type)
							    (get-list (unbox (tvar-tbox type)))]
							   [(<tuple>? type)
							    (<tuple>-list type)]
							   [else (raise-syntax-error #f (format "Not a list: ~a" type) #f)]))]) (get-list type))]
				       [nenvs (map patenv plist tlist (repeat context (length plist)))])
				  (let ([res (foldl union-envs (car nenvs) (cdr nenvs))])
				    (begin
					;(pretty-print (format "foldl result2: ~a" res))
				      res)))
))
		     
		     ]
		    [($ ast:ppat_construct longident cpat bool)
		     (let ([cpat-type (constructor-lookup (unlongident longident))]) 
		       (if cpat-type
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
				       (raise-syntax-error #f "Constructor expects no arguments" patsyn))))
			   (raise-syntax-error #f (format "No such constructor ~a" (unlongident longident)) patsyn)) )]
		    [($ ast:ppat_constraint pat ct)
		     (if (unify type (typecheck-type ct null) patsyn)
			 (update (syntax-object->datum (ast:ppat_var-name (ast:pattern-ppat_desc pat))) (schema type context) (empty-context)))]
		    [else
		     (raise-syntax-error #f "Pattern not recognized" patsyn)])))

	     
	   (define (same-types? tlist synlist)
;	     (pretty-print (format "same-types?: ~a" tlist))
	     (if (= (length tlist) 1)
		 #t
		 (let ([tocompare (cdr tlist)])
		   (and (andmap unify (repeat (car tlist) (length tocompare)) tocompare (cdr synlist)) (same-types? tocompare (cdr synlist))))))

	   (define (union tvl1 tvl2)
;	     (pretty-print (format "union: ~a ~a" tvl1 tvl2))
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
	      [(tarray? type) (unsolved (tarray-type type))]
	      [(tvar? type) (let ([r (tvar-tbox type)])
				  (if (null? (unbox r))
				      (list r)
				      (unsolved (unbox r))))]
	      [(option? type) (unsolved (option-type type))]
	      [(ref? type) (unsolved (ref-type type))]
	      [(usertype? type) (if (null? (usertype-params type))
				    null
				    (foldl (lambda (ut sl)
					     (union (unsolved ut) sl)) (unsolved (car (usertype-params type))) (cdr (usertype-params type))))]
	      [else (raise-syntax-error #f (format "Bad type: ~a" type))]))

	   (define (env-unsolved r)
;	     (pretty-print (format "env-unsolved: ~a" r))
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
				 [(tarray? t) (make-tarray (inst (tarray-type t)))]
				 [(tvar? t) (if (null? (unbox (tvar-tbox t)))
						(instVar (tvar-tbox t) tm)
						(inst (unbox (tvar-tbox t))))]
				 [(option? t) (make-option (inst (option-type t)))]
				 [(usertype? t) (make-usertype (usertype-name t) (map inst (usertype-params t)))]
				 [(ref? t) (make-ref (inst (ref-type t)))]))])
		 (inst type))))
		 
	   (define (schema type env)
	     (let* ([uv (unsolved type)]
		    [ev (env-unsolved env)]
		    [uvprime (diff uv ev)])
	       (cons type uvprime)))

	   (define (unify t1a t2a syn)
;	     (pretty-print (format "unify ~a ~a" t1a t2a))
	     (let* ([t1 (if (and (string? t1a) (constructor-lookup t1a))
			   (constructor-lookup t1a)
			   t1a)]
		    [t2 (if (and (string? t2a) (not (string? t1)) (constructor-lookup t2a))
			   (constructor-lookup t2a)
			   t2a)])
	       (cond
;	      [(eq? t1 t2) #t]
		[(tvar? t1) (unify-var t2 (tvar-tbox t1) syn)]
		[(string? t1)
		 (cond
		  [(string? t2) 
		   (if (equal? t1 t2) 
		       #t
		       (let* ([t1c (constructor-lookup t1)]
			      [t2c (constructor-lookup t2)]
			      [t1a (if t1c t1c t1)]
			      [t2a (if t2c t2c t2)])
			 (if (equal? t1a t2a)
			     #t
			     (raise-syntax-error #f (format "Expected ~a but found ~a" t1a t2a) syn))))]
;		[(usertype? t2) (equal? t1 (usertype-name t2))]
		  [(tvar? t2) (unify-var t1 (tvar-tbox t2) syn)]
		  [else (begin (raise-syntax-error #f (format "Expected ~a but found ~a" (print-type t1) (print-type t2)) syn ) #f)])]
		
		[(arrow? t1)
		 (cond
		  [(arrow? t2) (and (unify (car (arrow-arglist t1)) (car (arrow-arglist t2)) syn) (unify (arrow-result t1) (arrow-result t2) syn))]
		  [(tvar? t2) (unify-var t1 (tvar-tbox t2) syn)]
		  [else (begin (raise-syntax-error #f (format "Expected ~a -> ~a but found ~a" (car (arrow-arglist t1)) (arrow-result t1) t2) syn) #f)])]
		[(<tuple>? t1)
		 (cond
		  [(<tuple>? t2) 
		   (if (= (length (<tuple>-list t1)) (length (<tuple>-list t2)))
		       (andmap unify (<tuple>-list t1) (<tuple>-list t2) (repeat syn (length (<tuple>-list t1))))
		       (begin (raise-syntax-error #f (format "Expected ~a but found ~a" t1 t2) syn) #f))]
		  [(tvar? t2) (unify-var t1 (tvar-tbox t2) syn)]
		  [else (begin (raise-syntax-error #f "Expected a <tuple> type" syn) #f)])]
		[(tlist? t1)
		 (cond
		  [(tlist? t2) (unify (tlist-type t1) (tlist-type t2) syn)]
		  [(tvar? t2) (unify-var t1 (tvar-tbox t2) syn)]
		  [else (begin (raise-syntax-error #f "Expected a list type" syn) #f)])]
		[(tarray? t1)
		 (cond
		  [(tarray? t2) (unify (tarray-type t1) (tarray-type t2) syn)]
		  [(tvar? t2) (unify-var t1 (tvar-tbox t2) syn)]
		  [else (raise-syntax-error #f "Expected an array type" syn)])]
		[(option? t1)
		 (cond
		  [(option? t2) (unify (option-type t1) (option-type t2) syn)]
		  [(tvar? t2) (unify-var t1 (tvar-tbox t2) syn)]
		  [else (raise-syntax-error #f "Expected an option type" syn)])]
		[(ref? t1)
		 (cond
		  [(ref? t2) (unify (ref-type t1) (ref-type t2) syn)]
		  [(tvar? t2) (unify-var t1 (tvar-tbox t2) syn)]
		  [else (raise-syntax-error #f "Expected an option type" syn)])]
		[(usertype? t1)
		 (cond
		  [(usertype? t2) (if (equal? (usertype-name t1) (usertype-name t2))
				      (if (= (length (usertype-params t1)) (length (usertype-params t2)))
					  (andmap unify (usertype-params t1) (usertype-params t2) (repeat syn (length (usertype-params t1))))
					  (raise-syntax-error #f (format "Expected ~a with ~a parameters but found it with ~a parameters" (usertype-name t1) (length (usertype-params t1)) (length (usertype-params t2))) syn))
				      (raise-syntax-error #f (format "Expected ~a(~a) but found ~a(~a)" (usertype-name t1) (length (usertype-params t1)) (usertype-name t2) (length (usertype-params t2))) syn))]
		  [(tvar? t2) (unify-var t1 (tvar-tbox t2) syn)]
		  [else (raise-syntax-error #f (format "Expected ~a but found ~a (usertype doesn't match usertype)" (print-type t1) (print-type t2)) syn)])]
		[else (raise-syntax-error #f (format "Bad type to unify ~a" t1) syn)])))

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
		     (let* ([rname (cond
				    [(syntax? name) (syntax-object->datum name)]
				    [(string? name) name])]
			    [result (hash-table-get built-in-and-user-funcs rname (lambda () #f))])
		       (if result
			   (car result)
			   (begin
;			     (+ 1 #f)
			     (raise-syntax-error #f (format "Error: Unbound variable ~a" rname) (if syntax
												   syntax
												   uname ))
			     #f)))]
		    [($ ast:ldot longident name)

		     (if (ast:lident? longident)
			 (let ([lib-map (hash-table-get <library-names> (syntax-object->datum (ast:lident-name longident)) (lambda () #f))])
			   (if lib-map
			       (let ([function (hash-table-get lib-map (syntax-object->datum name) (lambda () #f))])
				 (if function
				     (car function)
				     (begin
				       (raise-syntax-error #f (format "Error: ~a not found in ~a" (syntax-object->datum name) (syntax-object->datum (ast:lident-name longident))) uname)
				       (if syntax syntax uname))))
			       (begin
				 (raise-syntax-error #f (format "Error: Library ~a not found" (syntax-object->datum (ast:lident-name longident))) (ast:lident-name longident))

				 (if syntax syntax uname)))))]))

	   (define (convert-tvars type mappings)
	     (cond
	      [(string? type) (cons type mappings)]
	      [(<tuple>? type) (let ([tlist (convert-list (<tuple>-list type) mappings null)])
			       (cons (make-<tuple> (reverse (car tlist))) (cdr tlist)))]
	      [(arrow? type) (let* ([tlist (convert-list (arrow-arglist type) mappings null)]
				    [restype (convert-tvars (arrow-result type) (cdr tlist))])
			       (cons (make-arrow (reverse (car tlist)) (car restype)) (cdr restype)))]
	      [(usertype? type) (let ([tlist (convert-list (usertype-params type) mappings null)])
				  (cons (make-usertype (usertype-name type) (reverse (car tlist))) (cdr tlist)))]
	      [(tconstructor? type) (let* ([argtype (if (null? (tconstructor-argtype type))
							(cons null mappings)
							(convert-tvars (tconstructor-argtype type) mappings))]
					   [restype (convert-tvars (tconstructor-result type) (cdr argtype))])
				      (cons (make-tconstructor (car argtype) (car restype)) (cdr restype)))]
					   
	      [(tlist? type) (let ([ltype (convert-tvars (tlist-type type) mappings)])
			       (cons (make-tlist (car ltype)) (cdr ltype)))]
	      [(tarray? type) (let ([atype (convert-tvars (tarray-type type) mappings)])
				(cons (make-tarray (car atype)) (cdr atype)))]
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
	      [(value-set? type) (let ([nmap (unconvert-tvars (value-set-type type) mappings)])
				   (cons (make-value-set (value-set-name type) (car nmap)) (cdr nmap)))]
	      [(tvariant? type) (cons type mappings)]
	      [(usertype? type) (let ([tlist (unconvert-list (usertype-params type) mappings null)])
				  (cons (make-usertype (usertype-name type) (reverse (car tlist))) (cdr tlist)))]
	      [(list? type) (map unconvert-tvars type (repeat null (length type)))]
	      [(string? type) (cons type mappings)]
	      [(<tuple>? type) (let ([tlist (unconvert-list (<tuple>-list type) mappings null)])
			       (cons (make-<tuple> (reverse (car tlist))) (cdr tlist)))]
	      [(arrow? type) (let* ([tlist (unconvert-list (arrow-arglist type) mappings null)]
				   [restype (unconvert-tvars (arrow-result type) (cdr tlist))])
			      (cons (make-arrow (reverse (car tlist)) (car restype)) (cdr restype)))]
	      [(tlist? type) (let ([ltype (unconvert-tvars (tlist-type type) mappings)])
			       (cons (make-tlist (car ltype)) (cdr ltype)))]
	      [(tarray? type) (let ([atype (unconvert-tvars (tarray-type type) mappings)])
				(cons (make-tarray (car atype)) (cdr atype)))]
	      [(option? type) (let ([otype (unconvert-tvars (option-type type) mappings)])
				(cons (make-option (car otype)) (cdr otype)))]
	      [(ref? type) (let ([rtype (unconvert-tvars (ref-type type) mappings)])
			     (cons (make-ref (car rtype)) (cdr rtype)))]
	      [(mlexn? type) (let ([newtypes (if (null? (tconstructor-argtype (mlexn-types type)))
						(cons null mappings)
						(unconvert-tvars (tconstructor-argtype (mlexn-types type)) mappings))])
			       (cons (make-mlexn (mlexn-name type) (make-tconstructor (car newtypes) (tconstructor-result (mlexn-types type)))) (cdr newtypes)))]
	      [(tvar? type) (let ([dbox (tvar-tbox type)])
			      (cond
			       [(string? dbox) (cons type mappings)]
			       [(null? (unbox dbox))
				(letrec ([tfunc (lambda (maplist)
						  (if (null? maplist)
						      #f
						      (if (eqv? (caar maplist) dbox)
							  (cdar maplist)
							  (tfunc (cdr maplist)))))])
				  (let ([res (tfunc mappings)])
				    (if res
					  (cons (make-tvar res) mappings)
					  (begin
					    (set! cur-var (integer->char (+ 1 (char->integer cur-var))))
					    (let ([old-var (format "'~a" (integer->char (- (char->integer cur-var) 1)))])
					      (cons (make-tvar old-var) (cons (cons dbox old-var) mappings)))))))]
			       [else
				  (unconvert-tvars (unbox dbox) mappings)]))]
	      [else ;(cons type mappings)]))
	       (raise-syntax-error #f (format "Bad type: ~a" type) type)]))
	   
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
;	     (pretty-print (format "constant-check ~a" const))
	     (cond
	      [(syntax? const) (constant-check (syntax-object->datum const))]
	      [(and (number? const) (exact? const)) "int"]
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

	   (define (istype? pred rtype)
	     (cond
	      [(pred rtype) #t]
	      [(tvar? rtype) (if (null? (unbox (tvar-tbox rtype)))
				 #t
				 (istype? pred (unbox (tvar-tbox rtype))))]
	      [else #f]))

;	   (define (istype? typename-as-symbol rtype)
;	     (let ([pred (datum->syntax-object (syntax rtype) (string->symbol (format "~a?" typename-as-symbol)))])
;	       (eval #`(cond
;			[(#,pred #,rtype) #t]
;			[(tvar? #,rtype) (if (null? (unbox (tvar-tbox #,rtype)))
;					  (begin ; ;(pretty-print "istype?: ununified type-var") 
;					    #f)
;					  (istype? #,typename-as-symbol (unbox (tvar-tbox #,rtype))))]
;			[else #f]))))
	     
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
;							    ;(pretty-print "Non-unified type variable: istype?")
;							    (istype? tname (unbox (tvar-tbox type))))]
;					  [else ;(pretty-print (list "Expected" tname "but found" type))]))))))


	   
	   (define (unlongident uname)
	     (match uname
		    [($ ast:lident name) (if (syntax? name)
					     (syntax-object->datum name)
					     name)]
		    [($ ast:ldot longident name) (format "~a.~a" (unlongident longident) (if (syntax? name)
											     (syntax-object->datum name)
											     name))]))
	   

	   (define (repeat x n)
	     (if (<= n 0)
		 null
		 (cons x (repeat x (- n 1)))))

	   (define-struct tcontext (context constraints))

	   (define (union-envs c1 c2)
	     (cond
	      [(null? c1) c2]
	      [(null? c2) c1]
	      [(equal? (car c1) (car c2)) (cons (car c1) (union-envs (cdr c1) (cdr c2)))]
	      [else (cons (car c1) (union-envs (cdr c1) c2))]))

	   
	   (define (present? var vlist)
	     (if (null? vlist)
		 #f
		 (if (equal? (car vlist) var)
		     #t
		     (present? var (cdr vlist)))))

	   (define (get-type var context)
	     ;(pretty-print (format "get-type ~a ~a" var context))
	     (if (null? context)
		 #f
		 (begin ;(pretty-print (format "Comparing ~a and ~a" (caar context) var))
		 (if (equal? (caar context) var)
		     (cdar context)
		     (get-type var (cdr context))))))

	   (define (update var type-tvarlist context)
;	     (pretty-print (format "update: ~a ~a ~a" var type-tvarlist context))
	     (let ([new-context (cons (cons var type-tvarlist) context)])
	       (begin ;(pretty-print (format "Update: ~a" new-context))
		      new-context)))

	   (define (empty-context) null)

	   (define (expr-location expr)
	     (if (syntax? expr)
		 expr
		 (match (ast:expression-pexp_desc expr)
			[($ ast:pexp_constant dcon) dcon]
			[($ ast:pexp_ident name) (longident-location name)]
			[($ ast:pexp_let rec pelist expr ksrc isrc) (pat-location (caar pelist))]
			[($ ast:pexp_function label expr pelist) (pat-location (caar pelist))]
			[($ ast:pexp_apply expr lelist) (expr-location expr)]
			[($ ast:pexp_match expr pelsit) (expr-location expr)]
			[($ ast:pexp_try expr pelist) (expr-location expr)]
			[($ ast:pexp_tuple elist) (expr-location (car elist))]
			[($ ast:pexp_construct name expr bool) (longident-location name)]
			[($ ast:pexp_ifthenelse test tesp fexp isrc tsrc esrc) (expr-location test)]
			[_ (raise-syntax-error #f (format "Couldn't find expression: ~a" expr) #f)])))
	   
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

	   (define (print-type type)
	     (cond
;	      [(value-set? type) (let ([nmap (unconvert-tvars (value-set-type type) mappings)])
;				   (cons (make-value-set (value-set-name type) (car nmap)) (cdr nmap)))]
	      [(tvariant? type) (tvariant-name type)]
	      [(usertype? type) (format "~a(~a)" (usertype-name type) (length (usertype-params type)))]
	      [(list? type) (map print-type type)]
	      [(string? type) type]
	      [(<tuple>? type) (format "tuple of ~a" (print-type (<tuple>-list type)))]
	      [(arrow? type) (format "~a -> ~a" (print-type (car (arrow-arglist type))) (print-type (arrow-result type)))]
	      [(tlist? type) (format "list of ~a" (print-type (tlist-type type)))]
	      [(tarray? type) (format "array of ~a" (print-type (tarray-type type)))]
	      [(option? type) (format "option of ~a" (print-type (option-type type)))]
	      [(ref? type) (format "ref of ~a" (print-type (ref-type type)))]
;	      [(mlexn? type) (let ([newtypes (if (null? (tconstructor-argtype (mlexn-types type)))
;						(cons null mappings)
;						(unconvert-tvars (tconstructor-argtype (mlexn-types type)) mappings))])
;			       (cons (make-mlexn (mlexn-name type) (make-tconstructor (car newtypes) (tconstructor-result (mlexn-types type)))) (cdr newtypes)))]
	      [(tvar? type) (format "tvar of ~a" (let ([dbox (tvar-tbox type)])
			      (cond
			       [(string? dbox) dbox]
			       [(null? (unbox dbox)) "()"]
			       [else
				(print-type (unbox dbox))])))]
	      [else ;(cons type mappings)]))
	       type]))
;	       (raise-syntax-error #f (format "Bad type: ~a" type) type)]))
	    
	   (define (constructor-lookup name)
	     (let ([constructor (hash-table-get <constructors> name (lambda () #f))])
	       (if constructor
		   (car (convert-tvars (car constructor) null))
		   #f)))

)
