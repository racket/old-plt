#cs(module typecheck mzscheme
	   (require "libs/basic.ss"
		    "prims.ss"
		    "error-msgs.ss"
		    (prefix ast: "ast.ss")
		    (lib "match.ss")
		    (lib "pretty.ss")
		    (lib "list.ss")
		    )
	   (provide typecheck-all convert-tvars)

	   (define cur-var (make-parameter #\a))
	   (define typechoice (make-parameter 'fullcheck))

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
	   (define loc (make-parameter #f))

	   (define stx-for-original-property (read-syntax #f (open-input-string "original")))
;;;; End stolen from Kathy Section

	   (define (typecheck-all stmt location tc)
	     (begin	       
	       (loc location)
	       (typechoice tc)
	       (let ([progtype (typecheck-ml stmt (empty-context))])
		 (begin ;(pretty-print (format "initial progtype: ~a" progtype))
		   (cur-var #\a)
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
		     (raise-error #f
				  (loc)
				  (format "Cannot typecheck: ~a" stmt)
				  stmt
				  #f)]))

	   (define (newboundtypes tdlist)
;	     (pretty-print (format "current newboundtype: ~a" (car tdlist)))
	     (if (null? tdlist)
		 null
		 (cons 
		  (cons 
		   (syntax-object->datum (car (car tdlist))) 
		   (let ([tkind (ast:type_declaration-kind (cdr (car tdlist)))])
		     (cond
		      [(ast:ptype_abstract? tkind)
;		       (pretty-print (format "ptype_abstract: ~a" (syntax-object->datum (car (car tdlist)))))
		       (make-tconstructor (car (convert-list (map make-tvar (ast:type_declaration-params (cdr (car tdlist)))) null null)) (syntax-object->datum (car (car tdlist))))]
		      [(ast:ptype_variant? tkind) 
;		       (pretty-print (format "ptype_variant: ~a" (syntax-object->datum (car (car tdlist)))))
		       (make-usertype (syntax-object->datum (car (car tdlist))) (car (convert-list (map make-tvar (ast:type_declaration-params (cdr (car tdlist)))) null null)))]
		      [else (raise-error #f
					 (loc)
					 (format "Bad kind: ~a" tkind)
					 tkind
					 (ast:type_declaration-loc (cdr (car tdlist))))])))
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
		     (let ([nconst (make-tconstructor (make-<tuple> (map typecheck-type decl (repeat null (length decl)) (repeat null (length decl)))) "exception")])
		       (begin
			 (hash-table-put! <constructors> (syntax-object->datum name) (cons (make-tconstructor 
											    (let ([args (map typecheck-type decl (repeat null (length decl)) (repeat null (length decl)))])
											      (cond 
											       [(null? args) null]
											       [(= (length args) 1) (car args)]
											       [else (make-<tuple> args)])) "exception" ) (string->symbol (format "make-~a" (syntax-object->datum name)))))
			 nconst))]
		       
		    [else
		     (raise-error #f
				  (loc)
				  (format "Unrecognized structure: ~a" desc)
				  desc
				  src)]))

		     
	   (define (typecheck-expr desc context src)
	     (match desc
		    [($ ast:pexp_constant const)
		     (constant-check (syntax-object->datum const))]

		    [($ ast:pexp_tuple xlist)
		     (make-<tuple> (map typecheck-ml xlist (repeat context (length xlist))))]

		    [($ ast:pexp_array xlist)
		     (let ([xtypes (map typecheck-ml xlist (repeat context (length xlist)))])
		       (when (same-types? xtypes (map at (map ast:expression-pexp_desc xlist) (map ast:expression-pexp_src xlist)))
			   (make-tarray (car xtypes))))]

		    [($ ast:pexp_ident name)
		     ;(pretty-print (format "Looking up pexp_ident: ~a" (unlongident name)))
		     (let ([type (get-type (unlongident name) context)])
		       (begin ;(pretty-print (format "pexp_ident type: ~a" type))
			 (if type
			     (let ([itype (instantiate type)])
			       itype)
			     (let* ([rtype (lookup-ident name src)]
				    [fres (car (convert-tvars rtype null))])
			       fres)
			     
			     
		       
		       
			     )))]


			   
		    [($ ast:pexp_ifthenelse test ifexp elseexp isrc tsrc esrc)
;		     (pretty-print (format "context for ifthenelse: ~a" context))
		     (let* ([testt (typecheck-ml test context)]
			   [ifexpt (typecheck-ml ifexp context)]
			   [elseexpt (if (null? elseexp)
					 "unit"
					 (typecheck-ml elseexp context))]
			   [elseexpsrc (if (null? elseexp)
					   (ast:expression-pexp_src ifexp)
					   (ast:expression-pexp_src elseexp))])
		       (begin
;			 (pretty-print (format "testt: ~a, ifexpt: ~a, elseexpt: ~a" testt ifexpt elseexpt))
		       (when (unify "bool" testt (ast:expression-pexp_src test))
			   (when (unify ifexpt elseexpt elseexpsrc)
			       ifexpt)
			   )
			   ))]
		    
		    [($ ast:pexp_match expr pelist)
		     (let ([totest (typecheck-ml expr context)])
		       (typecheck-match pelist totest context))]
		       ;; totest needs to match the beginning of the arrow type of a test
		       ;; all of the types of test need to match
		    [($ ast:pexp_apply proc lelist)
		     (let ([funt (typecheck-ml proc context)]
			   [argst (typecheck-lelist lelist context)])
		       (typecheck-application funt argst (ast:expression-pexp_src proc) (map ast:expression-pexp_src (map cdr lelist))))]


		     

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
				       (raise-error #f
						    (loc)
						    (format "The constructor ~a expects 0 arguments but was given ~a" (unlongident name) 
							    (match (ast:expression-pexp_desc expr)
								   [($ ast:pexp_constant const) "1 argument"]
								   [($ ast:pexp_tuple el) (format "~a arguments" (length el))]
								   [else (format "something I don't understand: ~a" expr)]))
						    (string->symbol (unlongident name))
						    src))
				   (begin				     
;				     (pretty-print (format "tconstructor-argtype is: ~a" (tconstructor-argtype constructor)))
				     (if (null? expr)
					 (raise-error #f
						      (loc)
						      (let ([num
							     (if (<tuple>? (tconstructor-argtype constructor))
								 (length (<tuple>-list (tconstructor-argtype constructor)))
								 1)])
							(format "The constructor ~a expects ~a argument~a but was given no arguments"
							(unlongident name)
							num
							(if (= 1 num)
							    ""
							    "s")))
						      (string->symbol (unlongident name))
						      src)
					 (when (unify (tconstructor-argtype constructor) (typecheck-ml expr context) (ast:expression-pexp_src expr))
					       (tconstructor-result constructor)
					       ))
				     )
				   )
			       constructor)
			   
			   (raise-error #f
					(loc)
					(format "Unbound constructor: ~a" (unlongident name) )
					(string->symbol (unlongident name))
					(longident->src name))))]
			   
		    [($ ast:pexp_try tryexp pelist)
		     (let ([tryt (typecheck-ml tryexp context)]
			   [exnt (typecheck-match pelist "exception" context)])
		       (when (unify tryt exnt src)
			   tryt))]

		    [($ ast:pexp_while testexpr bodyexpr)
		     (let ([testt (typecheck-ml testexpr context)]
			   [bodyt (typecheck-ml bodyexpr context)])
		       (when (and (unify "bool" testt (ast:expression-pexp_src testexpr)) (unify "unit" bodyt (ast:expression-pexp_src bodyexpr)))
			   "unit"))]

		    [($ ast:pexp_for var init test up body)
		     (let ([initt (typecheck-ml init context)]
			   [testt (typecheck-ml test context)])
		       (when (and (unify "int" initt (ast:expression-pexp_src init)) (unify "int" testt (ast:expression-pexp_src test)))
			   (when (unify "unit" 
                                      (typecheck-ml body (update (syntax-object->datum var) (cons "int" null) context))
                                      (ast:expression-pexp_src body))
			       "unit")))]
		    [($ ast:pexp_sequence firstexpr restexpr)
		     (let ([firstt (typecheck-ml firstexpr context)]
			   [restt (typecheck-ml restexpr context)])
		       ;; Warn if type of firstt is not unit
		       restt)]
		    [else
		     (raise-error #f
				  (loc)
				  (format "Cannot typecheck this expression: ~a" desc)
				  desc
				  src)]))


	   (define (print-boundlist item rest)
	     (format "(~a . ~a) ~a" (car item) (print-type (car (unconvert-tvars (cdr item) null))) rest))
		       
	   (define (typecheck-typedecl td boundlist)
;	     (pretty-print (format "typecheck-typedecl: ~a ~a" td (foldl print-boundlist "" boundlist)))
	     (let* ([name (syntax-object->datum (car td))]
		    [typedecl (cdr td)]
		    [params (map syntax-object->datum (ast:type_declaration-params typedecl))])
	       (match (ast:type_declaration-kind typedecl)
		      [($ ast:ptype_abstract dummy)
		       (let ([rtype (typecheck-type (ast:type_declaration-manifest typedecl) boundlist params)])
			 (begin
;			   (pretty-print (format "abstract type being stored: ~a" rtype))
			   (hash-table-put! <constructors> name (cons rtype "some error"))
			   (make-tabstract name
					   (map make-tvar params)
					   rtype)))]
		      [($ ast:ptype_variant scll)
		       (let* ([name-type (make-usertype name (usertype-params (get-type name boundlist)))]
			      [tscll (typecheck-scll name-type params scll boundlist)]
			      [dummy (cur-var #\a)]
			      [ntv (car (unconvert-tvars (make-tvariant name (if (null? params)
							   null
							   (usertype-params (get-type name boundlist)))
							   (map syntax-object->datum (map car scll)) tscll) null))])
			 (begin
			   (hash-table-put! <constructors> name (cons (car (unconvert-tvars name-type null)) "some error"))
			   ntv))]
		      [else
		       (raise-error #f
				    (loc)
				    (format "Unkown typedecl found: ~a" td)
				    td
				    (ast:type_declaration-loc td))]
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
			       [else (raise-error #f 
						   (loc)
						   (format "Bad ptyp_var: ~a" f)
						   f
						   (ast:core_type-src (car type-mapping)))])]
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
			[ttypes (map typecheck-type (cdr current) (repeat boundlist (length (cdr current))) (repeat params (length (cdr current))))]
)
		   (begin
		     (cur-var #\a)
		     (hash-table-put! <constructors> name (cond
							   [(> (length ttypes) 1)
							    (cons (car (unconvert-tvars (make-tconstructor (make-<tuple> ttypes) sname-type) null))
								  (string->symbol (format "make-~a" name)))]
							   [(= (length ttypes) 1)
							    (cons (car (unconvert-tvars (make-tconstructor (car ttypes) sname-type) null))
								  (string->symbol (format "make-~a" name)))]
							   [(= (length ttypes) 0)
							    (cons (car (unconvert-tvars (make-tconstructor null sname-type) null))
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
	       (when (same-types? expts (map at (map cdr pelist) (map ast:expression-pexp_src (map cdr pelist))))
;; Should also be checking that the patterns are exhaustive for t and don't overlap
		   (car expts))))


	   (define (typecheck-defines rec bindings context)
	     ;(pretty-print "typecheckdefines called")
	     (let ([contextwithbindings (typecheck-bindings rec bindings context)])
	       (begin 
;		 (pretty-print (format "contextwithbindings: ~a" contextwithbindings))
		 (cur-var #\a)
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
						   [dummy (if (ast:ppat_var? (ast:pattern-ppat_desc rpat))
							      #t
							      (raise-error #f
									   (loc)
									   "Only variables are allowed as left-hand side of 'let rec' ~a"
									   (string->symbol "let rec")
									   (ast:pattern-ppat_src (car cur-bind))))]
						   [tf (if constraint
							   (typecheck-type (car (convert-ttypes (ast:ppat_constraint-type (ast:pattern-ppat_desc (car cur-bind))))) null #f)
							   (make-arrow (list (fresh-type-var)) (fresh-type-var)))])
					      (update 
					       (syntax-object->datum (ast:ppat_var-name (ast:pattern-ppat_desc rpat))) 
					       (cons tf null) 
					       (bind-vars (cdr bindings))))))])
			  (let ([cprime (bind-vars bindings)])
			    (foldl (lambda (next init)
				     (update (car next) (cdr next) init))
				   context
				   (map (lambda (mapping)
					  (cons (car mapping) (schema (cdr mapping) context)))
					(map typecheck-binding bindings (repeat rec (length bindings)) (repeat cprime (length bindings)))))))

;; for let
		 (let ([ncontexts (map typecheck-binding bindings (repeat rec (length bindings)) (repeat context (length bindings)))])
		   
		   (let ([res (foldl union-envs (car ncontexts) (cdr ncontexts))])
		     (begin
		       ;(pretty-print (format "foldl result: ~a" res))
		       res))

)))

	   (define (typecheck-binding binding rec context)
;	     (pretty-print (format "typecheck-binding ~a" binding))
	     (let* ([rpat (if (ast:ppat_constraint? (ast:pattern-ppat_desc (car binding)))
			      (ast:ppat_constraint-pat (ast:pattern-ppat_desc (car binding)))
			      (car binding))])
	       (if rec
		   (if (and (ast:pexp_function? (ast:expression-pexp_desc (cdr binding)))
			    (ast:ppat_var? (ast:pattern-ppat_desc rpat)))
		       (let ([te (typecheck-ml (cdr binding) context)]
			     [tf (car (get-type (syntax-object->datum (ast:ppat_var-name (ast:pattern-ppat_desc rpat))) context))])
;			 (begin (pretty-print (format "te: ~a, tf: ~a" te tf))
			 (if (unify tf te (ast:expression-pexp_src (cdr binding)))
			     (cons (syntax-object->datum (ast:ppat_var-name (ast:pattern-ppat_desc rpat))) tf))
			 )
		       (raise-error #f 
				    (loc)
				    "This kind of expression is not allowed as right-hand side of 'let rec'" 
				    (string->symbol "let rec")
				    (ast:expression-pexp_src (cdr binding))))
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
;	     (pretty-print (format "context for typecheck-let: ~a" context))
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
	     
	       (when (and (same-types? (map car funvarenvs) (map grab-syntax pelist)) (same-types? expts (map at (map cdr pelist) (map ast:expression-pexp_src (map cdr pelist)))))
;; Should also be checking that the patterns are exhaustive for t and don't overlap
;		   (begin (pretty-print (format "argument: ~a, result: ~a" (car (car funvarenvs)) (car expts)))
		   (make-arrow (list (car (car funvarenvs))) (car expts))
	     )))

	   (define (grab-syntax pe)
	     (ast:pattern-ppat_src (car pe)))


	   (define (typecheck-constraint asttype boundlist)
	     (typecheck-type asttype boundlist #f))

;	   (define (typecheck-decl asttype boundlist)
;	     (typecheck-type asttype boudnlist #t))

	   (define (typecheck-type asttype boundlist decl?)
;	     (pretty-print (format "typecheck-type ~a ~a" asttype boundlist))
	     (match (ast:core_type-desc asttype)
		    [($ ast:ptyp_any dummy)
		     (make-tvar (box null))]
		    [($ ast:ptyp_var f)
		     (let ([content (cond
				     [(syntax? f) (syntax-object->datum f)]
				     [(or (string? f) (box? f)) f]
				     [else (raise-error #f 
						     (loc)
						     (format "Bad ptyp_var: ~a" f)
						     f
						     (ast:core_type-src asttype))])])
		       (if (and decl? (null? (filter (lambda (var)
						       (equal? var content))
						     decl?)))
			   (raise-error #f
					(loc)
					(format "Unbound type parameter '~a" content)
					(string->symbol content)
					(ast:core_type-src asttype))
			   (make-tvar content)))]
		    [($ ast:ptyp_arrow label ct1 ct2)
		     (make-arrow (list (typecheck-type ct1 boundlist decl?)) (typecheck-type ct2 boundlist decl?))]
		    [($ ast:ptyp_tuple ctlist)
		     (make-<tuple> (map typecheck-type ctlist (repeat boundlist (length ctlist)) (repeat decl? (length ctlist))))]
		    [($ ast:ptyp_constr name ctlist)
;		     (pretty-print (format "ptyp_constr ~a ~a" (unlongident name) ctlist))
		     (let* ([cexists (get-type (unlongident name) boundlist)]
			    [fconstructor (if cexists
					;(car (convert-tvars cexists null))
					      cexists
					      (constructor-lookup (unlongident name)))]
;			    [foobar (pretty-print (format "constructor-lookup: ~a" (unlongident name)))]
			    )
			   
		       (if fconstructor
			   (cond
			    [(tconstructor? fconstructor)
;			     (pretty-print "fconstructor in typecheck-type is tconstructor - this case should never occur")
			     (if (null? (tconstructor-argtype fconstructor))
				 (if (null? ctlist)
				     (tconstructor-result fconstructor)
				     (raise-syntax-error #f (format "Constructor expects no arguments but got:~a" ctlist) (ast:core_type-src asttype)) )
				 (if (unify (tconstructor-argtype) (typecheck-type (car ctlist) boundlist decl?) (ast:core_type-src (car ctlist)))
				     (tconstructor-result fconstructor)))]
				       

			    [(usertype? fconstructor)
			     (if (null? (usertype-params fconstructor))
				 (if (null? ctlist)
				     fconstructor
				     (raise-error #f
						  (loc)
						  (format "The type constructor ~a expects 0 arguments but was given ~a" (usertype-name fconstructor) (let ([num (length ctlist)])
																			(if (= 1 num)
																			    "1 argument"
																			    
																			    (format "~a arguments" num))))
						  (string->symbol (usertype-name fconstructor))
						  (ast:core_type-src asttype)))
				 (if (= (length (usertype-params fconstructor))
					(length ctlist))
				     (when (andmap 
					    unify 
					    (usertype-params fconstructor) 
					    (car 
					     (convert-list 
					      (map 
					       typecheck-type 
					       ctlist 
					       (repeat 
						boundlist 
						(length ctlist)) 
					       (repeat decl? 
						       (length ctlist))) 
					      null 
					      null)) 
					    (repeat 
					      (ast:core_type-src (car ctlist)) 
					     (length ctlist)))
					   fconstructor)
				     (raise-error #f
						  (loc)
						  (let ([cnum (length (usertype-params fconstructor))]
							[unum (length ctlist)])
						    (format "The type constructor ~a expects ~a argument~a but was given ~a argument~a"
							    (usertype-name fconstructor)
							    cnum
							    (if (= 1 cnum)
								""
								"s")
							    unum
							    (if (= 1 unum)
								""
								"s")))
						  (string->symbol (usertype-name fconstructor))
						  (ast:core_type-src asttype)))

				     )]
			    [(equal? (unlongident name) "list")
			     (one-argument? ctlist "list" make-tlist (ast:core_type-src asttype) boundlist decl?)]

			    [(equal? (unlongident name) "ref")
			     (one-argument? ctlist "ref" make-ref (ast:core_type-src asttype) boundlist decl?)]

			    [(equal? (unlongident name) "array")
			     (one-argument? ctlist "array" make-tarray (ast:core_type-src asttype) boundlist decl?)]

			    [(string? fconstructor)
			     (if (null? ctlist)
				 fconstructor
				 (raise-error #f
					      (loc)
					      (let ([cnum (length ctlist)])
						(format "The type constructor ~a expects 0 arguments but was given ~a argument~a"
							fconstructor
							cnum
							(if (= cnum 1)
							    ""
							    "s")))
					      (string->symbol fconstructor)
					      (ast:core_type-src asttype)))]
			    [else
;			     (raise-error #f
;					  (loc)
;					  (format "I don't know how to handle this. fcontructor: ~a ctlist: ~a"
;						  fconstructor ctlist)
;					  fconstructor
;					  (ast:core_type-src asttype))])
			     fconstructor])
			   (raise-error #f 
					(loc)
					(format "Unbound type constructor: ~a"
						(unlongident name)) 
					(string->symbol (unlongident name))
					(ast:core_type-src asttype))))]
;		    [($ ast:ptyp_variant rfl abool ll)
;		     'comeslater
		     ;; Need to implement this!!!
;]
		    [else
		     (raise-error #f 
				  (loc)
				  (format "Cannot handle type: ~a" asttype) 
				  asttype
				  (ast:core_type-src asttype))]))

	   (define (one-argument? alist name fun src boundlist decl?)
	     (if (= 1 (length alist))
		 (fun (typecheck-type (car alist) boundlist decl?))
		 (raise-error #f
			      (loc)
			      (format "The type constructor ~a takes 1 argument but was given ~a arguments" name (length alist))
			      (string->symbol name)
			      src)))

	   (define (typecheck-application funt argst funsyn argssyn)
;	     (pretty-print (format "typecheck-application ~a ~a" funt argst))
	     (when (with-handlers
		    ([exn:syntax? (lambda (exn)
				    (raise-error #f
						 (loc)
						 (format "This expression has type ~a. It cannot be applied because it is not a function." (car (unconvert-tvars funt null)))
						 (void)
						 funsyn))])
		    (unify (make-arrow (list (fresh-type-var)) (fresh-type-var)) funt funsyn))
		   (letrec ([numfunargs (lambda (f) (if (arrow? f)
							(+ 1 (numfunargs (arrow-result f)))
							0))])
;		     (if (= (length argst) (numfunargs funt))
			 (let ([arglist (get-arglist funt)])
			   (if (= (length arglist) (length argst))
			       (begin
;				 (pretty-print (format "arglist: ~a argst: ~a" arglist argst))
				 (when (andmap unify arglist argst argssyn)
				       (get-result funt))
				 )
			       (if (> (length argst) (length arglist))
				   (foldl uncurry (if (unify (car (get-arglist funt)) (car argst) (car argssyn))
						      (get-result funt)) 
					  (cdr argst)
					  (cdr argssyn))
				   (raise-error #f 
						(loc)
						(format "Expected ~a argument~a but found ~a" (length arglist) (if (= 1 (length arglist))
														   ""
														   "s")
							(length argst))
						(void)
						argssyn))))
;			 (letrec ([firstelemsrc (car argssyn)]
;				  [lastelem (lambda (v) (if (null? (cdr v))
;							    (car v)
;							    (lastelem (cdr v))))]
;				  [lastelemsrc (lastelem argssyn)]
;				  [argsrc
;				   (ast:make-src (ast:src-line firstelemsrc)
;						 (ast:src-col firstelemsrc)
;						 (ast:src-pos firstelemsrc)
;						 (+ (- (ast:src-pos lastelemsrc) (ast:src-pos firstelemsrc)) (ast:src-span lastelemsrc)))])
;			   (raise-error #f 
;					(loc)
;					(format "Expected ~a argument~a but found ~a" (numfunargs funt) (if (= 1 (numfunargs funt))
;													    ""
;													    "s")
;						(length argst))
;					(void)
;					argsrc))
			 ) ))


	   (define (uncurry nextt argsyn init)
;	     (pretty-print (format "uncurry: ~a ~a ~a" nextt argsyn init))
	     (if (unify init (make-arrow (list nextt) (fresh-type-var)) argsyn)
		 (let ([arglist (get-arglist init)])
		   (unless (> (length arglist) 1)
		       (get-result init)))))

		    
	   (define (funvarcheck context pat)
;	     (pretty-print (format "funvarcheck: ~a" pat))
	     (if (unique-var pat null)
		 (let ([varenv (funenv pat context)])
		   (begin ;(pretty-print (format "var-env from funvarcheck: ~a" varenv))
			  (cons (car varenv) (union-envs (cdr varenv) context))))))

	   (define (patcheck context type pat) 
	     ;(pretty-print (format "patcheck: ~a" pat))
	     (if (unique-var pat null)
		 (union-envs (patenv pat type context) context)
		 'fuzzle))

	   (define (unique-var pat curvars)
;	     (pretty-print (format "unique-var: ~a" pat))
	     (match (ast:pattern-ppat_desc pat)
		    [($ ast:ppat_any dummy)
		     curvars]
		    [($ ast:ppat_var name)
;		     (pretty-print (format "varname: ~a" name))
		     (if (present? (syntax-object->datum name) curvars)
			 (raise-error #f
				      (loc)
				      (format "The variable ~a is bound several times in this matching" (syntax-object->datum name))
				      (string->symbol (syntax-object->datum name))
				      (ast:pattern-ppat_src pat))
			 (cons (syntax-object->datum name) curvars))]
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

	   (define (funenv pat context)
;	     (if (and (eq? (typechoice) 'fullcheck)
;		      (not (ast:ppat_constraint?  (ast:pattern-ppat_desc pat))))
;		 (raise-error #f
;			      (loc)
;			      "No type provided for function paramater"
;			      (string->symbol (syntax-object->datum (ast:ppat_var-name (ast:pattern-ppat_desc pat))))
;			      (ast:pattern-ppat_src pat))
		 
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
			 (let ([varenvs (map funenv plist (repeat context (length plist)))])
			   (begin ;(pretty-print (format "varenvs from funenv: ~a" varenvs))
			     (cons (make-<tuple> (map car varenvs)) (foldl union-envs (car (map cdr varenvs)) (cdr (map cdr varenvs))))))]
			[($ ast:ppat_construct longident cpat bool)
			 (let ([cpat-type (constructor-lookup (unlongident longident))]) 
			   (if cpat-type
			       (if (tconstructor? cpat-type)
				   (if (null? cpat)
				       (if (null? (tconstructor-argtype cpat-type))
					   (cons (tconstructor-result cpat-type) (empty-context))
					   (raise-error #f 
							(loc)
							(let ([nargs (if (<tuple>? (tconstructor-argtype cpat-type))
									 (length (<tuple>-list) (tconstructor-argtype cpat-type))
									 1)])
							  (format "The constructor ~a expects ~a argument~a but was given no arguments"
								  (unlongident longident)
								  nargs
								  (if (= 1 nargs)
								      ""
								      "s")))
							(string->symbol (unlongident longident))
							(ast:pattern-ppat_src pat)))
				       (if (null? (tconstructor-argtype cpat-type))
					   (raise-error #f 
							(loc)
							(let* ([ftype (car (funenv cpat context))]
							       [nargs (if (<tuple>? ftype)
									  (length (<tuple>-list ftype))
									  1)])
							  (format "The constructor ~a expects 0 arguments but was given ~a argument~a"
								  (unlongident longident)
								  nargs
								  (if (= 1 nargs)
								      ""
								      "s")))
							(string->symbol (unlongident longident))
							(ast:pattern-ppat_src pat))
					   (cons (tconstructor-result cpat-type) (let ([fenvpat (funenv cpat context)])
										   (when (unify (tconstructor-argtype cpat-type) (car fenvpat) (ast:pattern-ppat_src cpat))
											 (cdr fenvpat))))))
				   (if (null? cpat)
				       (cons cpat-type (empty-context))
				       (raise-error #f 
						    (loc)
						    (let* ([ftype (car (funenv cpat context))]
							   [nargs (if (<tuple>? ftype)
								      (length (<tuple>-list ftype))
								      1)])
						      (format "The constructor ~a expects 0 arguments but was given ~a argument~a"
							      (unlongident longident)
							      nargs
							      (if (= 1 nargs)
								  ""
								  "s")))
						    (string->symbol (unlongident longident))
						    (ast:pattern-ppat_src pat))
				       ))
			       (raise-error #f 
					    (loc)
					    (format "Unbound constructor ~a" (unlongident longident))
					    (string->symbol (unlongident longident))
					    (ast:pattern-ppat_src pat))) )]
			
			[($ ast:ppat_constraint pat ct)
					;Assume a constraint must immediately follow a variable
			 (let ([type (typecheck-type (car (convert-ttypes (cons ct null))) null #f)]
			       [varname (syntax-object->datum (ast:ppat_var-name (ast:pattern-ppat_desc pat)))])
			   (cons type (update varname (cons type null) (empty-context))))]
			[else (raise-error #f 
					   (loc)
					   (format "No such pattern found: ~a" pat)
					   pat
					   (ast:pattern-ppat_src pat))]))
;)
		     
	   (define (patenv pat type context)
;	     (pretty-print (format "patenv type: ~a , context: ~a" type context))
	     (let ([patsyn (ast:pattern-ppat_src pat)])
	     (match (ast:pattern-ppat_desc pat)
		    [($ ast:ppat_any dummy)
		     (empty-context)]
		    [($ ast:ppat_var name)
;		     (pretty-print (format "var: ~a schema: ~a" (syntax-object->datum name) (schema type context)))
		     (update (syntax-object->datum name) (schema type context) (empty-context))]
		    [($ ast:ppat_constant const)
		     (let ([t2 (constant-check const)])
		       (when (unify type t2 patsyn)
			   (empty-context)
			   ))]
		    [($ ast:ppat_tuple plist)
		     (begin ;(pretty-print (format "plist: ~a" plist))
			    
			    (when (unify type (make-<tuple> (letrec 
							      ([ftvarlist (lambda (len)
										(if (= len 0)
								
										    null
										    (cons (fresh-type-var) (ftvarlist (- len 1)))))])
							    (ftvarlist (length plist))))
				       (ast:pattern-ppat_src (car plist)))
				(let* ([tlist (letrec ([get-list
							(lambda (type)
							  (cond
							   [(tvar? type)
							    (get-list (unbox (tvar-tbox type)))]
							   [(<tuple>? type)
							    (<tuple>-list type)]
							   [else (raise-error 
								  #f
								  (loc)
								  (format "Not a list: ~a" type) #f)]))]) (get-list type))]
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
					   (raise-error #f 
							(loc) 
							(let ([nargs (if (<tuple>? (tconstructor-argtype cpat-type))
									 (length (<tuple>-list (tconstructor-argtype cpat-type)))
									 1)])
							  (format "The constructor ~a expects ~a argument~a but was given 0 arguments"
								  (unlongident longident)
								  nargs
								  (if (= 1 nargs)
								      ""
								      "s")))
							(string->symbol (unlongident longident))
							patsyn))
				       (if (null? (tconstructor-argtype cpat-type))
					   (raise-error #f
							(loc)
							(let ([nargs (if (ast:ppat_tuple? (ast:pattern-ppat_desc cpat))
									 (length (ast:ppat_tuple-pattern-list (ast:pattern-ppat_desc cpat)))
									 1)])
							  (format "The constructor ~a expects 0 arguments but was given ~a argument~a"
								  (unlongident longident)
								  nargs
								  (if (= 1 nargs)
								      ""
								      "s")))
							  (string->symbol (unlongident longident))
							  patsyn)
					   (patenv cpat (tconstructor-argtype cpat-type) context))))
			       (if (unify type cpat-type patsyn)
				   (if (null? cpat)
				       (empty-context)
				       (raise-error #f
							(loc)
							(let ([nargs (if (ast:ppat_tuple? (ast:pattern-ppat_desc cpat))
									 (length (ast:ppat_tuple-pattern-list (ast:pattern-ppat_desc cpat)))
									 1)])
							  (format "The constructor ~a expects 0 arguments but was given ~a argument~a"
								  (unlongident longident)
								  nargs
								  (if (= 1 nargs)
								      ""
								      "s")))
							  (string->symbol (unlongident longident))
							  patsyn)
				       )))
			   (raise-error #f
					(loc)
					(format "Unbound constructor: ~a" (unlongident longident))
					(string->symbol (unlongident longident))
					patsyn)) )]
		    [($ ast:ppat_constraint pat ct)
		     (if (unify type (typecheck-type ct null) patsyn)
			 (update (syntax-object->datum (ast:ppat_var-name (ast:pattern-ppat_desc pat))) (schema type context) (empty-context)))]
		    [else
		     (raise-error 
		      #f 
		      (loc)
		      (format "Pattern not recognized ~a" pat)
		      pat
		      patsyn)])))

	     
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
	     (filter (lambda (r) 
		       (null? 
			(filter (lambda (rprime) 
				  (eq? r rprime)) 
				l2))) 
		     l1))


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
	      [else (raise-error 
		     #f 
		     (loc)
		     (format "Bad type: ~a" type))]))

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
				 [(ref? t) (make-ref (inst (ref-type t)))]
				 [else (raise-error #f
						    (loc)
						    (format "Cannot instantiate: ~a" t)
						    t
						    #f)]))])
		 (inst type))))
		 
	   (define (schema type env)
	     (let* ([uv (unsolved type)]
		    [ev (env-unsolved env)]
;		    [foo (pretty-print (format "uv: ~a , ev: ~a" uv ev))]
		    [uvprime (diff uv ev)]
;		    [goo (pretty-print (format "uvprime: ~a" uvprime))]
;		    [noo (pretty-print (
		    )
	       (cons type uvprime)))


	   (define (unify-error t1 t2 syn)
	     (cur-var #\a)
	     (raise-error #f
			  (loc)
			  (format "This expression is supposed to have type ~a but instead has type ~a" (print-type (car (unconvert-tvars t1 null))) (print-type (car (unconvert-tvars t2 null))))
			  (void)
			  syn))
	   
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
			     (unify-error t1a t2a syn))))]
		  [(tvar? t2) (unify-var t1 (tvar-tbox t2) syn)]
		  [else 
		   (unify-error t1 t2 syn)])]
		[(arrow? t1)
		 (cond
		  [(arrow? t2) 
		   (with-handlers 
		    ([exn:syntax?
		      (lambda (exn) (unify-error t1 t2 syn))]) 
		    (and 
		     (unify (car (arrow-arglist t1)) 
			    (car (arrow-arglist t2)) 
			    syn) 
		     (unify 
		      (arrow-result t1) 
		      (arrow-result t2) 
		      syn)))]
		  [(tvar? t2) (unify-var t1 (tvar-tbox t2) syn)]
		  [else (unify-error t1 t2 syn)])]
		[(<tuple>? t1)
		 (cond
		  [(<tuple>? t2) 
		   (if (= (length (<tuple>-list t1)) (length (<tuple>-list t2)))
		       (andmap unify (<tuple>-list t1) (<tuple>-list t2) (repeat syn (length (<tuple>-list t1))))
		       (unify-error t1 t2 syn))]
		  [(tvar? t2) (unify-var t1 (tvar-tbox t2) syn)]
		  [else (unify-error t1 t2 syn)])]
		[(ml-exn? t1)
		 (cond
		  [(ml-exn? t2) #t]
		  [(tvar? t2) (unify-var t1 (tvar-tbox t2) syn)]
		  [else (unify-error t1 t2 syn)])]
		[(tlist? t1)
		 (cond
		  [(tlist? t2) (unify (tlist-type t1) (tlist-type t2) syn)]
		  [(tvar? t2) (unify-var t1 (tvar-tbox t2) syn)]
		  [else (unify-error t1 t2 syn)])]
		[(tarray? t1)
		 (cond
		  [(tarray? t2) (unify (tarray-type t1) (tarray-type t2) syn)]
		  [(tvar? t2) (unify-var t1 (tvar-tbox t2) syn)]
		  [else (unify-error t1 t2 syn)])]
		[(option? t1)
		 (cond
		  [(option? t2) (unify (option-type t1) (option-type t2) syn)]
		  [(tvar? t2) (unify-var t1 (tvar-tbox t2) syn)]
		  [else (unify-error t1 t2 syn)])]
		[(ref? t1)
		 (cond
		  [(ref? t2) (unify (ref-type t1) (ref-type t2) syn)]
		  [(tvar? t2) (unify-var t1 (tvar-tbox t2) syn)]
		  [else (unify-error t1 t2 syn)])]
		[(usertype? t1)
		 (cond
		  [(usertype? t2) (if (equal? (usertype-name t1) (usertype-name t2))
				      (if (= (length (usertype-params t1)) (length (usertype-params t2)))
					  (andmap unify (usertype-params t1) (usertype-params t2) (repeat syn (length (usertype-params t1))))
					  (raise-syntax-error #f (format "Expected ~a with ~a parameters but found it with ~a parameters" (usertype-name t1) (length (usertype-params t1)) (length (usertype-params t2))) syn))
				      (unify-error t1 t2 syn))]
		  [(tvar? t2) (unify-var t1 (tvar-tbox t2) syn)]
		  [else (unify-error t1 t2 syn)])]
		[else (raise-error #f
				   (loc)
				   (format "Bad type to unify ~a" t1)
				   t1
				   syn)])))

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

	   (define (lookup-ident uname src)
	     (match uname
		    [($ ast:lident name)
		     (let* ([rname (cond
				    [(syntax? name) (syntax-object->datum name)]
				    [(string? name) name])]
			    [result (hash-table-get built-in-and-user-funcs rname (lambda () #f))])
		       (if result
			   (car result)
			   (with-handlers
			    ([exn:syntax? (lambda (exn)
					    (raise-error #f
							 (loc)
							 (format "Unbound value: ~a" rname)
							 (string->symbol rname)
							 src)
					    )])
			    (lookup-ident (ast:make-ldot (ast:make-lident (datum->syntax-object #f "Pervasives")) name) src))
					    ))]
		    
		    [($ ast:ldot longident name)

		     (when (ast:lident? longident)
			 (let* ([lib-name (syntax-object->datum (ast:lident-name longident))]
				[lib-map (hash-table-get <library-names> lib-name (lambda () #f))])
			   (if lib-map
			       (let ([function (hash-table-get lib-map (syntax-object->datum name) (lambda () #f))])
				 (if function
			
				     (car function)
				     
				     (raise-error #f
						  (loc)
						  (format "Value ~a not found in library ~a" (syntax-object->datum name) lib-name)
						  (string->symbol (syntax-object->datum name))
						  name)))

			       (raise-error #f
					    (loc)
					    (format "Unknown libaray: ~a" lib-name)
					    (string->symbol lib-name)
					    (ast:lident-name longident))
				)))]
		    [_
		     (raise-error #f
				  (loc)
				  (format "Bad longident: ~a" uname)
				  uname
				  uname)]))
			       

	   (define (convert-tvars type mappings)
	     (cond
	      [(string? type) (cons type mappings)]
	      [(<tuple>? type) (let ([tlist (convert-list (<tuple>-list type) mappings null)])
			       (cons (make-<tuple> (reverse (car tlist))) (cdr tlist)))]
	      [(arrow? type) (let* ([tlist (convert-list (arrow-arglist type) mappings null)]
				    [restype (convert-tvars (arrow-result type) (cdr tlist))])
			       (cons (make-arrow (reverse (car tlist)) (car restype)) (cdr restype)))]
	      [(usertype? type) 
	       (let ([tlist (convert-list (usertype-params type) mappings null)])
				  (cons (make-usertype (usertype-name type) (reverse (car tlist))) (cdr tlist)))
	       
	       ]
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
				    (cons newvar (cons (cons (tvar-tbox type) newvar) mappings))))
)]
	      [(option? type) (let ([mtype (convert-tvars (option-type type)  mappings)])
				(cons (make-option (car mtype)) (cdr mtype)))]
	      [(ml-exn? type) (cons type mappings)]
	      [(ref? type) (let ([rtype (convert-tvars (ref-type type) mappings)])
			     (cons (make-ref (car rtype)) (cdr rtype)))]
	      
	      [else (raise-syntax-error #f "Bad type to convert!" type)]))

	   (define (unconvert-tvars type mappings)
	     (cond
	      [(value-set? type) (let ([nmap (unconvert-tvars (value-set-type type) mappings)])
				   (cons (make-value-set (value-set-name type) (car nmap)) (cdr nmap)))]
	      [(tabstract? type) (let* ([nparams (unconvert-list (tabstract-params type) mappings null)]
				       [nt (unconvert-tvars (tabstract-type type) (cdr nparams))])
				  (cons (make-tabstract (tabstract-name type) (reverse (car nparams)) (car nt)) (cdr nt)))]
	      [(tvariant? type) (let* ([nparams (unconvert-list (tvariant-params type) mappings null)]
				       [ntlist (unconvert-list (tvariant-variantlist type) (cdr nparams) null)])
				  (cons (make-tvariant (tvariant-name type) (reverse (car nparams)) (tvariant-varnames type) (reverse (car ntlist))) (cdr ntlist)))]
	      [(usertype? type) 
	       (let ([tlist (unconvert-list (usertype-params type) mappings null)])
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
	      [(ml-exn? type) (cons type mappings)]
	      [(tconstructor? type) (let* ([atype (unconvert-tvars (tconstructor-argtype type) mappings)]
					   [rtype (unconvert-tvars (tconstructor-result type) (if (null? atype)
												  mappings 
												  (cdr atype)))])
				      (cons (make-tconstructor (if (null? atype)
								   null
								   (car atype)) 
							       (car rtype)) 
					    (cdr rtype)))]
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
					  (cur-var (integer->char (+ 1 (char->integer (cur-var)))))
					  (let ([old-var (format "'~a" (integer->char (- (char->integer (cur-var)) 1)))])
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
;	       (set! cur-varc (+ cur-var 1))
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
	    
	   (define (constructor-lookup name)
	     (let ([constructor (hash-table-get <constructors> name (lambda () #f))])
	       (if constructor
		   (car (convert-tvars (car constructor) null))
		   #f)))

)
