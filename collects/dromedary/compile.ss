#cs(module compile mzscheme
     (require "parse.ss"
	      "prims.ss"
	      (prefix ast: "ast.ss")
	      (lib "match.ss")
	      (lib "list.ss")
	      (lib "pretty.ss"))

     (provide compile-all)

     (define next-label 0)     
     (define loc #f)

     (define (compile-all stmt location)
       (set! loc location)
       (list
;	(datum->syntax-object
;	 #f
	 (compile-ml stmt (empty-context))))
;	 #f)))

     (define current-compile-context (make-parameter #f))

     (define (compile-ml stmt context)
       (match stmt
	      [($ ast:expression desc src)
	       (compile-expr desc src context)]
	      [(a . b)
;	       (if (null? b)
		   (cons (compile-ml a context) (compile-ml b context))]
;		      [($ ast:structure_item desc src)
;		       (compile-structure desc src context)]
;		      )]
	      [($ ast:structure_item desc src)
	       (compile-structure desc src context)]
	      [() null]
	      [else
	       (pretty-print (list "Unknown: " stmt))]))

     (define (compile-mls slist context)
       (if (pair? slist)
	   (cons (compile-ml (car slist) context) (compile-mls (cdr slist) context))
	   (if (null? slist)
	       null
	       (cons (compile-ml slist context) null))))

     (define (compile-structure desc src context)
       (match desc
	      [($ ast:pstr_value rec_flag pelist)
	       (let ([all-bindings (map compile-define (repeat rec_flag (length pelist)) pelist (repeat context (length pelist)))])
		 all-bindings)]
	      [($ ast:pstr_type stdlist)
	       (let* ([assumeone (car stdlist)]
		      [name (eval (car assumeone))]
		      [typedecl (cdr assumeone)])
		 (match (ast:type_declaration-kind typedecl)
			[($ ast:ptype_abstract dummy)
			 (let ([core (compile-core_type (ast:type_declaration-manifest typedecl))])
			   (begin
			     (hash-table-put! user-types name core)
			     #'(make-<voidstruct> #f)))]
			[($ ast:ptype_variant scll)
			 (let ([cscll (compile-scll scll)])
			   (begin
			     (hash-table-put! user-types name (lambda (x) (isa-variant? 'a 'b cscll)))
			     (pretty-print "begining type definitions")
			     #`(begin #,@(mkdefinestructs scll) (make-<voidstruct> #f))))]
;			     #`(define-struct #,(string->symbol(eval (caar scll))) (tlist))))]
			 ))]
	      [($ ast:pstr_eval expr)
	       (compile-ml expr context)]
	      [($ ast:pstr_exception name decl)
	       #`(begin (define-struct (#,(string->symbol (format "exn:~a" (eval name))) exn) ())
			(make-<voidstruct> #f))]
	      [($ ast:pstr_exn_rebind name ident)
;	       #'"voigt"]
	       (let* ([rname (syntax-object->datum name)]
		      [rident (syntax-object->datum ident)]
		      [maker (string->symbol (format "make-~a" rname))]
		      [omaker (string->symbol (format "make-~a" rident))]
		      [test (string->symbol (format "~a?" rname))]
		      [otest (string->symbol (format "~a?" rident))])
	       #`(begin (define-values (#,maker #,test) (values #,omaker #,otest))
			(make-<voidstruct> #f)))]
	      [else
	       (pretty-print (list "Unknown structure: " desc))]))

     (define (mkdefinestructs scll)
       (if (null? scll)
	   null
	   (cons #`(define-struct #,(string->symbol (eval (caar scll))) (tlist) (make-inspector)) (mkdefinestructs (cdr scll)))))

     (define (mkdefine binding)
       #`(define #,(car binding) #,(cdr binding)))

     (define (compile-core_type ct)
       (match (ast:core_type-desc ct)
	      [($ ast:ptyp_any dummy)
	       any?]
	      [($ ast:ptyp_constr name ctl)
	       (hash-table-get constructors (unlongident name))]
	      [else
	       (pretty-print (list "Bad core type" ct))]))

     (define (compile-scll scll)
       (if (null? scll)
	   null
	   (let* ([current (car scll)]
		  [name (eval (car current))]
		  [compiled-types (map compile-core_type (cdr current))])
	     (cons (lambda (xname xtypes) (and (string=? name xname) (= (length (compiled-types)) (length (xtypes))) (correct-types? compiled-types xtypes)))
		   (compile-scll (cdr scll))))))

     (define (isa-variant? name type variants)
       (if (null? variants)
	   #f
	   (if ((car variants) name type)
	       #t
	       (isa-variant? name type (cdr variants)))))

     (define (add-types cscll)
       (if (null? cscll)
	   null
	   (let* ([current (car cscll)]
		  [name (car current)]
		  [types (cdr current)])
	     (begin
	       (hash-table-put! user-types name (lambda (x) (and (= (length x) (length types)) (correct-types? types x))))
	       (add-types (cdr cscll))))))

     (define (correct-types? types x)
       (if (null? x)
	   #t
	   (and ((car types) (car x)) (correct-types? (cdr types) (cdr x)))))

     (define (compile-expr desc src context)
       (match desc
	      [($ ast:pexp_constant const)
	       const]
	      [($ ast:pexp_apply proc lelist)
	       (let ([fun (compile-ml proc context)]
		     [args (compile-lelist lelist context)])
		 (if (ml-primitive? (syntax-object->datum fun) ml-prims)
		     #`(#,fun #,@args)
		     (compile-apply fun (reverse args))))]

	      [($ ast:pexp_let rec bindings expr)
	       (compile-let rec bindings expr context)]
;	       (let* ([all-bindings (compile-bindings bindings context)]
;		      [lhss (map car all-bindings)]
;		      [rhss (map cdr all-bindings)])
;		 (with-syntax ([(lhs ...) lhss]
;			       [(rhs ...) rhss])
;			      #`(#,(if rec #'letrec #'let) ((lhs rhs) ...) #,(compile-ml expr context))))]
	       
			       


	      [($ ast:pexp_ident name)
	       (datum->syntax-object #f (or (lookup-ident name) (string->symbol (unlongident name))))]

	      [($ ast:pexp_function label expr pelist)
;; The parser makes only one-variable functions by default which is a pain
;; when it comes to application, so we need to figure out just how many
;; variables this function has
	       (compile-function pelist context)]
;	       (let ([varsandbody (compile-function pelist context)])
;		 #`(lambda #,(car varsandbody) #,(cdr varsandbody)))]
	      [($ ast:pexp_ifthenelse test ifexp elseexp)
	       (let ([testc (compile-ml test context)]
		     [ifexpc (compile-ml ifexp context)]
		     [elseexpc (if (null? elseexp) null (compile-ml elseexp context))])
		 #`(if #,testc #,ifexpc #,(if (not (null? elseexpc)) elseexpc)))]
	      [($ ast:pexp_construct name expr bool)
	       (let ([constr (hash-table-get constructors (unlongident name) (lambda () #f))])
		 (if constr
		     (if (null? expr)
			 (cdr constr)
			 (let ([args (compile-expr (ast:expression-pexp_desc expr) (ast:expression-pexp_src expr) context)])
			   #`(#,(cdr constr) #,@(cond
						[(tuple? args) (tuple-list args)]
						[(list? args) args]
						[else (list args)]))))
		     (let ([rconstr (string->symbol (format "make-~a" (unlongident name)))]
			   [args (if (null? expr) #f (compile-exps (ast:pexp_tuple-expression-list (ast:expression-pexp_desc expr)) context))])
		       (if args
			   #`(#,rconstr #,@args)
			   #`#,rconstr))))]
;	       (cond
;		[(and (null? expr) (not bool) 
;		      (if (hash-table-get constructors name (lambda () #f)) #t #f))
;		 (cdr (hash-table-get constructors name))]
;		[(and (not bool) (if (hash-table-get constructors name (lambda () #f)) #t #f))
;		 (let ([constr (cdr (hash-table-get constructors name))]
;		       [args (compile-exps (ast:pexp_tuple-expression-list (ast:expression-pexp_desc expr)) context)])
;		   #`(#,constr #,@args))]
;		[else
;		 (let ([constr (string->symbol (format "make-~a" (ast:lident-name name)))]
;		       [args (if (null? expr) null (compile-ml expr context))])
;		 #`(#,(string->symbol (format "make-~a" (ast:lident-name name))) #,args))]
;		)]

	      [($ ast:pexp_match expr pelist)
	       (let ([totest (compile-ml expr context)]
		     [tests (compile-match pelist context)])
		 #`((match-lambda #,@tests) #,totest))]

	      [($ ast:pexp_tuple xlist)
	       #`(make-tuple (list #,@(compile-exps xlist context)))]

	      [($ ast:pexp_try tryexp pelist)
	       (let ([matches (compile-match pelist)]
		     [tryres (compile-ml tryexp)])
		 #`(with-handlers ([(not-break-exn? (match-lambda #,@matches))]) #,tryres))]
				  
	      [else
	       (pretty-print (list "Expression unknown: " desc))]))


     (define (compile-match pelist context)
       (if (null? pelist)
	   null
	   (cons #`[#,(get-varpat (caar pelist)) #,(compile-ml (cdar pelist) context)]
		 (compile-match (cdr pelist) context))))

     (define (repeat x n)
       (if (<= n 0)
	   null
	   (cons x (repeat x (- n 1)))))

     (define (compile-apply fun args)
       (if (null? args)
	   fun
	   #`(#,(compile-apply fun (cdr args)) #,(car args))))



     (define (get-varpat pattern)
       (match (ast:pattern-ppat_desc pattern)
	      [($ ast:ppat_var variable)
	       (string->symbol (eval variable))]
	      [($ ast:ppat_constant const)
	       (eval const)]
	      [($ ast:ppat_tuple tlist)
	       #`($ tuple #,(map get-varpat tlist))]
	      [($ ast:ppat_constraint pat ct)
	       (get-varpat pat)]
	      [($ ast:ppat_any dummy)
	       #'_]
	      [($ ast:ppat_construct name pat bool)
	       (cond [(and (null? pat) (not bool) (if (hash-table-get constructors (unlongident name) (lambda () #f)) #t #f))
		      (hash-table-get constructors (unlongident name))]
		     [(not bool)
		      (let ([constructor (hash-table-get constructors (unlongident name) (lambda () #f))])
			(if constructor
			    ;; Best way I can think of to do this is specail case
			    (if (equal? (hash-table-get constructors (unlongident name)) cons)
				;; The pattern should be a tuple of two
				(if (ast:ppat_tuple? (ast:pattern-ppat_desc pat))
				    (let ([head (get-varpat (car (ast:ppat_tuple-pattern-list (ast:pattern-ppat_desc pat))))]
					  [tail (get-varpat (cadr (ast:ppat_tuple-pattern-list (ast:pattern-ppat_desc pat))))])
				      #`(#,head . #,tail))
				    (pretty-print (list "Not a tuple: " (ast:pattern-ppat_desc pat))))
				#`($ #,(string->symbol (eval (ast:lident-name name))) #,(if (null? pat) pat (get-varpat pat))))
			    #`($ #,(string->symbol (unlongident name)) #,(if (null? pat) #'dummy (get-varpat pat)))))]
		     [else (pretty-print (list "Unknown construct: " name pat bool))])]
	      [else
	       (let [(src-loc (ast:pattern-ppat_src pattern))]
		 (raise-syntax-error #f "Not a variable" pattern))]))

     (define (compile-test pattern context)
       (match (ast:pattern-ppat_desc pattern)
	      [($ ast:ppat_any dummy)
	       #'_]
	      [($ ast:ppat_var name)
	       (string->symbol name)]
	      [($ ast:ppat_constant const)
	       const]
	      [($ ast:ppat_tuple tlist)
	       (let ([tests (map compile-test tlist (repeat (length tlist)) (repeat context (length tlist)))])
	       #`($ tuple #,tests))]
	      [($ ast:ppat_construct name pat bool)
	       (cond [(and (null? pat) (not bool) (if (hash-table-get constructors name (lambda () #f)) #t #f))
		      (hash-table-get constructors name)]
		     [(not bool)
		      ;; Best way I can think of to do this is specail case
		      (if (equal? (hash-table-get constructors name) cons)
			  ;; The pattern should be a tuple of two
			  (if (ast:ppat_tuple? (ast:pattern-ppat_desc pat))
			      (let ([head (compile-test (car (ast:ppat_tuple-pattern-list (ast:pattern-ppat_desc pat))) context)]
				    [tail (compile-test (cadr (ast:ppat_tuple-pattern-list (ast:pattern-ppat_desc pat))) context)])
				#`(#,head . #,tail))
			      (pretty-print (list "Not a tuple: " (ast:pattern-ppat_desc pat))))
			  #`($ #,(string->symbol (eval (ast:lident-name name))) #,(if (null? pat) pat (compile-test pat context))))]
		     [else (pretty-print (list "Unknown construct: " name pat bool))])]
	      [else (pretty-print (list "Unknown pattern: " (ast:pattern-ppat_desc pattern)))]))


     (define (compile-function pelist context)
       (let ([patterns (map get-varpat (map car pelist))]
	     [exps (map compile-ml (map cdr pelist) (repeat context (length (map cdr pelist))))])
	 (with-syntax ([(pat ...) patterns]
		       [(expr ...) exps])
			    #`(match-lambda (pat expr) ...))))
;       (match (ast:pattern-ppat_desc (caar pelist))
;	      [($ ast:ppat_var variable)
;	       (match (cdar pelist)
;		      [($ ast:expression desc src)
;		       (match desc
;			      [($ ast:pexp_function label exp npelist)
;			       (let ([prev (compile-function npelist context)])
;				 (cons (cons (string->symbol variable) (car prev)) (cdr prev)))]
;			      [else
;			       (cons (list (string->symbol variable)) (compile-ml (cdar pelist) context))])])]
;	      [($ ast:ppat_tuple tlist)
;	       (let ([tests (map compile-test tlist (repeat (length tlist)) (repeat context (length tlist)))])
;		 (cons #'(dummy) #`(match dummy [($ tuple #,tests) #,(compile-ml (cdar pelist) context)])))]
;	      [($ ast:ppat_constraint pat type)
;	       (match (ast:pattern-ppat_desc pat)
;		      [($ ast:ppat_var variable)
;		       (match (cdar pelist)
;			      [($ ast:expression desc src)
;			       (match desc
;				      [($ ast:pexp_function label exp npelist)
;				       (let ([prev (compile-function npelist context)])
;					 (cons (cons (string->symbol variable) (car prev)) (cdr prev)))]
;				      [else
;				       (cons (list (string->symbol variable)) (compile-ml (cdar pelist) context))])])])]
;	      
;	      [else (pretty-print (list "Unknown function pattern: " (caar pelist)))]))

     (define (compile-define rec binding context)
       (if (and (ast:pexp_function? (ast:expression-pexp_desc (cdr binding)))
		(or (ast:ppat_var? (ast:pattern-ppat_desc (car binding)))
		    (and (ast:ppat_constraint? (ast:pattern-ppat_desc (car binding)))
			 (ast:ppat_var? (ast:pattern-ppat_desc (ast:ppat_constraint-pat (ast:pattern-ppat_desc (car binding))))))))
	   #`(define #,(string->symbol (eval (ast:ppat_var-name (ast:pattern-ppat_desc (if (ast:ppat_constraint? (ast:pattern-ppat_desc (car binding)))
										     (ast:ppat_constraint-pat (ast:pattern-ppat_desc (car binding)))
      (car binding))))))
	       #,(compile-ml (cdr binding) context)))
       (if rec
	   (pretty-print "This kind of expression is not allowed on right hand side of let rec")
	   (let ([varpat (get-varpat (car binding))]
		 [val (compile-ml (cdr binding) context)])
	     #`(match-define (#,varpat #,val)))))


     (define (compile-let rec bindings finalexpr context)
       (if (null? bindings)
	   (compile-ml finalexpr context)
	   (let* ([cur-bind (car bindings)])
	     (if (and (ast:pexp_function? (ast:expression-pexp_desc (cdr cur-bind))) (or (ast:ppat_var? (ast:pattern-ppat_desc (car cur-bind)))
											 (and (ast:ppat_constraint? (ast:pattern-ppat_desc (car cur-bind))) (ast:ppat_var? (ast:pattern-ppat_desc (ast:ppat_constraint-pat (ast:pattern-ppat_desc (car cur-bind))))))))
		 #`(#,(if rec #'letrec #'let) ([#,(string->symbol (eval (ast:ppat_var-name (ast:pattern-ppat_desc (car cur-bind))))) #,(compile-ml (cdr cur-bind) context)]) #,(compile-let rec (cdr bindings) finalexpr context))
		 (if rec
		     (pretty-print "This kind of expression is not allowed as right-hand side of `let rec'")
		     (let ([varpat (get-varpat (car cur-bind))]
			   [val (compile-ml (cdr cur-bind) context)])
		       #`(match #,val
				[#,varpat
				 #,(compile-let rec (cdr bindings) finalexpr context)])))))))


     (define (flatten-list flist)
       (if (null? flist)
	   null
	   (if (list? (car flist))
	       (append (flatten-list (car flist)) (flatten-list (cdr flist)))
	       (cons (car flist) (flatten-list (cdr flist))))))

     (define (flatten-tuple tlist)
       (if (null? tlist)
	   null
	   (if (tuple? (car tlist))
	       (append (flatten-tuple (tuple-list (car tlist))) (flatten-tuple (cdr tlist)))
	       (cons (car tlist) (flatten-tuple (cdr tlist))))))
       
     (define (compile-exps bindings context)
       (if (null? bindings)
	   null
	   (cons (compile-ml (car bindings) context) (compile-exps (cdr bindings) context))))

     (define (compile-lelist lelist context)
       (if (null? lelist)
	   null
	   (cons (compile-ml (cdar lelist) context) (compile-lelist (cdr lelist) context))))
	       

     (define (lookup-ident uname)
       (match uname
	      [($ ast:lident name)


	       (let ([result (hash-table-get built-in-and-user-funcs (eval name) (lambda () #f))])
		 (if result
		     (cdr result)
		     #f))]
	      [($ ast:ldot longident name)
	       (match longident
		      [($ ast:lident library)
		       (let ([lib-map (hash-table-get library-names (eval library) (lambda () #f))])
			 (if lib-map
			     (let ([function (hash-table-get lib-map (syntax-object->datum name) (lambda () #f))])
			       (if function
				   (cdr function)
				   (begin (pretty-print (list "Error: " (syntax-object->datum name) "not found in" (eval library))) #f)))
			     (begin (pretty-print (list "Error: " library "not found")) #f)))])]))

     (define (look-up name context)
       (if (null? context)
	   #f
	   (if (string=? (caar context) name)
	       (caddar context)
	       (look-up name (cdr context)))))
	  
     (define (unlongident uname)
       (match uname
	      [($ ast:lident name) (eval name)]
	      [($ ast:ldot longident name) (format "~a.~a" (unlongident longident) (eval name))]))
     
     (define (ml-primitive? fun primlist)
       (if (null? primlist)
	   #f
	   (or (equal? fun (car primlist)) (ml-primitive? fun (cdr primlist)))))

     (define ml-prims (list + - * / equal? = < <= > >= <or> <and> != not append string-append)) 

     (define (empty-context) 
       '(("+" operator +)
	 ("+." operator +)
	 ("-" operator -)
	 ("-." operator -)
	 ("*" operator *)
	 ("*." operator *)
	 ("/" operator /)
	 ("/." operator /)
	 ("=" operator equal?)
	 ("==" operator =)
	 ("<" operator <)
	 ("<=" operator <=)
	 (">" operator >)
	 (">=" operator >=)
	 ("or" operator or)
	 ("&&" operator and)
	 ("!=" operator !=)
	 ("not" operator not)
	 ("true" constructor #t)
	 ("false" constructor #f)
	 ("[]" constructor ())
	 ("::" constructor cons)
	 ("float" constructor float?)
	 ("int" constructor int?)
	 ("bool" constructor boolean?)
	 ("string" constructor string?)
	 ("char" constructor char?)
	 ("@" operator append)
	 ("^" operator string-append)))
)