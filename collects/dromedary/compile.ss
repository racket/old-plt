#cs(module compile mzscheme
     (require "parse.ss"
	      "prims.ss"
	      (prefix ast: "ast.ss")
	      (lib "match.ss")
	      (lib "list.ss")
	      (lib "pretty.ss")
	      (lib "structure.ss"))

     (provide compile-all)

     (define (hash-table-getprintfunc ht key func)
       (begin
	 (pretty-print (format "hash-table-get ~a ~a" ht key))
	 (hash-table-get ht key func)))

     (define (hash-table-getprint ht key)
       (begin
	 (pretty-print (format "hash-table-get ~a ~a" ht key))
	 (hash-table-get ht key)))

     (define next-label 0)     
     (define loc #f)
     (define structure-provides null)
     (define structure-number 0)
     (define structure-list null)

     (define (build-src syn)
       (if syn
	   (cond
	    [(syntax? syn)
	     (list loc (syntax-line syn) (syntax-column syn) (syntax-position syn) (syntax-span syn))]
	    [(ast:src? syn)
	     (list loc (ast:src-line syn) (ast:src-col syn) (ast:src-pos syn) (ast:src-span syn))]
	    [else (error 'build-src (format "Given ~a" syn))])
	   #f))
     
     (define (at expr src)
	     (datum->syntax-object (current-compile-context) expr
				   (list loc
					 (ast:src-line src)
					 (ast:src-col src)
					 (ast:src-pos src)
					 (ast:src-span src))))

     (define stx-for-original-property (read-syntax #f (open-input-string "original")))
     (define create-syntax
       (lambda (oddness sexpression source)
	 (datum->syntax-object oddness sexpression source stx-for-original-property)))
     (define make-syntax
       (lambda (oddness sexpression source)
	 (datum->syntax-object oddness sexpression source)))

     (define translate-id
       (lambda (id src)
	 (create-syntax #f (build-identifier id) (build-src src))))

     (define build-identifier
       (lambda (name)
	 (cond
	  ((symbol? name) name)
	  ((string? name) (string->symbol name))
	  ((procedure? name) name)
	  (else 
	   (error 'build-identifier (format "Given ~s" name))
	   name))))

     (define (compile-all stmt location)
       (set! loc location)
       (set! structure-provides null)
       (set! structure-number 0)
       (<flatten> (list
;	(datum->syntax-object
;	 #f
	 (let ([result (compile-ml stmt (empty-context))])
;	   (pretty-print "Compile successful")
	   ;(pretty-print (format "initial progval: ~a" result))
	    result))))
;	 #f)))

     (define current-compile-context (make-parameter #f))

     (define (compile-ml stmt context)
;       (pretty-print (format "compile-ml: ~a" stmt))
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
	      [($ ast:pstr_value rec_flag pelist lsrc)
	       (begin
		 (set! structure-provides null)
		 (let-values ([(binding exprs)
			       (if rec_flag
				   (let* ([all-bindings (map compile-rec-define pelist (repeat context (length pelist)) (repeat lsrc (length pelist)))]
					  [vars (map car all-bindings)]
					  [exps (map cdr all-bindings)])
				     (values
				      (with-syntax ([(lhs ...) vars]
						    [(rhs ...) exps])
						   #`(define-values #,(map datum->syntax-object (repeat (current-compile-context) (length vars)) vars) (match-letrec ((lhs rhs) ...) (values #,@vars))))
				      exps))
				   
				   
				   (let* ([all-bindings (map compile-define pelist (repeat context (length pelist)) (repeat lsrc (length pelist)))]
					  [fall-bindings (<flatten> all-bindings)])
				     (values
				      #`(begin #,@(map car fall-bindings))
				      (map cdr fall-bindings)))
				   )])
			     (begin
;		     (pretty-print (format "structure-provides: ~a" structure-provides))
;		     (pretty-print (format "all bindings: ~a" all-bindings))
			       (set! structure-number (+ 1 structure-number))
		     
			       #`(begin
				   (structure #,(datum->syntax-object (current-compile-context) (string->symbol (format "<foo~a>" structure-number))) #,(map datum->syntax-object (repeat (current-compile-context) (length structure-provides)) (map string->symbol structure-provides)) #,binding)
		       (open #,(datum->syntax-object (current-compile-context) (string->symbol (format "<foo~a>" structure-number))))
		       #,@exprs
;		       (make-<voidstruct> #f))
		   ))))]
;		 all-bindings)))]
	      [($ ast:pstr_type stdlist)
	       (map compile-typedecl stdlist)]
	      [($ ast:pstr_eval expr lsrc)
	       (if lsrc
		   #`((#,(create-syntax #f `lambda (build-src lsrc)) () #, (compile-ml expr context)))
		   (compile-ml expr context))]              
;	       #`((#,(create-syntax #f `lambda (if lsrc (build-src lsrc) #f)) () #,(compile-ml expr context)))]
	      [($ ast:pstr_exception name decl)
	       #`(begin (define-struct (#,(datum->syntax-object (current-compile-context) (string->symbol (syntax-object->datum name))) ml-exn) #,(if (null? decl)
																		   #`()
																		   #`(param)))
			  

;(string->symbol (format "exn:~a" (syntax-object->datum name)))) exn) ())
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

     (define (compile-typedecl td)
	       (let ([name (syntax-object->datum (car td))]
		     [typedecl (cdr td)])
		 (match (ast:type_declaration-kind typedecl)
			[($ ast:ptype_abstract dummy)
;			 (let ([core (compile-core_type (ast:type_declaration-manifest typedecl))])
;			   (begin
;			     (hash-table-put! user-types name core)
			     #'(make-<voidstruct> #f)]
			[($ ast:ptype_variant scll)
;			 (let ([cscll (compile-scll scll)])
;			   (begin
;			     (hash-table-put! user-types name (lambda (x) (isa-variant? 'a 'b cscll)))
;			     (pretty-print "begining type definitions")
			     #`(begin #,@(mkdefinestructs scll) (make-<voidstruct> #f))]
;			     #`(define-struct #,(string->symbol(eval (caar scll))) (tlist))))]
			 )))

     (define (mkdefinestructs scll)
       (if (null? scll)
	   null
	   (cons #`(define-struct (#,(datum->syntax-object (current-compile-context) (string->symbol (syntax-object->datum (caar scll)))) <user-type>) (tlist) (make-inspector)) (mkdefinestructs (cdr scll)))))

     (define (mkdefine binding)
       #`(define #,(car binding) #,(cdr binding)))

     (define (compile-core_type ct)
       (match (ast:core_type-desc ct)
	      [($ ast:ptyp_any dummy)
	       any?]
	      [($ ast:ptyp_constr name ctl)
	       (let ([res (hash-table-get <constructors> (unlongident name) (lambda () #f))])
		 (if res
		     res
		     (pretty-print (format "Not found in constructors: ~a" (unlongident name)))))]
	      [else
	       (pretty-print (list "Bad core type" ct))]))

     (define (compile-scll scll)
       (if (null? scll)
	   null
	   (let* ([current (car scll)]
		  [name (syntax-object->datum (car current))]
		  [compiled-types (map compile-core_type (cdr current))])
	     (cons (lambda (xname xtypes) (and (string=? name xname) (= (length (compiled-types)) (length (xtypes))) (correct-types? compiled-types xtypes)))
		   (compile-scll (cdr scll))))))

     (define (isa-variant? name type variants)
       (if (null? variants)
	   #f
	   (if ((car variants) name type)
	       #t
	       (isa-variant? name type (cdr variants)))))

 

     (define (correct-types? types x)
       (if (null? x)
	   #t
	   (and ((car types) (car x)) (correct-types? (cdr types) (cdr x)))))

     (define (compile-expr desc src context)
       (match desc
	      [($ ast:pexp_constant const)
	       const]
	      [($ ast:pexp_array expr-list)
	       #`(vector #,@(map compile-ml expr-list (repeat context (length expr-list))))]
	      [($ ast:pexp_apply proc lelist)
	       (let ([fun (compile-ml proc context)]
		     [args (compile-lelist lelist context)])
;		 (if (ml-primitive? (syntax-object->datum fun) ml-prims)
;		     #`(#,fun #,@args)
		     (compile-apply fun (reverse args)))]

	      [($ ast:pexp_let rec bindings expr ksrc isrc)
	       (compile-let rec bindings expr context ksrc isrc)]
;	       (let* ([all-bindings (compile-bindings bindings context)]
;		      [lhss (map car all-bindings)]
;		      [rhss (map cdr all-bindings)])
;		 (with-syntax ([(lhs ...) lhss]
;			       [(rhs ...) rhss])
;			      #`(#,(if rec #'letrec #'let) ((lhs rhs) ...) #,(compile-ml expr context))))]
	       
			       


	      [($ ast:pexp_ident name)
;	       (datum->syntax-object #f (lookup-ident name))]
;	       (pretty-print (format "Source for ~a is: ~a" (unlongident name) (longident-src name)))
	       (datum->syntax-object (current-compile-context)
				     (translate-id
				      (or (let ([res (lookup-ident name)])
					    (begin
;					      (pretty-print (format "lookup-ident result: ~a" res))
					      res))
					  (unlongident name))
				      (longident-src name)))]

	      [($ ast:pexp_function label expr pelist)
;; The parser makes only one-variable functions by default which is a pain
;; when it comes to application, so we need to figure out just how many
;; variables this function has
	       (compile-function pelist context)]
;	       (let ([varsandbody (compile-function pelist context)])
;		 #`(lambda #,(car varsandbody) #,(cdr varsandbody)))]
	      [($ ast:pexp_ifthenelse test ifexp elseexp ifsrc thensrc elsesrc)
	       (let ([testc (compile-ml test context)]
		     [ifexpc (compile-ml ifexp context)]
		     [elseexpc (if (null? elseexp) null (compile-ml elseexp context))])
		 #`(#,(create-syntax #f `if (build-src ifsrc)) #,testc ((#,(create-syntax #f `lambda (build-src thensrc)) () #,ifexpc)) #,(if (not (null? elseexpc)) #`((#,(create-syntax #f `lambda (build-src elsesrc)) () #,elseexpc)) (make-<unit>))))]
	      [($ ast:pexp_construct name expr bool)
	       (let ([constr (hash-table-get <constructors> (unlongident name) (lambda () #f))])
		 (if constr
		     (if (null? expr)
			 (begin
			   ;(pretty-print (format "constr found: ~a" constr))
			 (if (symbol? (cdr constr))
			     #`(#,(cdr constr) #f)
			     (create-syntax #f (cdr constr) (build-src (longident-src name))))
			 )
			 (let ([args (compile-expr (ast:expression-pexp_desc expr) (ast:expression-pexp_src expr) context)])
			   #`(#,(cdr constr) #,@(cond
						[(<tuple>? args) (<tuple>-list args)]
						[(list? args) args]
						[else (list args)]))))
		     (let ([rconstr (string->symbol (format "make-~a" (unlongident name)))]
			   [args (cond
				  [(null? expr) #f]
				  [(ast:pexp_tuple? expr) 
				   (compile-constr-args (ast:pexp_tuple-expression-list (ast:expression-pexp_desc expr)) context)]
				  [else (compile-ml expr context)])])
		       (if args
			   #`(#,rconstr #,args)
			       #`(#,rconstr #f)
			       ))))]
;	       (cond
;		[(and (null? expr) (not bool) 
;		      (if (hash-table-get <constructors> name (lambda () #f)) #t #f))
;		 (cdr (hash-table-get <constructors> name))]
;		[(and (not bool) (if (hash-table-get <constructors> name (lambda () #f)) #t #f))
;		 (let ([constr (cdr (hash-table-get <constructors> name))]
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
	       #`(make-<tuple> (#,list #,@(compile-exps xlist context)))]

	      [($ ast:pexp_try tryexp pelist)
	       (let ([matches (compile-match pelist)]
		     [tryres (compile-ml tryexp)])
		 #`(with-handlers ([(not-break-exn? (match-lambda #,@matches))]) #,tryres))]
				  
	      [($ ast:pexp_while testexpr bodyexpr)
	       (let ([testc (compile-ml testexpr context)]
		     [bodyc (compile-ml bodyexpr context)])
		 #`(let loop ()
		       (if #,testc
			   (begin
			     #,bodyc
			     (loop))
			   (make-<unit>))))]

	      [($ ast:pexp_for var init test up body)
	       (pretty-print (format "for testc: ~a" test))
	       (let ([initc (compile-ml init context)]
		     [testc (compile-ml test context)]
		     [bodyc (compile-ml body context)]
		     [rvar (string->symbol (syntax-object->datum var))])
		 #`(let loop ([#,rvar #,initc]) 
		     (if (= #,rvar #,testc)
			 (begin 
			   #,bodyc 
			   (loop #,(if up 
				       #`(+ #,rvar 1)
				       #`(- #,rvar 1))))
			 (make-<unit>))))]
	      [($ ast:pexp_sequence firstexpr restexpr)
	       #`(begin #,(compile-ml firstexpr context)
			#,(compile-ml restexpr context))]
	      [else
	       (pretty-print (list "Expression unknown: " desc))]))


     (define (compile-match pelist context)
       (if (null? pelist)
	   null
	   (cons #`[#,(get-varpat (caar pelist) #f) #,(compile-ml (cdar pelist) context)]
		 (compile-match (cdr pelist) context))))

     (define (repeat x n)
       (if (<= n 0)
	   null
	   (cons x (repeat x (- n 1)))))

     (define (compile-apply fun args)
;       (pretty-print (format "compile-apply ~a ~a" (syntax-object->datum fun) args))
       (cond 
	[(null? args) fun]
;	[(null? (cdr-args))
;	 #`(#,(compile-apply fun (make-<unit>)) #,(car args))]
	[else
	 #`(#,(compile-apply fun (cdr args)) #,(car args))]))



     (define (get-varpat pattern define?)
       (match (ast:pattern-ppat_desc pattern)
	      [($ ast:ppat_var variable)
	       (begin
		 (if define?
		     (if (null? (filter (lambda (x) (equal? x (syntax-object->datum variable)))  structure-provides))
			 (set! structure-provides (cons (syntax-object->datum variable) structure-provides))
			 structure-provides))
		 (translate-id (syntax-object->datum variable) variable))]
	      [($ ast:ppat_constant const)
	       (syntax-object->datum const)]
	      [($ ast:ppat_tuple tlist)
	       #`($ <tuple> #,(map get-varpat tlist (repeat define? (length tlist))))]
	      [($ ast:ppat_constraint pat ct)
	       (get-varpat pat define?)]
	      [($ ast:ppat_any dummy)
	       #'_]
	      [($ ast:ppat_construct name pat bool)
	       (cond [(and (null? pat) (not bool) (if (hash-table-get <constructors> (unlongident name) (lambda () #f)) #t #f))
		      (let ([cconstr (hash-table-get <constructors> (unlongident name))])
			(if (tconstructor? (car cconstr))
			    #`($ #,(string->symbol (unlongident name)) #f)
			    (cdr cconstr)))]
		     [(not bool)
		      (let ([constructor (hash-table-get <constructors> (unlongident name) (lambda () #f))])
			(if constructor
			    ;; Best way I can think of to do this is specail case
			    (begin
;			      (pretty-print (format "constructor found: ~a" constructor))
			    (if (equal? (cdr constructor) <cons>)
				;; The pattern should be a <tuple> of two
				(if (ast:ppat_tuple? (ast:pattern-ppat_desc pat))
				    (let ([head (get-varpat (car (ast:ppat_tuple-pattern-list (ast:pattern-ppat_desc pat))) define?)]
					  [tail (get-varpat (cadr (ast:ppat_tuple-pattern-list (ast:pattern-ppat_desc pat))) define?)])
				      #`(#,head . #,tail))
				    (pretty-print (list "Not a tuple: " (ast:pattern-ppat_desc pat))))
				#`($ #,(string->symbol (syntax-object->datum (ast:lident-name name))) #,(if (null? pat) pat (get-varpat pat define?))))
			    )
			    #`($ #,(string->symbol (unlongident name)) #,(if (null? pat) #'dummy (get-varpat pat define?)))))]
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
	       #`($ <tuple> #,tests))]
	      [($ ast:ppat_construct name pat bool)
	       (cond [(and (null? pat) (not bool) (if (hash-table-get <constructors> name (lambda () #f)) #t #f))
		      (hash-table-get <constructors> name)]
		     [(not bool)
		      ;; Best way I can think of to do this is specail case
		      (if (equal? (hash-table-get <constructors> name) <cons>)
			  ;; The pattern should be a <tuple> of two
			  (if (ast:ppat_tuple? (ast:pattern-ppat_desc pat))
			      (let ([head (compile-test (car (ast:ppat_tuple-pattern-list (ast:pattern-ppat_desc pat))) context)]
				    [tail (compile-test (cadr (ast:ppat_tuple-pattern-list (ast:pattern-ppat_desc pat))) context)])
				#`(#,head . #,tail))
			      (pretty-print (list "Not a tuple: " (ast:pattern-ppat_desc pat))))
			  #`($ #,(string->symbol (syntax-object->datum (ast:lident-name name))) #,(if (null? pat) pat (compile-test pat context))))]
		     [else (pretty-print (list "Unknown construct: " name pat bool))])]
	      [else (pretty-print (list "Unknown pattern: " (ast:pattern-ppat_desc pattern)))]))


     (define (compile-function pelist context)
       (let ([patterns (map get-varpat (map car pelist) (repeat #f (length (map car pelist))))]
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
;		 (cons #'(dummy) #`(match dummy [($ <tuple> #,tests) #,(compile-ml (cdar pelist) context)])))]
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

     (define (compile-rec-define binding context lsrc)
       (if (not (ast:pexp_function? (ast:expression-pexp_desc (cdr binding))))
	   (raise-syntax-error #f "This kind of expression is not allowed on right hand side of let rec" (at (cdr binding) (ast:expression-pexp_src (cdr binding))))
	   (let ([varpat (get-varpat (car binding) #t)]
		 [val (compile-ml (cdr binding) context)])
	     (cons varpat val))))

     (define (compile-define binding context lsrc)
	     (let ([varpat (get-varpat (car binding) #t)]
		   [val (compile-ml (cdr binding) context)])
		 (cons #`(#,(create-syntax #f `match-define (build-src lsrc)) #,varpat #,val)
		       val)))


     (define (compile-let rec bindings finalexpr context ksrc isrc)
       (if (null? bindings)
	   (compile-ml finalexpr context)
	   (let* ([cur-bind (car bindings)])
	     (if (and (ast:pexp_function? (ast:expression-pexp_desc (cdr cur-bind))) (or (ast:ppat_var? (ast:pattern-ppat_desc (car cur-bind)))
											 (and (ast:ppat_constraint? (ast:pattern-ppat_desc (car cur-bind))) (ast:ppat_var? (ast:pattern-ppat_desc (ast:ppat_constraint-pat (ast:pattern-ppat_desc (car cur-bind))))))))
		 (let ([name (ast:ppat_var-name (ast:pattern-ppat_desc (car cur-bind)))])
		   #`(#,(create-syntax #f (if rec `letrec `let) (build-src ksrc)) ([#,(translate-id (syntax-object->datum name) name)
;(string->symbol (eval (ast:ppat_var-name (ast:pattern-ppat_desc (car cur-bind)))))
						  #,(compile-ml (cdr cur-bind) context)]) ((#,(create-syntax #f `lambda (build-src isrc)) () #,(compile-let rec (cdr bindings) finalexpr context ksrc isrc)))
						  ))
		 (if rec
		     (pretty-print "This kind of expression is not allowed as right-hand side of `let rec'")
		     (let ([varpat (get-varpat (car cur-bind) #f)]
			   [val (compile-ml (cdr cur-bind) context)])
		       #`(#,(create-syntax #f `match (build-src ksrc)) 
			  ((#,(create-syntax #f `lambda (build-src isrc)) () #,val))
				[#,varpat
				 #,(compile-let rec (cdr bindings) finalexpr context ksrc isrc)])))))))


     (define (flatten-list flist)
       (if (null? flist)
	   null
	   (if (list? (car flist))
	       (append (flatten-list (car flist)) (flatten-list (cdr flist)))
	       (cons (car flist) (flatten-list (cdr flist))))))

     (define (flatten-tuple tlist)
       (if (null? tlist)
	   null
	   (if (<tuple>? (car tlist))
	       (append (flatten-tuple (<tuple>-list (car tlist))) (flatten-tuple (cdr tlist)))
	       (cons (car tlist) (flatten-tuple (cdr tlist))))))
       
     (define (compile-constr-args exps context)
;       (pretty-print (format "compile-contr-args ~a" exps))
       (let ([blen (length exps)])
	 (cond
	  [(= blen 0) #f]
	  [(= blen 1) (compile-ml (car exps) context)]
	  [else #`(make-<tuple> (list #,@(map compile-ml exps (repeat context (length exps)))))])))

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

	       (let ([result (hash-table-get built-in-and-user-funcs (syntax-object->datum name) (lambda () #f))])
		 (if result
		     (cdr result)
		     (lookup-ident (ast:make-ldot (ast:make-lident (datum->syntax-object #f "Pervasives")) name))
			   ))]
	      [($ ast:ldot longident name)
	       (match longident
		      [($ ast:lident library)
		       (let ([lib-map (hash-table-get <library-names> (syntax-object->datum library) (lambda () #f))])
			 (if lib-map
			     (let ([function (hash-table-get lib-map (syntax-object->datum name) (lambda () #f))])
			       (if function
				   (cdr function)
				  #f))
			     #f))])]))

     (define (look-up name context)
       (if (null? context)
	   #f
	   (if (string=? (caar context) name)
	       (caddar context)
	       (look-up name (cdr context)))))
	  
     (define (unlongident uname)
       (match uname
	      [($ ast:lident name) (syntax-object->datum name)]
	      [($ ast:ldot longident name) (format "~a.~a" (unlongident longident) (syntax-object->datum name))]))
     
     (define (longident-src uname)
       (match uname
	      [($ ast:lident name) (ast:make-src (syntax-line name)
						 (syntax-column name)
						 (syntax-position name)
						 (syntax-span name))]
	      [($ ast:ldot longident name)
	       (let [(fromsrc (longident-src longident))]
		 (ast:make-src (ast:src-line fromsrc)
			       (ast:src-col fromsrc)
			       (ast:src-pos fromsrc)
			       (- (+ (syntax-position name) (syntax-span name))
				  (ast:src-pos fromsrc))))]))
     
     (define (ml-primitive? fun primlist)
       (if (null? primlist)
	   #f
	   (or (equal? fun (car primlist)) (ml-primitive? fun (cdr primlist)))))



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