#cs(module compile mzscheme
     (require "parse.ss"
	      "prims.ss"
	      (prefix ast: "ast.ss")
	      (lib "match.ss")
	      (lib "list.ss")
	      (lib "pretty.ss"))

     (provide compile-all)

     (define next-label 0)

     (define (compile-all stmt ctx)
;       (datum->syntax-object
;	ctx
;	(parameterize ([current-compile-context ctx])
		      (compile-ml stmt (empty-context)))

     (define current-compile-context (make-parameter #f))

     (define (compile-ml stmt context)
       (match stmt
	      [($ ast:expression desc src)
	       (compile-expr desc src context)]
	      [else
	       (pretty-print (list "Unknown: " stmt))]))

     (define (compile-expr desc src context)
       (match desc
	      [($ ast:pexp_constant const)
	       const]
	      [($ ast:pexp_apply proc lelist)
	       (let ([fun (compile-ml proc context)]
		     [args (compile-lelist lelist context)])
		 #`(#,fun #,@args))]

	      [($ ast:pexp_let rec bindings expr)
	       (let* ([all-bindings (compile-bindings bindings context)]
		      [lhss (map car all-bindings)]
		      [rhss (map cdr all-bindings)])
;	       (let ([lhss (map get-var (map ast:pattern-ppat_desc (map car bindings)))]
;		     [rhss (compile-exps (map cdr bindings) context)])
		 (with-syntax ([(lhs ...) lhss]
			       [(rhs ...) rhss])
			      #`(letrec ((lhs rhs) ...) #,(compile-ml expr context))))]
	       
			       


	      [($ ast:pexp_ident name)
	       (lookup-ident name context)]

	      [($ ast:pexp_function label expr pelist)
;; The parser makes only one-variable functions by default which is a pain
;; when it comes to application, so we need to figure out just how many
;; variables this function has
	       (let ([varsandbody (compile-function pelist context)])
		 #`(lambda #,(car varsandbody) #,(cdr varsandbody)))]
	      [($ ast:pexp_ifthenelse test ifexp elseexp)
	       (let ([testc (compile-ml test context)]
		     [ifexpc (compile-ml ifexp context)]
		     [elseexpc (if (null? elseexp) null (compile-ml elseexp context))])
		 #`(if #,testc #,ifexpc #,(if (not (null? elseexpc)) elseexpc)))]
	      [($ ast:pexp_construct name expr bool)
	       (cond
		[(and (null? expr) (not bool))
		 (lookup-ident name context)]
		[(and (not bool) (ast:pexp_tuple? (ast:expression-pexp_desc expr)))
		 (let ([constr (lookup-ident name context)]
		       [args (compile-exps (ast:pexp_tuple-expression-list (ast:expression-pexp_desc expr)) context)])
		   #`(#,constr #,@args))])]

	      [($ ast:pexp_match expr pelist)
	       (let ([totest (compile-ml expr context)]
		     [tests (compile-match pelist context)])
		 #`((match-lambda #,@tests) #,totest))]

	      [($ ast:pexp_tuple xlist)
	       #`(make-tuple (list #,@(compile-exps xlist context)))]

	      [else
	       (pretty-print (list "Expression unknown: " desc))]))


     (define (compile-match pelist context)
       (if (null? pelist)
	   null
	   (cons #`[#,(compile-test (caar pelist) context) #,(compile-ml (cdar pelist) context)]
		 (compile-match (cdr pelist) context))))

     (define (repeat x n)
       (if (<= n 0)
	   null
	   (cons x (repeat x (- n 1)))))

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
	       (cond [(and (null? pat) (not bool))
		      (lookup-ident name context)]
		     [(and (not (null? pat)) (not bool))
		      ;; Best way I can think of to do this is specail case
		      (if (equal? (lookup-ident name context) 'cons)
			  ;; The pattern should be a tuple of two
			  (if (ast:ppat_tuple? (ast:pattern-ppat_desc pat))
			      (let ([head (compile-test (car (ast:ppat_tuple-pattern-list (ast:pattern-ppat_desc pat))) context)]
				    [tail (compile-test (cadr (ast:ppat_tuple-pattern-list (ast:pattern-ppat_desc pat))) context)])
				#`(#,head . #,tail))
			      (pretty-print (list "Not a tuple: " (ast:pattern-ppat_desc pat))))
			  (pretty-print (list "Not cons: " (lookup-ident name context))))]
		     [else (pretty-print (list "Unknown construct: " name pat bool))])]
	      [else (pretty-print (list "Unknown pattern: " (ast:pattern-ppat_desc pattern)))]))


     (define (compile-function pelist context)
       (match (ast:pattern-ppat_desc (caar pelist))
	      [($ ast:ppat_var variable)
	       (match (cdar pelist)
		      [($ ast:expression desc src)
		       (match desc
			      [($ ast:pexp_function label exp npelist)
			       (let ([prev (compile-function npelist context)])
				 (cons (cons (string->symbol variable) (car prev)) (cdr prev)))]
			      [else
			       (cons (list (string->symbol variable)) (compile-ml (cdar pelist) context))])])]
	      [($ ast:ppat_tuple tlist)
	       (let ([tests (map compile-test tlist (repeat (length tlist)) (repeat context (length tlist)))])
		 (cons #'(dummy) #`(match dummy [($ tuple #,tests) #,(compile-ml (cdar pelist) context)])))]
	      [($ ast:ppat_constraint pat type)
	       (match (ast:pattern-ppat_desc pat)
		      [($ ast:ppat_var variable)
		       (match (cdar pelist)
			      [($ ast:expression desc src)
			       (match desc
				      [($ ast:pexp_function label exp npelist)
				       (let ([prev (compile-function npelist context)])
					 (cons (cons (string->symbol variable) (car prev)) (cdr prev)))]
				      [else
				       (cons (list (string->symbol variable)) (compile-ml (cdar pelist) context))])])])]
	      
	      [else (pretty-print (list "Unknown function pattern: " (caar pelist)))]))


     (define (compile-bindings bindings context)
       (if (null? bindings)
	   null
	   (let* ([cur-bind (car bindings)]
		  [var (get-var (car cur-bind))]
		  [val (compile-ml (cdr bindings) context)])
	     (if (list? var)
		 (append (map cons var (tuple-list val)) (compile-bindings (cdr bindings context)))
		 (cons (cons var val) (compile-bindings (cdr bindings context)))))))

     (define (get-var pattern)
       (match (ast:pattern-ppat_desc pattern)
	      [($ ast:ppat_var variable)
	       (string->symbol variable)]
	      [($ ast:ppat_tuple tlist)
	       (map get-var tlist)]
	      [($ ast:ppat_any dummy)
	       (begin
		 (set! next-label (+ 1 next-label))
		 (string->symbol (format "<dummy~a>" next-label)))]
	      [else
	       (pretty-print "Not a variable")]))

     (define (compile-exps bindings context)
       (if (null? bindings)
	   null
	   (cons (compile-ml (car bindings) context) (compile-exps (cdr bindings) context))))

     (define (compile-lelist lelist context)
       (if (null? lelist)
	   null
	   (cons (compile-ml (cdar lelist) context) (compile-lelist (cdr lelist) context))))
	       

     (define (lookup-ident uname context)
       (match uname
	      [($ ast:lident name)
	       (look-up name context)]
	      [($ ast:ldot longident name)
	       (match longident
		      [($ ast:lident library)
		       (let ([lib-map (hash-table-get library-names library (lambda () #f))])
			 (if lib-map
			     (let ([function (hash-table-get lib-map (syntax-object->datum name) (lambda () #f))])
			       (if function
				   function
				   (pretty-print (list "Error: " (syntax-object->datum name) "not found in" library))))
			     (pretty-print (list "Error: " library "not found"))))])]))

     (define (look-up name context)
       (if (null? context)
	   (begin
	     (datum->syntax-object #f (string->symbol name)))
	   (if (string=? (caar context) name)
	       (caddar context)
	       (look-up name (cdr context)))))

     (define (empty-context) 
       '(("+" operator +)
	  ("-" operator -)
	  ("-." operator -)
	  ("*" operator *)
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
	  ("@" operator append)
	  ("^" operator string-append)))
)