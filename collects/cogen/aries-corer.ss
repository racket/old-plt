;; Shriram, then Moy, then Robby, then Shriram, then John

; Aries adds begin, begin0 and let's to the transformed source.

; Dependencies:
;   ariesus.ss
;   pretty.ss  [for debugging]


(unit/sig plt:aries^
  (import [z : zodiac:system^]
	  [z:interface : zodiac:interface^]
          [utils : cogen-utils^])
  
  (define signal-not-boolean utils:signal-not-boolean)
  (define signal-undefined utils:signal-undefined)
  
  (define w-c-m-key (gensym))
  
  (define error-box
    (box #f))
  
  (define unparse-read utils:read->raw)
  
  ; The new wrap simply puts a w-c-m around an expression. This
  ; allows debugging to occur.
  
  (define wrap
    (lambda (zodiac body)
      (let ([start (z:zodiac-start zodiac)]
	    [finish (z:zodiac-finish zodiac)])
	`(#%with-continuation-mark (#%quote ,w-c-m-key)
	  ,(z:make-zodiac #f start finish)
	  ,body))))
  
  ; extract-zodiac-location takes a continuation-mark-set
  ; and returns the innermost (most recent) location. It returns #f if
  ; the list is empty.
  
  (define (extract-zodiac-location mark-set)
    (let ([mark-list (continuation-mark-set->list mark-set w-c-m-key)])
      (if (null? mark-list)
          #f
          (car mark-list))))
  
  ; break is not defined in the old version of aries to do anything useful
  
  (define (break)
    (raise 'silly-rabbit-trix-are-for-kids))
  
  ; paroptarglist-> ilist and arglist->ilist are used to recreate
  ; mzscheme sexp syntax from the parsed zodiac form, so that the
  ; resulting expression can be fed to mzscheme.
  
  (define paroptarglist->ilist
    (lambda (paroptarglist)
      (let ((process-args
	     (lambda (element)
	       (if (pair? element)
		   (and (utils:check-for-keyword (car element))
			(list (get-binding-name (car element))
			      (annotate/inner (cdr element))))
		   (and (utils:check-for-keyword element)
			(get-binding-name element))))))
	(cond
	  ((z:sym-paroptarglist? paroptarglist)
	   (process-args (car (z:paroptarglist-vars paroptarglist))))
	  ((z:list-paroptarglist? paroptarglist)
	   (map process-args (z:paroptarglist-vars paroptarglist)))
	  ((z:ilist-paroptarglist? paroptarglist)
	   (let loop ((vars (map process-args 
				 (z:paroptarglist-vars paroptarglist))))
	     (if (null? (cddr vars))
		 (cons (car vars) (cadr vars))
		 (cons (car vars) (loop (cdr vars))))))
	  (else
	   (z:interface:internal-error paroptarglist
				       "Given to paroptarglist->ilist"))))))
  
  ; get-binding-name extracts the S-expression name for a binding. Zodiac
  ; creates a unique, gensym'd symbol for each binding, but the name is
  ; unreadable. Here, we create a new gensym, but the name of the generated
  ; symbol prints in the same way as the original symbol.
  
  (define (get-binding-name binding)
    (let ([name (lookup-new-binding-name binding)])
      (or name
	  (let* ([orig-name (z:binding-orig-name binding)]
		 [name (string->uninterned-symbol (symbol->string orig-name))])
	    (set-new-binding-name! binding name)
	    name))))

  (define-values (lookup-new-binding-name set-new-binding-name!)
    (let-values ([(getter setter) (z:register-client 'aries:new-name (lambda () #f))])
      (values
       (lambda (parsed) (getter (z:parsed-back parsed)))
       (lambda (parsed n) (setter (z:parsed-back parsed) n)))))
  
  ; translate-bound-varref is a short piece of code which would otherwise appear
  ; twice: it translates a bound variable, inserting the undefined-value-check if
  ; this parameter is enabled.  This duplication stems from the problems involving
  ; the miscategorization of unit-vars described above.
  
  (define (translate-bound-varref expr maybe-undef?)
    (let ([v (if (z:top-level-varref? expr)
		 (z:varref-var expr)
		 (get-binding-name (z:bound-varref-binding expr)))]
	  [real-v (if (z:top-level-varref? expr)
		      (z:varref-var expr)
		      (z:binding-orig-name
		       (z:bound-varref-binding expr)))])
      (if (and maybe-undef? (utils:signal-undefined))
	  (wrap expr
		`(#%if (#%eq? ,v ,utils:the-undefined-value)
		       (#%raise (,utils:make-undefined
				 ,(format utils:undefined-error-format real-v)
				 (#%current-continuation-marks)
				 (#%quote ,v)))
		       ,v))
	  ; don't wrap lexical variables - nothing can go wrong
	  v)))

  (define (translate-bound-varref/lhs expr)
    ; Right now, translate-bound-varref adds no annotation
    ;  if the seconrd argument is #f.
    (translate-bound-varref expr #f))

  ; mark lexical variables that are known never to be undefined, an
  ; important optimization when (signal-undefined) is #t

  (define-values (never-undefined? mark-never-undefined)
    (let-values ([(getter setter) (z:register-client 'aries:never-undefined (lambda () #f))])
      (values
       (lambda (parsed) (getter (z:parsed-back parsed)))
       (lambda (parsed) (setter (z:parsed-back parsed) #t)))))
  
  ; the annotate function is the primary one in aries: it takes a parsed zodiac 
  ; AST, and constructs an SEXP which includes debugging information, and inserts
  ; assorted runtime checks, depending on the state of various parameters. annotate/top-level
  ; and annotate/inner are wrappers for the annotate/both function. Externally, annotate/top-level
  ; must be defined as simply annotate. annotate assumes that it is called in a context
  ; where top-level-defines are legal, and thus that a) it is okay not to wrap begins and
  ; defines with a continuation mark, and b) it is important not to, in case a begin contains 
  ; a `top-level' define.
  
  (define annotate/both
    (lambda (top-level-env)
      (lambda (expr ignored)
	(cond
	  [(z:bound-varref? expr)
	   (translate-bound-varref 
	    expr
	    (not (never-undefined? (z:bound-varref-binding expr))))]
	  
	  [(z:top-level-varref? expr)
	   (if (utils:is-unit-bound? expr)
	       (translate-bound-varref expr #t)
	       (begin
		 (utils:check-for-keyword/proc expr)		   
		 (wrap expr (z:varref-var expr))))]
	  
	  [(z:app? expr)
	   (wrap expr 
		 (map annotate/inner (cons (z:app-fun expr)
					   (z:app-args expr))))]
	  
	  [(z:struct-form? expr)
	   (let ([transformed
		  `(#%struct
		    ,(if (z:struct-form-super expr)
			 (list (utils:read->raw (z:struct-form-type expr))
			       (annotate/inner (z:struct-form-super expr)))
			 (utils:read->raw (z:struct-form-type expr)))
		    ,(map utils:read->raw (z:struct-form-fields expr)))])
	     (if (z:struct-form-super expr)
		 (wrap expr transformed)
		 transformed))]
	  
	  [(z:if-form? expr)
	   (if (signal-not-boolean)
	       (let ([if-test-v (gensym "if-test-v")])
		 (wrap expr
		       `(#%let ((,if-test-v ,(annotate/inner (z:if-form-test expr))))
			 (#%if (#%boolean? ,if-test-v)
			  (#%if ,if-test-v
			   ,(annotate/inner (z:if-form-then expr))
			   ,(annotate/inner (z:if-form-else expr)))
			  (#%raise (,utils:make-not-boolean
				    (#%format ,utils:not-boolean-error-format
				     ,if-test-v)
				    (#%current-continuation-marks)
				    ,if-test-v))))))
	       (wrap expr 
		     `(#%if ,(annotate/inner (z:if-form-test expr))
		       ,(annotate/inner (z:if-form-then expr))
		       ,(annotate/inner (z:if-form-else expr)))))]
	  
	  [(z:quote-form? expr)
	   `(#%quote ,(utils:read->raw (z:quote-form-expr expr)))]
	  
	  ; we don't wrap begin forms at top level
	  [(z:begin-form? expr)
	   (if top-level-env
	       `(#%begin ,@(map annotate/top-level (z:begin-form-bodies expr)))
	       (wrap expr
		     `(#%begin
		       ,@(map annotate/inner (z:begin-form-bodies expr)))))]
	  
	  [(z:begin0-form? expr)
	   (wrap expr
		 `(#%begin0
		   ,@(map annotate/inner (z:begin0-form-bodies expr))))]
	  
	  
	  [(z:let-values-form? expr)
	   (let ([bindings
		  (map (lambda (vars val)
			 (for-each utils:check-for-keyword vars)
			 (for-each mark-never-undefined vars)
			 `(,(map get-binding-name vars)
			   ,(annotate/inner val)))
		       (z:let-values-form-vars expr)
		       (z:let-values-form-vals expr))])
	     (wrap expr
		   `(#%let-values ,bindings
		     ,(annotate/inner (z:let-values-form-body expr)))))]
	  
	  ; in MzScheme 100, there is no more letrec*-values.  letrec-values
	  ; has taken its place.  binding initializers are always evaluated
	  ; sequentially.  However, zodiac still uses a structure called 
	  ; letrec*-values, at least for a while.
	  
	  [(z:letrec*-values-form? expr)
	   ; Are all RHSes values? ...
	   (when (andmap z:case-lambda-form? (z:letrec*-values-form-vals expr))
	     ; ...yes , mark vars as never undefined.
	     ; (We do this before annotating any RHS!)
	     (for-each (lambda (vars)
			 (for-each mark-never-undefined vars))
		       (z:letrec*-values-form-vars expr)))
	   (let* ([bindings
		   (map (lambda (vars val)
			  (for-each utils:check-for-keyword vars)
			  `(,(map get-binding-name vars)
			    ,(annotate/inner val)))
			(z:letrec*-values-form-vars expr)
			(z:letrec*-values-form-vals expr))])
	     (wrap expr
		   `(#%letrec-values ,bindings
		     ,(annotate/inner (z:letrec*-values-form-body expr)))))]
	  
	  ; we may not wrap a define-values form in a w-c-m expression; define-values
	  ; is legal _only_ at the top level (which includes being inside a `begin' at
	  ; top level), and thus if we put it inside a w-c-m expression, it's no longer
	  ; legal.  That's okay, because the source position of the define-values expr
	  ; can be inferred, and no additional variables occur free in the define-values
	  ; which do not occur in the body.
	  
	  [(z:define-values-form? expr)
	   `(#%define-values
	     ,(map (lambda (v)
		     (utils:check-for-keyword v)
		     (translate-bound-varref/lhs v))
		   (z:define-values-form-vars expr))
	     ,(annotate/inner (z:define-values-form-val expr)))]
	  
	  [(z:set!-form? expr)
	   (utils:check-for-keyword (z:set!-form-var expr))	
	   (wrap expr
		 `(#%set! ,(translate-bound-varref/lhs (z:set!-form-var expr))
		   ,(annotate/inner (z:set!-form-val expr))))]
	  
	  [(z:case-lambda-form? expr)
	   `(#%case-lambda
	     ,@(map (lambda (args body)
		      (let ((args (utils:arglist->ilist args)))
			(utils:improper-foreach utils:check-for-keyword args)
			(utils:improper-foreach mark-never-undefined args)
			`(,(utils:improper-map get-binding-name args)
			  ,(annotate/inner body))))
		    (z:case-lambda-form-args expr)
		    (z:case-lambda-form-bodies expr)))]
	  
	  [(z:with-continuation-mark-form? expr)
	   (wrap expr
		 `(#%with-continuation-mark 
		   ,(annotate/inner (z:with-continuation-mark-form-key expr))
		   ,(annotate/inner (z:with-continuation-mark-form-val expr))
		   ,(annotate/inner (z:with-continuation-mark-form-body expr))))]
	  
	  ; the unit-form introduces a new top-level context, and the unit-clauses
	  ; must be annotated accordingly.
	  
	  [(z:unit-form? expr)
	   (let ((imports (z:unit-form-imports expr))
		 (exports (map (lambda (export)
				 (list (translate-bound-varref/lhs (car export))
				       (z:read-object (cdr export))))
			       (z:unit-form-exports expr)))
		 (clauses (map annotate/top-level (z:unit-form-clauses expr))))
	     (for-each utils:check-for-keyword imports)
	     `(#%unit
	       (import ,@(map get-binding-name imports))
	       (export ,@exports)
	       ,@clauses))]
	  
	  [(z:compound-unit-form? expr)
	   (let ((imports (map get-binding-name
			       (z:compound-unit-form-imports expr)))
		 (links (z:compound-unit-form-links expr))
		 (exports (z:compound-unit-form-exports expr)))
	     (let
		 ((links
		   (map
		    (lambda (link-clause)
		      (let ((tag (utils:read->raw (car link-clause)))
			    (sub-unit (annotate/inner (cadr link-clause)))
			    (imports
			     (map (lambda (import)
				    (if (z:lexical-varref? import)
					(translate-bound-varref/lhs import)
					`(,(utils:read->raw (car import))
					  ,(utils:read->raw (cdr import)))))
				  (cddr link-clause))))
			`(,tag (,sub-unit ,@imports))))
		    links))
		  (exports
		   (map
		    (lambda (export-clause)
		      `(,(utils:read->raw (car export-clause))
			(,(utils:read->raw (cadr export-clause))
			 ,(utils:read->raw (cddr export-clause)))))
		    exports)))
	       (let ((e `(#%compound-unit
			  (import ,@imports)
			  (link ,@links)
			  (export ,@exports))))
		 (wrap expr e))))]
	  
	  [(z:invoke-unit-form? expr)
	   (wrap expr
		 `(#%invoke-unit ,(annotate/inner (z:invoke-unit-form-unit expr))
				 ,@(map translate-bound-varref/lhs
					(z:invoke-unit-form-variables expr))))]
	  
	  [(z:interface-form? expr)
	   (let ((vars (z:interface-form-variables expr)))
	     (for-each utils:check-for-keyword vars)
	     (wrap expr
		   `(#%interface ,(map annotate/inner
				       (z:interface-form-super-exprs expr))
				 ,@(map utils:read->raw vars))))]
	  
	  [(z:class*/names-form? expr)
	   (wrap expr
		 `(#%class*/names
		   (,(get-binding-name (z:class*/names-form-this expr))
		    ,(get-binding-name (z:class*/names-form-super-init expr)))
		   ,(annotate/inner (z:class*/names-form-super-expr expr))
		   ,(map annotate/inner (z:class*/names-form-interfaces expr))
		   ,(paroptarglist->ilist (z:class*/names-form-init-vars expr))
		   ,@(map
		      (lambda (clause)
			(cond
			 ((z:public-clause? clause)
			  `(public
			     ,@(map (lambda (internal export expr)
				      `((,(get-binding-name internal)
					 ,(utils:read->raw export))
					,(annotate/inner expr)))
				    (z:public-clause-internals clause)
				    (z:public-clause-exports clause)
				    (z:public-clause-exprs clause))))
			 ((z:override-clause? clause)
			  `(override
			     ,@(map (lambda (internal export expr)
				      `((,(get-binding-name internal)
					 ,(utils:read->raw export))
					,(annotate/inner expr)))
				    (z:override-clause-internals clause)
				    (z:override-clause-exports clause)
				    (z:override-clause-exprs clause))))
			 ((z:private-clause? clause)
			  `(private
			     ,@(map (lambda (internal expr)
				      `(,(get-binding-name internal)
					,(annotate/inner expr)))
				    (z:private-clause-internals clause)
				    (z:private-clause-exprs clause))))
			 ((z:inherit-clause? clause)
			  `(inherit
			    ,@(map (lambda (internal inherited)
				     `(,(get-binding-name internal)
				       ,(utils:read->raw inherited)))
				   (z:inherit-clause-internals clause)
				   (z:inherit-clause-imports clause))))
			 ((z:rename-clause? clause)
			  `(rename
			    ,@(map (lambda (internal import)
				     `(,(get-binding-name internal)
				       ,(utils:read->raw import)))
				   (z:rename-clause-internals clause)
				   (z:rename-clause-imports clause))))
			 ((z:sequence-clause? clause)
			  `(sequence
			     ,@(map annotate/inner
				    (z:sequence-clause-exprs clause))))))
		      (z:class*/names-form-inst-clauses expr))))]
	  
	  [else
	   (print-struct #t)
	   (z:interface:internal-error
	    expr
	    (format "aries:annotate: unknown object to annotate, ~a~n" expr))]))))
  
  (define annotate/top-level (annotate/both #t))
  (define annotate/inner (annotate/both #f))
  
  (define annotate (lambda (expr _) (annotate/top-level expr)))
  
  
  (define transform
    (lambda (port offset file)
      (let ([reader (z:read port
			    (z:make-location 1 1 offset file))])
	(let read-loop ([exprs null])
	  (let ([expr (reader)])
	    (printf "expr: ~s~n" expr)
	    (if (z:eof? expr)
		(apply values (reverse exprs))
		(let* ([expanded (z:scheme-expand expr)]
		       [_ (printf "expanded: ~s~n" expanded)]
		       [annotated (annotate/top-level expanded)])
		  (begin ((global-defined-value 'pretty-print) annotated)
			 (newline))
		  (read-loop (cons annotated exprs))))))))))
