;; Shriram, then Moy, then Robby, then Shriram, then John

; Aries adds begin, begin0 and let's to the transformed source.

; Dependencies:
;   ariesus.ss
;   pretty.ss  [for debugging]


    (unit/sig plt:aries^
      (import [z : zodiac:system^]
	[z:interface : zodiac:interface^])

      (define w-c-m-key (gensym))
      
      (define error-box
	(box #f))

      (define make-improper
	(lambda (combine)
	  (rec improper ;; `rec' is for the name in error messages
	       (lambda (f list)
		 (let improper-loop ([list list])
		   (cond
		     ((null? list) list)
		     ((pair? list) (combine (f (car list))
					    (improper-loop (cdr list))))
		     (else (f list))))))))
      (define improper-map (make-improper cons))
      (define improper-foreach (make-improper (lambda (x y) y)))

      ; Robby's old definition, commented out:
    
      (quote
	(define unparse-read
	  (lambda (read)
	    (cond
	      [(z:improper-list? read)
		(let loop ([l (z:read-object read)])
		  (cond
		    [(null? (cdr l)) (unparse-read (car l))]
		    [else (cons (unparse-read (car l)) (loop (cdr l)))]))]
	      [(z:vector? read)
		(apply vector (map unparse-read (z:read-object read)))]
	      [(z:list? read) (map unparse-read (z:read-object read))]
	      [else (z:read-object read)]))))

      ; Objects that are passed to eval get quoted by M3.  These objects
      ; do not belong in the `read' structure framework.  Hence, if they
      ; are passed to z:sexp->raw, they will error.  Thus, we first check
      ; before sending things there.

      ; jbc additional comments, including elucidation from shriram:
      ; there are three `levels' of parsed stuff:
      ; raw: simple, unannotated scheme values
      ; sexp: simple scheme values with attached zodiac information
      ; parsed: fully parsed into zodiac structures
  
      (define read->raw
	(lambda (read)
	  (if (z:zodiac? read)
	    (z:sexp->raw read)
	    read)))

      (define unparse-read read->raw)

      ; The new wrap simply puts a w-c-m around an expression. This
      ; allows debugging to occur.
      
      (define wrap
	(lambda (zodiac body)
	  (let ([start (z:zodiac-start zodiac)]
		[finish (z:zodiac-finish zodiac)])
	    `(#%with-continuation-mark (#%quote ,w-c-m-key)
	      ,(z:make-zodiac #f start finish)
	      ,body))))

      ; check whether the supplied id is a keyword. if the id is a syntax or
      ; macro keyword, issue an error.  If disallow-procedures? is true, then
      ; we issue an error for _any_ use of a keyword. These procedures are used
      ; to prevent the program from redefining keywords.
      
      (define check-for-keyword/both
	(lambda (disallow-procedures?)
	  (lambda (id)
	    (let ([real-id
		   (cond
		     [(z:binding? id) (z:binding-orig-name id)]
		     [(z:top-level-varref? id) (z:varref-var id)]
		     [(z:bound-varref? id)
		      (z:binding-orig-name (z:bound-varref-binding id))]
		     [(z:symbol? id)
		      (z:read-object id)]
		     [else
		      (z:interface:internal-error id
						  "Given in check-for-keyword")])])
	      (when (and (keyword-name? real-id)
			 (or disallow-procedures?
			     (let ([gdv (global-defined-value real-id)])
			       (or (syntax? gdv)
				   (macro? gdv)))))
		(z:interface:static-error id "Invalid use of keyword ~s"
					  real-id))))))
      
      (define check-for-keyword (check-for-keyword/both #t))
      (define check-for-keyword/proc (check-for-keyword/both #f))

      ; paroptarglist-> ilist and arglist->ilist are used to recreate
      ; mzscheme sexp syntax from the parsed zodiac form, so that the
      ; resulting expression can be fed to mzscheme.
      
      (define paroptarglist->ilist
	(lambda (paroptarglist)
	  (let ((process-args
		  (lambda (element)
		    (if (pair? element)
		      (and (check-for-keyword (car element))
			(list (z:binding-var (car element))
			  (annotate (cdr element))))
		      (and (check-for-keyword element)
			(z:binding-var element))))))
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

      ; divined notes about the structure of an arglist.  Evidently, an arglist can
      ; take one of three forms:
      ; list-arglist : this arglist represents a simple list of arguments
      ; ilist-arglist : this arglist represents a list of arguments which uses 
      ;   `dot-notation' to separate the last element of the list
      ; sym-arglist : this arglist represents the `single argument with no 
      ;   parens' style of argument list.
      
      (define arglist->ilist
	(lambda (arglist)
	  (cond
	    ((z:list-arglist? arglist)
	      (z:arglist-vars arglist))
	    ((z:ilist-arglist? arglist)
	      (let loop ((vars (z:arglist-vars arglist)))
		(if (null? (cddr vars))
		  (cons (car vars) (cadr vars))
		  (cons (car vars) (loop (cdr vars))))))
	    ((z:sym-arglist? arglist)
	      (car (z:arglist-vars arglist)))
	    (else
	      (z:interface:internal-error arglist
		"Given to arglist->ilist")))))

      
      (define the-undefined-value (letrec ((x x)) x))

      (define-struct (undefined struct:exn) (id))
      (define signal-undefined (make-parameter #t))
      (define undefined-error-format
	 "Variable ~s referenced before definition or initialization")

      (define-struct (not-boolean struct:exn) (val))
      (define signal-not-boolean (make-parameter #f))
      (define not-boolean-error-format "if: value ~e is neither #t nor #f")

      ; there is a problem with Zodiac.  The problem is that Zodiac has not been
      ; distinguishing between top-level variables and those bound by unit clauses.
      ; this is an important distinction to make, because the variables bound by 
      ; unit clauses may take on the `undefined' value, whereas those bound as
      ; top-level variables will never require this check.  (If used before defined,
      ; these values are simply considered unbound.  To this end, Matthew has modified
      ; Zodiac to add a bit of information which aries can use to distinguish these 
      ; fields.  Currently, this information is stored in the `unit?' field of a 
      ; `top-level-varref/bind/unit' structure.  There are cleaner solutions, but
      ; this one fits well into the current state of the world.  This may change at
      ; some point in the future.  For the moment, here is the function which 
      ; distinguishes between these two types of binding:
      
      (define (is-unit-bound? varref)
	(and (z:top-level-varref/bind/unit? varref)
	     (z:top-level-varref/bind/unit-unit? varref)))

      ; translate-bound-varref is a short piece of code which would otherwise appear
      ; twice: it translates a bound variable, inserting the undefined-value-check if
      ; this parameter is enabled.  This duplication stems from the problems involving
      ; the miscategorization of unit-vars described above.
      
      (define (translate-bound-varref expr)
	(let ([v (z:varref-var expr)]
	      [real-v (z:binding-orig-name
		       (z:bound-varref-binding expr))])
	  (wrap expr
		(if (signal-undefined)
		    `(#%if (#%eq? ,v ,the-undefined-value)
		      (#%raise (,make-undefined
				,(format undefined-error-format real-v)
				((#%debug-info-handler))
				(#%quote ,v)))
		      ,v)
		    v))))
      
      ; the annotate function is the primary one in aries: it takes a parsed zodiac 
      ; AST, and constructs an SEXP which includes debugging information, and inserts
      ; assorted runtime checks, depending on the state of various parameters.
      
      (define annotate
	(lambda (expr)
	  (cond
	    [(z:bound-varref? expr)
	     (translate-bound-varref expr)]
	    
	    [(z:top-level-varref? expr)
	     (if (is-unit-bound? expr)
		 (translate-bound-varref expr)
		 (begin
		   (check-for-keyword/proc expr)		   
		   (wrap expr (z:varref-var expr))))]

	    [(z:app? expr)
	     (wrap expr 
		   (map annotate (cons (z:app-fun expr)
				       (z:app-args expr))))]

	    [(z:struct-form? expr)
	      `(#%struct
		 ,(if (z:struct-form-super expr)
		    (list (read->raw (z:struct-form-type expr))
		      (wrap (z:struct-form-super expr)
			(annotate (z:struct-form-super expr))))
		    (read->raw (z:struct-form-type expr)))
		 ,(map read->raw (z:struct-form-fields expr)))]

	    [(z:if-form? expr)
	     (if (signal-not-boolean)
		 (let ([if-test-v (gensym "if-test-v")])
		   (wrap expr
			 `(#%let ((,if-test-v ,(annotate (z:if-form-test expr))))
			   (#%if (#%boolean? ,if-test-v)
			    (#%if ,if-test-v
			     ,(annotate (z:if-form-then expr))
			     ,(annotate (z:if-form-else expr)))
			    (#%raise (,make-not-boolean
				      (#%format ,not-boolean-error-format
				       ,if-test-v)
				      ((#%debug-info-handler))
				      ,if-test-v))))))
		 (wrap expr 
		       `(#%if ,(annotate (z:if-form-test expr))
			 ,(annotate (z:if-form-then expr))
			 ,(annotate (z:if-form-else expr)))))]

	    [(z:quote-form? expr)
	      `(#%quote ,(read->raw (z:quote-form-expr expr)))]

	    [(z:begin-form? expr)
	      (wrap expr
		    `(#%begin
		      ,@(map annotate (z:begin-form-bodies expr))))]

	    [(z:begin0-form? expr)
	     (wrap expr
		   `(#%begin0
		     ,@(map annotate (z:begin0-form-bodies expr))))]
	
	    
	    [(z:let-values-form? expr)
	     (let ([bindings
		    (map (lambda (vars val)
			   (for-each check-for-keyword vars)
			   `(,(map z:binding-var vars)
			     ,(annotate val)))
			 (z:let-values-form-vars expr)
			 (z:let-values-form-vals expr))])
	       (wrap expr
		     `(#%let-values ,bindings
		       ,(annotate (z:let-values-form-body expr)))))]
	    
	    ; in MzScheme 100, there is no more letrec*-values.  letrec-values
	    ; has taken its place.  binding initializers are always evaluated
	    ; sequentially.  However, zodiac still uses a structure called 
	    ; letrec*-values, at least for a while.
	    
	    [(z:letrec*-values-form? expr)
	     (let ([bindings
		    (map (lambda (vars val)
			   (for-each check-for-keyword vars)
			   `(,(map z:binding-var vars)
			     ,(annotate val)))
			 (z:letrec*-values-form-vars expr)
			 (z:letrec*-values-form-vals expr))])
	       (wrap expr
		     `(#%letrec-values ,bindings
		       ,(annotate (z:letrec*-values-form-body expr)))))]

	    ; since define-values may only occur at the top level, we must
	    ; induce an inner continuation frame to attach our mark to. 
	    ; we do this using let-values.
	    
	    ; hmmm, I wrote that about three months ago, and now I'm not sure
	    ; it's necessary. JBC, 12/98
	    
	    [(z:define-values-form? expr)
	     (let* ([args (z:define-values-form-vars expr)]
		    [arg-temps (map (lambda (v) (gensym (z:varref-var v))) args)])				   
	       `(#%define-values
		 ,(map (lambda (v)
			 (check-for-keyword v)
			 (z:varref-var v))
		       (z:define-values-form-vars expr))
		 ,(wrap expr
			`(#%let-values ((,arg-temps
					 ,(annotate (z:define-values-form-val expr))))
			  (values ,@arg-temps)))))]

	    [(z:set!-form? expr)
	     (check-for-keyword (z:set!-form-var expr))	
	      (wrap expr
		    `(#%set! ,(z:varref-var (z:set!-form-var expr))
		      ,(annotate (z:set!-form-val expr))))]
	    
	    [(z:case-lambda-form? expr)
	     `(#%case-lambda
	       ,@(map (lambda (args body)
			(let ((args (arglist->ilist args)))
			  (improper-foreach check-for-keyword args)
			  `(,(improper-map z:binding-var args)
			    ,(annotate body))))
		      (z:case-lambda-form-args expr)
		      (z:case-lambda-form-bodies expr)))]

	    [(z:with-continuation-mark-form? expr)
	     (wrap expr
		   `(#%with-continuation-mark 
		     ,(annotate (z:with-continuation-mark-form-key expr))
		     ,(annotate (z:quote-form-expr (z:with-continuation-mark-form-val expr)))
		     ,(annotate expr)))]
	    
	    [(z:unit-form? expr)
	      (let ((imports (z:unit-form-imports expr))
		     (exports (map (lambda (export)
				     (list (z:varref-var (car export))
				       (z:read-object (cdr export))))
				(z:unit-form-exports expr)))
		     (clauses (map annotate (z:unit-form-clauses expr))))
		(for-each check-for-keyword imports)
		`(#%unit
		   (import ,@(map z:binding-var imports))
		   (export ,@exports)
		   ,@clauses))]

	    [(z:compound-unit-form? expr)
	      (let ((imports (map z:binding-var
			       (z:compound-unit-form-imports expr)))
		     (links (z:compound-unit-form-links expr))
		     (exports (z:compound-unit-form-exports expr)))
		(let
		  ((links
		     (map
		       (lambda (link-clause)
			 (let ((tag (read->raw (car link-clause)))
				(sub-unit (annotate (cadr link-clause)))
				(imports
				  (map (lambda (import)
					 (if (z:lexical-varref? import)
					   (z:varref-var import)
					   `(,(read->raw (car import))
					     ,(read->raw (cdr import)))))
				    (cddr link-clause))))
			   `(,tag (,sub-unit ,@imports))))
		       links))
		    (exports
		      (map
			(lambda (export-clause)
			  `(,(read->raw (car export-clause))
			     (,(read->raw (cadr export-clause))
			       ,(read->raw (cddr export-clause)))))
			exports)))
		  (let ((e `(#%compound-unit
			      (import ,@imports)
			      (link ,@links)
			      (export ,@exports))))
		    e)))]

	    [(z:invoke-unit-form? expr)
	      `(#%invoke-unit ,(annotate (z:invoke-unit-form-unit expr))
		 ,@(map z:varref-var	
		     (z:invoke-unit-form-variables expr)))]

	    [(z:invoke-open-unit-form? expr)
	      (let ((name-spec (z:invoke-open-unit-form-name-specifier
				 expr))
		     (unit (annotate
			     (z:invoke-open-unit-form-unit expr)))
		     (vars (map z:varref-var
			     (z:invoke-open-unit-form-variables expr))))
		(cond
		  ((null? name-spec)
		    `(#%invoke-open-unit ,unit))
		  ((not name-spec)
		    `(#%invoke-open-unit ,unit #f ,@vars))
		  ((symbol? name-spec)
		    `(#%invoke-open-unit ,unit ,name-spec ,@vars))
		  (else
		    (z:interface:internal-error name-spec
		      "given as name-spec for invoke-open-unit"))))]

	    [(z:interface-form? expr)
	      (let ((vars (z:interface-form-variables expr)))
		(for-each check-for-keyword vars)
		`(#%interface ,(map annotate
				 (z:interface-form-super-exprs expr))
		   ,@(map read->raw vars)))]

	    [(z:class*/names-form? expr)
	      `(#%class*/names
		 (,(z:binding-var (z:class*/names-form-this expr))
		   ,(z:binding-var (z:class*/names-form-super-init expr)))
		 ,(annotate (z:class*/names-form-super-expr expr))
		 ,(map annotate (z:class*/names-form-interfaces expr))
		 ,(paroptarglist->ilist (z:class*/names-form-init-vars expr))
		 ,@(map
		     (lambda (clause)
		       (cond
			 ((z:public-clause? clause)
			   `(public
			      ,@(map (lambda (internal export expr)
				       `((,(z:binding-var internal)
					  ,(read->raw export))
					 ,(annotate expr)))
				  (z:public-clause-internals clause)
				  (z:public-clause-exports clause)
				  (z:public-clause-exprs clause))))
			 ((z:override-clause? clause)
			   `(override
			      ,@(map (lambda (internal export expr)
				       `((,(z:binding-var internal)
					  ,(read->raw export))
					 ,(annotate expr)))
				  (z:override-clause-internals clause)
				  (z:override-clause-exports clause)
				  (z:override-clause-exprs clause))))
			 ((z:private-clause? clause)
			   `(private
			      ,@(map (lambda (internal expr)
				       `(,(z:binding-var internal)
					  ,(annotate expr)))
				  (z:private-clause-internals clause)
				  (z:private-clause-exprs clause))))
			 ((z:inherit-clause? clause)
			   `(inherit
			      ,@(map (lambda (internal inherited)
				       `(,(z:binding-var internal)
					  ,(read->raw inherited)))
				  (z:inherit-clause-internals clause)
				  (z:inherit-clause-imports clause))))
			 ((z:rename-clause? clause)
			   `(rename
			      ,@(map (lambda (internal import)
				       `(,(z:binding-var internal)
					  ,(read->raw import)))
				  (z:rename-clause-internals clause)
				  (z:rename-clause-imports clause))))
			 ((z:sequence-clause? clause)
			   `(sequence
			      ,@(map annotate
				  (z:sequence-clause-exprs clause))))))
		     (z:class*/names-form-inst-clauses expr)))]

	    [else
	      (print-struct #t)
	      (z:interface:internal-error
		expr
		(format "aries:annotate: unknown object to annotate, ~a~n" expr))])))

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
			  [annotated (annotate expanded)])
		    (begin ((global-defined-value 'pretty-print) annotated)
			   (newline))
		    (read-loop (cons annotated exprs))))))))))
