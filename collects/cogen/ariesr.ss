;; Shriram, then Moy, then Robby, then Shriram

;; aries adds only begin and let to the transformed source

; Commented out these, they must be loaded by whoever loads this file:
;(plt:require-library "ariess.ss")
;(require-library "pretty.ss")

    (unit/sig plt:aries^
      (import [z : zodiac:system^]
	[z:interface : zodiac:interface^])

      (define error-box
	(box #f))

      (define improper-map
	(lambda (f list)
	  (cond
	    ((null? list) list)
	    ((pair? list) (cons (f (car list)) (improper-map f (cdr list))))
	    (else (f list)))))

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

      (define read->raw
	(lambda (read)
	  (if (z:zodiac? read)
	    (z:sexp->raw read)
	    read)))

      (define unparse-read read->raw)

      (define wrap
	(lambda (zodiac x)
	  (let ([start (z:zodiac-start zodiac)]
		 [finish (z:zodiac-finish zodiac)])
	    `(#%begin (,set-box! ,error-box
			,(z:make-zodiac #f start finish))
	       ,x))))

      (define check-for-keyword
	(lambda (id)
	  (let ((real-id
		  (cond
		    ((z:binding? id) (z:binding-orig-name id))
		    ((z:top-level-varref? id) (z:varref-var id))
		    ((z:bound-varref? id)
		      (z:binding-orig-name
			(z:bound-varref-binding id)))
		    ((z:symbol? id)
		      (z:read-object id))
		    (else
		      (z:interface:internal-error id
			"Given in check-for-keyword")))))
	    (when (keyword-name? real-id)
	      (z:interface:static-error id "Invalid use of keyword ~s"
		real-id)))))

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

      (define annotate
	(lambda (expr)
	  (cond
	    [(z:bound-varref? expr)
	      (z:varref-var expr)]

	    [(z:top-level-varref? expr)
	      (wrap expr (z:varref-var expr))]
	 
	    [(z:app? expr)
	      (let* ([aries:app-arg (gensym 'aries:app-arg)]
		      [aries:app-break (gensym 'aries:app-break)]
		      [last-arg (gensym 'last-arg)]
		      [fun-sym (gensym "fun")]
		      [args (map (lambda (x) `(,(gensym "arg")
						,(annotate x)))
			      (z:app-args expr))])
		`(#%let ([,fun-sym ,(annotate (z:app-fun expr))]
			  ,@args)
		   ,(wrap expr `(,fun-sym ,@(map car args)))))]

	    [(z:struct-form? expr)
	      `(#%struct
		 ,(if (z:struct-form-super expr)
		    (list (read->raw (z:struct-form-type expr))
		      (annotate (z:struct-form-super expr)))
		    (read->raw (z:struct-form-type expr)))
		 ,(map read->raw (z:struct-form-fields expr)))]

	    [(z:if-form? expr)
	      `(#%if ,(annotate (z:if-form-test expr))
		 ,(annotate (z:if-form-then expr))
		 ,(annotate (z:if-form-else expr)))]

	    [(z:quote-form? expr)
	      `(#%quote ,(unparse-read (z:quote-form-expr expr)))]

	    [(z:begin-form? expr)
	      `(#%begin
		 ,@(map annotate (z:begin-form-bodies expr)))]

	    [(z:begin0-form? expr)
	      `(#%begin0
		 ,@(map annotate (z:begin0-form-bodies expr)))]

	    [(z:let-values-form? expr)
	      (let ((bindings
		      (map (lambda (vars val)
			     (map check-for-keyword vars)
			     `(,(map z:binding-var vars)
				,(annotate val)))
			(z:let-values-form-vars expr)
			(z:let-values-form-vals expr))))
		`(#%let-values ,bindings
		   ,(annotate (z:let-values-form-body expr))))]

	    [(z:letrec*-values-form? expr)
	      (let ((bindings
		      (map (lambda (vars val)
			     (map check-for-keyword vars)
			     `(,(map z:binding-var vars)
				,(annotate val)))
			(z:letrec*-values-form-vars expr)
			(z:letrec*-values-form-vals expr))))
		`(#%letrec*-values ,bindings
		   ,(annotate (z:letrec*-values-form-body expr))))]

	    [(z:define-values-form? expr)
	      `(#%define-values
		 ,(map (lambda (v)
			 (check-for-keyword v)
			 (z:varref-var v))
		    (z:define-values-form-vars expr))
		 ,(annotate (z:define-values-form-val expr)))]

	    [(z:set!-form? expr)
	      (check-for-keyword (z:set!-form-var expr))
	      (let ([g (gensym "set!")])
		`(#%let ([,g ,(annotate (z:set!-form-val expr))])
		   ,(wrap expr 
		      `(#%set! ,(z:varref-var (z:set!-form-var expr))
			 ,g))))]

	    [(z:case-lambda-form? expr)
	      `(#%case-lambda
		 ,@(map (lambda (args body)
			  (let ((args (arglist->ilist args)))
			    (improper-map check-for-keyword args)
			    `(,(improper-map z:binding-var args)
			       ,(annotate body))))
		     (z:case-lambda-form-args expr)
		     (z:case-lambda-form-bodies expr)))]

	    [(z:unit-form? expr)
	      (let ((imports (z:unit-form-imports expr))
		     (exports (map (lambda (export)
				     (list (z:varref-var (car export))
				       (z:read-object (cdr export))))
				(z:unit-form-exports expr)))
		     (clauses (map annotate (z:unit-form-clauses expr))))
		(map check-for-keyword imports)
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
					   (annotate import)
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
		  `(#%compound-unit
		     (import ,@imports)
		     (link ,@links)
		     (export ,@exports))))]

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
		(map check-for-keyword vars)
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
		(format "unknown object to annotate, ~a~n" expr))])))

      (define transform
	(lambda (port offset file)
	  (let ([reader (z:read port
			  (z:make-location 1 1 offset file))])
	    (let read-loop ([exprs null])
	      (let ([expr (reader)])
		'(printf "expr: ~s~n" expr)
		(if (z:eof? expr)
		  (apply values (reverse exprs))
		  (let* ([expanded (z:scheme-expand expr)]
			  [_ '(printf "expanded: ~s~n" expanded)]
			  [annotated (annotate expanded)])
		    '(begin ((global-defined-value 'pretty-print) annotated)
		       (newline))
		    (read-loop (cons annotated exprs))))))))))
