;; Shriram, then Moy, then Robby, then Shriram

;; aries adds only begin and let to the transformed source

(plt:require-library "ariess.ss")

(define plt:aries@
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

    (define unparse-read z:sexp->raw)

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
		  ((z:lexical-varref? id)
		    (z:binding-orig-name
		      (z:bound-varref-binding id)))
		  (else
		    (z:interface:internal-error id
		      "Given in check-for-keyword")))))
	  (when (keyword-name? real-id)
	    (z:interface:static-error id "Invalid use of keyword")))))

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
	  [(z:lexical-varref? expr)
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
		  (list (z:sexp->raw (z:struct-form-type expr))
		    (annotate (z:struct-form-super expr)))
		  (z:sexp->raw (z:struct-form-type expr)))
	       ,(map z:sexp->raw (z:struct-form-fields expr)))]

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
			  `(,(map z:binding-var args)
			     ,(annotate body))))
		   (z:case-lambda-form-args expr)
		   (z:case-lambda-form-bodies expr)))]

	  [(z:unit-form? expr)
	    (let ((imports (z:unit-form-imports expr))
		   (exports (map (lambda (export)
				   (list (z:read-object (car export))
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
		       (let ((tag (z:sexp->raw (car link-clause)))
			      (sub-unit (annotate (cadr link-clause)))
			      (imports
				(map (lambda (import)
				       (if (z:lexical-varref? import)
					 (annotate import)
					 `(,(z:sexp->raw (car import))
					    ,(z:sexp->raw (cdr import)))))
				  (cddr link-clause))))
			 `(,tag (,sub-unit ,@imports))))
		     links))
		  (exports
		    (map
		      (lambda (export-clause)
			(let ((tag (car export-clause))
			       (exports (map (lambda (export)
					       `(,(z:sexp->raw (car export))
						  ,(z:sexp->raw (cdr export))))
					  (cdr export-clause))))
			  `(,(z:sexp->raw tag) ,@exports)))
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

;	  [(z:class*-form? expr)
;	    ]

;	  [(z:ivar-form? expr)
;	    ]

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
		    (read-loop (cons annotated exprs)))))))))))
