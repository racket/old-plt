;; Zodiac interface and library routines
;; (c)1996-7 Sebastian Good

(unit/sig compiler:zlayer^
  (import (compiler:option : compiler:option^)
	  (zodiac : zodiac:system^)
	  compiler:cstructs^
	  compiler:driver^
	  mzlib:function^
	  (mrspidey : compiler:mrspidey^))

  ;;----------------------------------------------------------------------------
  ;; ANNOTATIONS
  ;;
  ;; zodiac:* AST notes are annotated using set-annotation!, and
  ;; the annotations are extracted using get-annotation. Every
  ;; AST node has a single annotation, but the type of the annotation
  ;; depends on the type of the AST node.

  ; This is the default annotation value, used before the annotation
  ; is set for an AST node
  (define compiler:empty-annotation (gensym 'mzc-default-annotation))

  ; Create a new back-box for a new zodiac AST node
  (define (make-empty-box) (zodiac:make-empty-back-box))

  ; Manipulating annotations:
  ; NOTE: Zodiac must be invoked before this unit
  (define-values (get-annotation set-annotation!)
    (let-values ([(getter setter)
		  (zodiac:register-client 'compiler 
					  (lambda ()
					    compiler:empty-annotation))])
      (values
       (lambda (ast)
	 (getter (zodiac:parsed-back ast)))
       (lambda (ast obj)
	 (setter (zodiac:parsed-back ast) obj)))))
  (define (annotated? ast)
    (not (eq? (get-annotation ast)
	      compiler:empty-annotation)))
  (define (remove-annotation! ast)
    (set-annotation! ast compiler:empty-annotation))

  ;;----------------------------------------------------------------------------
  ;; Error handling

  (define compiler:escape-on-error (make-parameter #f))

  ;; initialize zodiac-error procedures
  (define zodiac-error-template
    (lambda (c s)
      (lambda (where fmt-spec . args)
	(c where 
	   (string-append s
			  (apply format (cons fmt-spec args))))
	(when (compiler:escape-on-error)
	  (error 'compiler "parsing error")))))
  
  (define (call-compiler:fatal-error . args)
    (apply compiler:fatal-error args))
  
  (define static-error 
    (zodiac-error-template call-compiler:fatal-error "(syntax) "))
  (define internal-error
    (zodiac-error-template call-compiler:fatal-error "(elaboration) "))
  (define dynamic-error
    (zodiac-error-template call-compiler:fatal-error "(parser dynamic) "))
  (define analysis-error
    (zodiac-error-template call-compiler:fatal-error "(MrSpidey) "))
  (define analysis-internal-error
    (zodiac-error-template call-compiler:fatal-error "(MrSpidey internal) "))
  

  ;;----------------------------------------------------------------------------
  ;; BEGIN0-FORM
  ;;
  ;;  maintain the illusion of a two slot begin0-form
  
  (define zodiac:begin0-form-first
    (compose car zodiac:begin0-form-bodies))
  (define zodiac:begin0-form-rest
    (compose cadr zodiac:begin0-form-bodies))
  (define zodiac:set-begin0-form-first!
    (lambda (ast v)
      (set-car! (zodiac:begin0-form-bodies ast) v)))
  (define zodiac:set-begin0-form-rest!
    (lambda (ast v)
      (set-car! (cdr (zodiac:begin0-form-bodies ast)) v)))
  
  ;;----------------------------------------------------------------------------
  ;; CLASS init args
  
  (define (class-init-defaults-map! ast f)
    (let ([args (zodiac:paroptarglist-vars 
		 (zodiac:class*/names-form-init-vars ast))])
      (let loop ([l args])
	(unless (null? l)
	  (when (pair? (car l)) 
	    (set-cdr! (car l)
		      (f (caar l) (cdar l))))
	  (loop (cdr l))))))
  
  ;;----------------------------------------------------------------------------
  ;; INVOKE and INVOKE-OPEN
  
  (define (zodiac:invoke-form? x)
    (or (zodiac:invoke-unit-form? x)
	(zodiac:invoke-open-unit-form? x)))
  
  (define (zodiac:invoke-form-unit ast)
    ((if (zodiac:invoke-unit-form? ast)
	 zodiac:invoke-unit-form-unit
	 zodiac:invoke-open-unit-form-unit)
     ast))
  (define (zodiac:set-invoke-form-unit! ast u)
    ((if (zodiac:invoke-unit-form? ast)
	 zodiac:set-invoke-unit-form-unit!
	 zodiac:set-invoke-open-unit-form-unit!)
     ast u))
  
  (define (zodiac:invoke-form-variables ast)
    ((if (zodiac:invoke-unit-form? ast)
	 zodiac:invoke-unit-form-variables
	 zodiac:invoke-open-unit-form-variables)
     ast))
  (define (zodiac:set-invoke-form-variables! ast v)
    ((if (zodiac:invoke-unit-form? ast)
	 zodiac:set-invoke-unit-form-variables!
	 zodiac:set-invoke-open-unit-form-variables!)
     ast v))

  ;;----------------------------------------------------------------------------
  ;; SPECIAL CONSTANTS
  ;;
  ;; some constants we don't know how to write, like #<void>
  ;;

  (define-struct (zodiac:void zodiac:struct:scalar) ())
  (define-struct (zodiac:undefined zodiac:struct:scalar) ())

  (define zodiac:make-void make-zodiac:void)
  (define zodiac:make-undefined make-zodiac:undefined)

  (define zodiac:make-special-constant
    ;; make-quote, make-constant
    (lambda (text)
      (let ([z zodiac:default-initial-location])
	(zodiac:make-quote-form 
	 z z z (make-empty-box)
	 (case text
	   [(void) (zodiac:make-void z z z text)]
	   [(null) (zodiac:make-list z z z null null null)]
	   [(undefined) (zodiac:make-undefined z z z text)]
	   [else (compiler:internal-error 'make-special-constant "bad type")])))))
  
  ;;-----------------------------------------------------------------------------
  ;; BINDING->LEXICAL-VARREF
  ;;
  ;; creates a zodiac:lexical-varref from a zodiac:binding
  ;;
  
  (define zodiac:binding->lexical-varref
    (lambda (ast)
      (let ([v (zodiac:make-lexical-varref (zodiac:zodiac-origin ast)
					   (zodiac:zodiac-start ast)
					   (zodiac:zodiac-finish ast)
					   (make-empty-box)
					   (zodiac:binding-var ast)
					   ast)])
	(set-annotation! v (varref:empty-attributes))
	v)))

  ;;----------------------------------------------------------------------------
  ;; POSITION REPORTING
  
  (define main-source-file (make-parameter #f))
  
  (define zodiac:print-start!
    (lambda (port ast)
      (let ([bad (lambda () (fprintf port " [?,?]: "))])
	(if (and ast (or (zodiac:zodiac? ast) (zodiac:location? ast)))
	    (let* ([start (or (and (zodiac:location? ast) ast)
			      (zodiac:zodiac-start ast))]
		   [good (lambda ()
			   (fprintf port " ~a[~a,~a]: "
				    (if (equal? (main-source-file) (zodiac:location-file start))
					""
					(format "~s " (zodiac:location-file start)))
				    (zodiac:location-line start)
				    (zodiac:location-column start)))])
	      (if (zodiac:location? start)
		  (good)
		  (begin	    
		    (when (compiler:option:debug)
		      (fprintf port "{~a had ~a for location info}" ast start))
		    (bad))))
	    (bad)))))
  

  ;;----------------------------------------------------------------------
  ;; Debugging: AST to annotated S-expression
  (define zodiac->sexp/annotate
    (lambda (ast)
      (let ([v (if #f
		   (mrspidey:get-annotations ast)
		   (mrspidey:SDL-type ast))])
	(if v
	    `(: ,(zodiac->sexp ast) ,v)
	    (zodiac->sexp ast)))))
  
  (define zodiac->sexp
    (lambda (ast)

      (cond 
       [(or (zodiac:quote-form? ast) 
	    (zodiac:binding? ast)
	    (zodiac:varref? ast))
	(zodiac:parsed->raw ast)]
       
       ; compound sexps
       [(zodiac:define-values-form? ast)
	`(define-values ,(map zodiac->sexp (zodiac:define-values-form-vars ast))
	   ,(zodiac->sexp/annotate (zodiac:define-values-form-val ast)))]
       
       [(zodiac:app? ast)
	`(,(zodiac->sexp/annotate (zodiac:app-fun ast))
	  ,@(map zodiac->sexp/annotate (zodiac:app-args ast)))]
       
       [(zodiac:set!-form? ast)
	`(set! ,(zodiac->sexp (zodiac:set!-form-var ast))
	       ,(zodiac->sexp/annotate (zodiac:set!-form-val ast)))]
       
       [(zodiac:case-lambda-form? ast)
	`(case-lambda
	  ,@(map
	     (lambda (args body)
	       `(,(let ([vars (zodiac:arglist-vars args)])
		    (cond
		     [(zodiac:sym-arglist? args) (zodiac->sexp (car vars))]
		     [(zodiac:list-arglist? args) (map zodiac->sexp vars)]
		     [(zodiac:ilist-arglist? args) (let loop ([args vars])
						     (if (null? (cdr args))
							 (zodiac->sexp (car args))
							 (cons (zodiac->sexp (car args))
							       (loop (cdr args)))))]))
		 ,(zodiac->sexp/annotate body)))
	     (zodiac:case-lambda-form-args ast)
	     (zodiac:case-lambda-form-bodies ast)))]

       [(zodiac:begin-form? ast)
	`(begin ,@(map zodiac->sexp/annotate (zodiac:begin-form-bodies ast)))]

       [(zodiac:begin0-form? ast)
	`(begin ,@(map zodiac->sexp/annotate (zodiac:begin0-form-bodies ast)))]

       [(zodiac:let-values-form? ast)
	`(let-values
	     ,(map list
		   (map (lambda (l) (map zodiac->sexp l)) (zodiac:let-values-form-vars ast))
		   (map zodiac->sexp/annotate (zodiac:let-values-form-vals ast)))
	   ,(zodiac->sexp/annotate (zodiac:let-values-form-body ast)))]
       
       [(zodiac:letrec*-values-form? ast)
	`(letrec*-values
	     ,(map list
		   (map (lambda (l) (map zodiac->sexp l)) (zodiac:letrec*-values-form-vars ast))
		   (map zodiac->sexp/annotate (zodiac:letrec*-values-form-vals ast)))
	   ,(zodiac->sexp/annotate (zodiac:letrec*-values-form-body ast)))]

       [(zodiac:if-form? ast)
	`(if ,(zodiac->sexp/annotate (zodiac:if-form-test ast))
	     ,(zodiac->sexp/annotate (zodiac:if-form-then ast))
	     ,(zodiac->sexp/annotate (zodiac:if-form-else ast)))]
       
       [(zodiac:struct-form? ast)
	(let ([type (zodiac:struct-form-type ast)]
	      [super (zodiac:struct-form-super ast)]
	      [fields (zodiac:struct-form-fields ast)])
	  (if super
	      `(struct (,type ,(zodiac->sexp/annotate super)) ,fields)
	      (zodiac:parsed->raw ast)))]

       [(zodiac:unit-form? ast)
	`(unit (import ,@(map zodiac->sexp (zodiac:unit-form-imports ast)))
	       (export ,@(map (lambda (a) (list (zodiac->sexp (car a))
						(zodiac:read-object (cdr a))))
			      (zodiac:unit-form-exports ast)))
	       ,@(map zodiac->sexp/annotate (zodiac:unit-form-clauses ast)))]

       [(zodiac:compound-unit-form? ast)
	`(compound-unit
	  (import ,@(map zodiac->sexp (zodiac:compound-unit-form-imports ast)))
	  (link ,(map  
		  (lambda (l)
		    (zodiac:read-object (car l))
		    `(,(zodiac->sexp (cadr l))
		      ,@(map (lambda (x)
			       (if (pair? x)
				   `(,(zodiac:read-object (car x)) ,(zodiac:read-object (cdr x)))
				   (zodiac->sexp x)))
			     (cddr l))))
		  (zodiac:compound-unit-form-links ast)))
	  (export ,@(map
		     (lambda (e)
		       `(,(zodiac:read-object (car e))
			 (,(zodiac:read-object (cadr e))
			  ,(zodiac:read-object (cddr e)))))
		     (zodiac:compound-unit-form-exports ast))))]

       [(zodiac:invoke-unit-form? ast)
	`(invoke-unit ,(zodiac->sexp/annotate (zodiac:invoke-unit-form-unit ast))
		      ,@(map zodiac->sexp (zodiac:invoke-unit-form-variables ast)))]

       [(zodiac:class*/names-form? ast)
	`(class*/names
	  (,(zodiac->sexp (zodiac:class*/names-form-this ast))
	   ,(zodiac->sexp (zodiac:class*/names-form-super-init ast)))
	  ,(zodiac->sexp/annotate (zodiac:class*/names-form-super-expr ast))
	  ,(map zodiac->sexp/annotate (zodiac:class*/names-form-interfaces ast))
	  ,(let* ([args (zodiac:class*/names-form-init-vars ast)]
		  [vars (zodiac:paroptarglist-vars args)]
		  [zodiac->sexp (lambda (var)
				  (if (pair? var)
				      `(,(zodiac->sexp (car var)) (zodiac->sexp/annotate (cdr var)))
				      (zodiac->sexp var)))])
	     (cond
	      [(zodiac:sym-paroptarglist? args) (zodiac->sexp (car vars))]
	      [(zodiac:list-paroptarglist? args) (map zodiac->sexp (car vars))]
	      [(zodiac:ilist-paroptarglist? args) (let loop ([args vars])
						    (if (null? (cdr args))
							(zodiac->sexp (car args))
							(cons (zodiac->sexp (car args))
							      (loop (cdr args)))))]))
	  ,@(map (lambda (clause)
		   (cond
		    ((zodiac:public-clause? clause)
		     `(public
			,@(map (lambda (internal export expr)
				 `((,(zodiac->sexp internal) ,(zodiac->sexp export))
				   ,(zodiac->sexp/annotate expr)))
			       (zodiac:public-clause-internals clause)
			       (zodiac:public-clause-exports clause)
			       (zodiac:public-clause-exprs clause))))
		    ((zodiac:override-clause? clause)
		     `(override
			,@(map (lambda (internal export expr)
				 `((,(zodiac->sexp internal) ,(zodiac->sexp export))
				   ,(zodiac->sexp/annotate expr)))
			       (zodiac:override-clause-internals clause)
			       (zodiac:override-clause-exports clause)
			       (zodiac:override-clause-exprs clause))))
		    ((zodiac:private-clause? clause)
		     `(private
			,@(map (lambda (internal expr)
				 `(,(zodiac->sexp internal) ,(zodiac->sexp/annotate expr)))
			       (zodiac:private-clause-internals clause)
			       (zodiac:private-clause-exprs clause))))
		    ((zodiac:inherit-clause? clause)
		     `(inherit
		       ,@(map (lambda (internal inherited)
				`(,(zodiac->sexp internal) ,(zodiac->sexp inherited)))
			      (zodiac:inherit-clause-internals clause)
			      (zodiac:inherit-clause-imports clause))))
		    ((zodiac:rename-clause? clause)
		     `(rename
		       ,@(map (lambda (internal inherited)
				`(,(zodiac->sexp internal) ,(zodiac->sexp inherited)))
			      (zodiac:rename-clause-internals clause)
			      (zodiac:rename-clause-imports clause))))
		    ((zodiac:sequence-clause? clause)
		     `(sequence
			,@(map zodiac->sexp/annotate (zodiac:sequence-clause-exprs clause))))))
		 (zodiac:class*/names-form-inst-clauses ast)))]

       [(zodiac::-form? ast)
	(zodiac->sexp (zodiac::-form-exp ast))]
       
       [else
	(error 'zodiac->sexp/annotate "unsupported ~s" ast)])))
  
  )







