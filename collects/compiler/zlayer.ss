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

  ;; Create a new back-box for a new zodiac AST node
  (define (make-empty-box) (zodiac:make-empty-back-box))

  ;; Manipulating annotations:
  ;; NOTE: Zodiac must be invoked before this unit
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
	   (string-append 
	    s
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
  ;; SPECIAL CONSTANTS
  ;;
  ;; some constants we don't know how to write, like #<void>
  ;;

  (define undefined (letrec ([x x]) x))

  (define (undefined? x) (eq? x udnefined))

  (define zodiac:make-special-constant
    ;; make-quote, make-constant
    (lambda (text)
      (let ([z (quote-syntax here)])
	(zodiac:make-quote-form 
	 z (make-empty-box)
	 (case text
	   [(void) (datum->syntax (void) #f #f)]
	   [(null) (datum->syntax null #f #f)]
	   [(undefined) (datum->syntax undefined #f #f)]
	   [else (compiler:internal-error 'make-special-constant "bad type")])))))
  
  ;;-----------------------------------------------------------------------------
  ;; BINDING->LEXICAL-VARREF
  ;;
  ;; creates a zodiac:lexical-varref from a zodiac:binding
  ;;
  
  (define zodiac:binding->lexical-varref
    (lambda (ast)
      (let ([v (zodiac:make-lexical-varref (zodiac:zodiac-stx ast)
					   (make-empty-box)
					   (zodiac:binding-var ast))])
	(set-annotation! v (varref:empty-attributes))
	v)))

  ;;----------------------------------------------------------------------------
  ;; POSITION REPORTING
  
  (define main-source-file (make-parameter #f))
  
  (define zodiac:print-start!
    (lambda (port ast)
      (let ([bad (lambda () (fprintf port " [?,?]: "))])
	(if (and ast (zodiac:zodiac? ast))
	    (let* ([start (zodiac:zodiac-start ast)]
		   [good (lambda ()
			   (fprintf port " ~a[~a,~a]: "
				    (if (equal? (main-source-file) (zodiac:location-file start))
					""
					(format "~s " (zodiac:location-file start)))
				    (zodiac:location-line start)
				    (zodiac:location-column start)))])
	      (good))
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
	`(begin0 ,@(map zodiac->sexp/annotate (zodiac:begin0-form-bodies ast)))]

       [(zodiac:let-values-form? ast)
	`(let-values
	     ,(map list
		   (map (lambda (l) (map zodiac->sexp l)) (zodiac:let-values-form-vars ast))
		   (map zodiac->sexp/annotate (zodiac:let-values-form-vals ast)))
	   ,(zodiac->sexp/annotate (zodiac:let-values-form-body ast)))]
       
       [(zodiac:letrec-values-form? ast)
	`(letrec-values
	     ,(map list
		   (map (lambda (l) (map zodiac->sexp l)) (zodiac:letrec-values-form-vars ast))
		   (map zodiac->sexp/annotate (zodiac:letrec-values-form-vals ast)))
	   ,(zodiac->sexp/annotate (zodiac:letrec-values-form-body ast)))]

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

       [(zodiac:with-continuation-mark-form? ast)
	`(with-continuation-mark 
	     ,(zodiac->sexp/annotate (zodiac:with-continuation-mark-form-key ast))
	     ,(zodiac->sexp/annotate (zodiac:with-continuation-mark-form-val ast))
	   ,(zodiac->sexp/annotate (zodiac:with-continuation-mark-form-body ast)))]

       [else
	(error 'zodiac->sexp/annotate "unsupported ~s" ast)])))
  
  )







