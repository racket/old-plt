;; Zodiac interface and library routines
;; (c)1996-7 Sebastian Good

(unit/sig
 compiler:zlayer^
 (import (compiler:option : compiler:option^)
	 (zodiac : zodiac:system^)
	 compiler:cstructs^
	 compiler:driver^
	 mzlib:function^)

;;----------------------------------------------------------------------------
;; ANNOTATIONS
;;

(define compiler:empty-annotation (gensym))
(define (make-empty-box) (zodiac:make-empty-back-box))
(define get-annotation #f)
(define set-annotation! #f)
(define (annotated? ast)
  (not (eq? (get-annotation ast)
	    compiler:empty-annotation)))
(define (remove-annotation! ast)
  (set-annotation! ast compiler:empty-annotation))

;; register-with-zodiac sets the getter and setter procedures
;; IMPLICATION: Zodiac must be invoked first
(let-values ([(getter setter)
	      (zodiac:register-client 'compiler 
				      (lambda ()
					compiler:empty-annotation))])
    (set! get-annotation
	  (lambda (ast)
	    (getter (zodiac:parsed-back ast))))
    (set! set-annotation!
	  (lambda (ast obj)
	    (setter (zodiac:parsed-back ast) obj))))

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
;; BEGIN-FORM
;;
;;  maintain the illusion of a two slot begin-form; ensured by prephase
;;  also for begin0
;;

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
	 [(undefined) (zodiac:make-undefined z z z text)]
	 [else (compiler:internal-error 'make-special-constant "bad type")])))))
 
;;-----------------------------------------------------------------------------
;; BINDING->LEXICAL-VARREF
;;
;; creates a lexical-varref from a binding occurrence
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
)

#|

;;----------------------------------------------------------------------------
;; SOME RANDOM DEBUGGING STUFF USED IN THE PAST
;;
;;
(define zodiac->sexp:annotate #f)
(define zodiac->sexp
  (lambda (ast)
    (cond 

      ; literal values
      [(zodiac:quote-form? ast) 
       (list 'quote
	     (let xlate ([ast (zodiac:quote-form-expr ast)])
	       (cond
		 [(zodiac:scalar? ast) (zodiac:read-object ast)]
		 [(zodiac:list? ast) (map xlate (zodiac:read-object ast))]
		 [(zodiac:improper-list? ast)
		  (improper-map xlate (zodiac:read-object ast))]
		 [(zodiac:vector? ast)
		  (list->vector (map xlate (zodiac:read-object ast)))]
		 [else 
		  (error 'quote-form "~a not list,vector,or scalar" ast)])))]
    
     ; variable references
     [(zodiac:top-level-varref? ast)
      (if zodiac->sexp:annotate
	  (list '%global (zodiac:id-var ast))
	  (zodiac:id-var ast))]
     
     [(compiler:env-varref? ast)
      (if zodiac->sexp:annotate
	  (list '%env (zodiac:id-var ast))
	  (zodiac:id-var ast))]
     
     [(zodiac:lexical-varref? ast)
      (if zodiac->sexp:annotate
	  (list '%lexical (zodiac:id-var ast) )
	  (zodiac:id-var ast))]
     
     [(compiler:static-varref? ast)
      (list '%static (zodiac:id-var ast))]

     [(compiler:bound? ast)
      (if zodiac->sexp:annotate
	  (let ([binding (compiler:bound-binding ast)])
	    `( %compiler-bound
	       ,(zodiac:bound-var ast)
	       (rec? ,(binding-rec? binding))
	       (mutable? ,(binding-mutable? binding))
	       ,@(if (binding-known? binding) '(known) '())
	       ;,@(if (binding-known? binding)
	       ;	     (list '= (zodiac->sexp (binding-val binding)))
	       ;	     '(%unknown))
	       (rep ,(binding-rep binding))))
	  (zodiac:bound-var ast))]
	         
     [(zodiac:bound? ast)
      (list '%bound
	    (zodiac:bound-var ast))]

     ; compound sexps
     [(zodiac:define-form? ast)
      `(define ,(zodiac->sexp (zodiac:define-form-var ast))
	       ,(zodiac->sexp (zodiac:define-form-val ast)))]
     
     [(compiler:app? ast)
      (if zodiac->sexp:annotate
	  `(,(if (app-tail? (compiler:app-app ast)) '%tail-apply '%apply)
	    ,(zodiac->sexp (zodiac:app-fun ast))
	    ,@(map zodiac->sexp (zodiac:app-args ast)))
	  `(,(zodiac->sexp (zodiac:app-fun ast))
	    ,@(map zodiac->sexp (zodiac:app-args ast))))]
     
     [(zodiac:app? ast)
      `(,(zodiac->sexp (zodiac:app-fun ast))
	,@(map zodiac->sexp (zodiac:app-args ast)))]
     
     [(zodiac:set!-form? ast)
      `(set! ,(zodiac->sexp (zodiac:set!-form-var ast))
	     ,(zodiac->sexp (zodiac:set!-form-val ast)))]

     [(compiler:lambda-form? ast)
      (if zodiac->sexp:annotate
	  (let ([code (compiler:lambda-form-code ast)])
	    `(lambda
		 (args ,@(improper-map 
			  zodiac->sexp 
			  (zodiac:lambda-form-args ast)))
	       (free-vars ,@(map zodiac->sexp
				 (set->list (code-free-vars code))))
	       (local-vars ,@(map zodiac->sexp
				  (set->list (code-local-vars code))))
	       (captured-vars ,@(map zodiac->sexp
				     (set->list (code-captured-vars code))))
	       ,(zodiac->sexp (zodiac:lambda-form-body ast))))
	  `(lambda
	       ,(improper-map zodiac->sexp (zodiac:lambda-form-args ast))
	     ,(zodiac->sexp (zodiac:lambda-form-body ast))))]
     
     [(zodiac:lambda-form? ast)
      `(lambda
	   ,(improper-map zodiac->sexp (zodiac:lambda-form-args ast))
	 ,(zodiac->sexp (zodiac:lambda-form-body ast)))]

     [(compiler:make-closure? ast)
      (if zodiac->sexp:annotate
	  `(make-closure ,(zodiac->sexp (compiler:make-closure-lambda ast)))
	  (zodiac->sexp (compiler:make-closure-lambda ast)))]
     
     [(zodiac:begin-form? ast)
      `(begin ,(zodiac->sexp (zodiac:begin-form-first ast))
	      ,(zodiac->sexp (zodiac:begin-form-rest ast)))]

     [(zodiac:let-form? ast)
      `(let 
	 ,(map list
		  (map zodiac->sexp (zodiac:let-form-vars ast))
		  (map zodiac->sexp (zodiac:let-form-vals ast)))
	 ,(zodiac->sexp (zodiac:let-form-body ast)))]
     
     [(zodiac:letrec-form? ast)
      `(letrec ,(map list
		  (map zodiac->sexp (zodiac:letrec-form-vars ast))
		  (map zodiac->sexp (zodiac:letrec-form-vals ast)))
	 ,(zodiac->sexp (zodiac:letrec-form-body ast)))]

     [(zodiac:if-form? ast)
      `(if ,(zodiac->sexp (zodiac:if-form-test ast))
	   ,(zodiac->sexp (zodiac:if-form-then ast))
	   ,(zodiac->sexp (zodiac:if-form-else ast)))]
 
     [(zodiac:delay-form? ast)
      `(delay ,(zodiac->sexp (zodiac:delay-form-expr ast)))]

     [(zodiac:define-struct-form? ast)
      (let ([type (zodiac:define-struct-form-type ast)]
	    [super (zodiac:define-struct-form-super ast)]
	    [fields (zodiac:define-struct-form-fields ast)])
      `(define-struct ,(if super
			   (list (zodiac:read-object type) (zodiac->sexp super))
			   (map zodiac:read-object type))
	 ,(map zodiac:read-object fields)))]
     
     [else
      (error 'zodiac->sexp "unsupported ~s" ast)])))
|#

