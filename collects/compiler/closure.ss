;; closure.ss
;; analyzes & transforms code

(unit/sig
 compiler:closure^
 (import (compiler:option : compiler:option^)
	 compiler:library^
	 compiler:cstructs^
	 (zodiac : zodiac:system^)
	 compiler:zlayer^
	 compiler:const^
	 compiler:driver^)


(define compiler:lambda-list null)
(define compiler:add-lambda!
  (lambda (l)
    (set! compiler:lambda-list
	  (cons l compiler:lambda-list))))

;; adds a one-time closure-creation to be performed at startup
(define compiler:once-closures-list null)
(define compiler:once-closures-globals-list null)
(define compiler:add-static-closure!
  (lambda (code globals)
    (let* ([var (gensym 'const)]
	   [sv (zodiac:make-top-level-varref/bind
		(zodiac:zodiac-origin code)
		(zodiac:zodiac-start code)
		(zodiac:zodiac-finish code)
		(make-empty-box)
		var
		(box '()))]
	   [def (zodiac:make-define-values-form 
		 (zodiac:zodiac-origin code)
		 (zodiac:zodiac-start code)
		 (zodiac:zodiac-finish code)
		 (make-empty-box)
		 (list sv) code)])
      (set-annotation! sv (varref:empty-attributes))
      (varref:add-attribute! sv varref:static)
      (varref:add-attribute! sv varref:per-load-static)
      (compiler:add-per-load-static-list! var)
      (set! compiler:once-closures-list (cons def compiler:once-closures-list))
      (set! compiler:once-closures-globals-list (cons globals compiler:once-closures-globals-list))
      sv)))


(define (compiler:init-once-closure-lists!)
  (set! compiler:once-closures-list null)
  (set! compiler:once-closures-globals-list null))

(define (compiler:init-lambda-lists!)
  (set! compiler:lambda-list null)
  (compiler:init-once-closure-lists!))

(define closure-expression!
  (letrec
      ([transform-closure!
	(lambda (ast args)
	  (compiler:add-lambda! ast)
	  (let* ([code (get-annotation ast)]
		 [name (code-name code)]
		 [free (code-free-vars code)]
		 [mk-closure (make-compiler:make-closure 
			      (zodiac:zodiac-origin ast)
			      (zodiac:zodiac-start ast)
			      (zodiac:zodiac-finish ast)
			      ast free args
			      name)])
	    (if (and (set-empty? free) (null? args))
		;; Closes over nothing and uses no arguments, so make closure once at startup time
		(compiler:add-static-closure! mk-closure (code-global-vars code))
		mk-closure)))]
       [transform!
	(lambda (ast)
	  (cond
	   
	   ;;------------------------------------------------------------------
	   ;; CONSTANTS
	   ;;
	   [(zodiac:quote-form? ast) ast]
	   
	   ;;------------------------------------------------------------------
	   ;; VARIABLE REFERENCES
	   ;;
	   [(zodiac:varref? ast) ast]

	   ;;------------------------------------------------------------------
	   ;; LAMBDA EXPRESSIONS
	   ;;
	   ;; We turn this into a make-closure form and catalogue the code body
	   ;; we also decide which vehicle in which to put the body
	   ;;
	   [(zodiac:case-lambda-form? ast)
	    (zodiac:set-case-lambda-form-bodies! ast 
						 (map (lambda (body)
							(transform! body))
						      (zodiac:case-lambda-form-bodies ast)))
	    (transform-closure! ast null)]

	   ;;------------------------------------------------------------------
	   ;; LET EXPRESSIONS
	   ;;
	   [(zodiac:let-values-form? ast)
	    (zodiac:set-let-values-form-vals! 
	     ast (map transform! (zodiac:let-values-form-vals ast)))
	    (zodiac:set-let-values-form-body! 
	     ast (transform! (zodiac:let-values-form-body ast)))
	    ast]

	   [(zodiac:letrec*-values-form? ast)
	    (zodiac:set-letrec*-values-form-vals! 
	     ast 
	     (map transform! (zodiac:letrec*-values-form-vals ast)))
	    (zodiac:set-letrec*-values-form-body! 
	     ast (transform! (zodiac:letrec*-values-form-body ast)))
	    ast]
	   
	   ;;-----------------------------------------------------------------
	   ;; IF EXPRESSIONS
	   ;;
	   [(zodiac:if-form? ast)
	    (zodiac:set-if-form-test! ast (transform! (zodiac:if-form-test ast)))
	    (zodiac:set-if-form-then! ast (transform! (zodiac:if-form-then ast)))
	    (zodiac:set-if-form-else! ast (transform! (zodiac:if-form-else ast)))
	    ast]

	   ;;------------------------------------------------------------------
	   ;; BEGIN EXPRESSIONS
	   ;;
	   [(zodiac:begin-form? ast)
	    (map! transform! (zodiac:begin-form-bodies ast))
	    ast]
  
	   ;;------------------------------------------------------------------
	   ;; BEGIN0 EXPRESSIONS
	   ;;
	   [(zodiac:begin0-form? ast)
	    (zodiac:set-begin0-form-first!
	     ast (transform! (zodiac:begin0-form-first ast)))
	    (zodiac:set-begin0-form-rest!
	     ast (transform! (zodiac:begin0-form-rest ast)))
	    ast]
  
	   ;;------------------------------------------------------------------
	   ;; DEFINE/SET! EXPRESSIONS
	   ;;
	   [(zodiac:set!-form? ast)
	    (zodiac:set-set!-form-val! ast (transform! (zodiac:set!-form-val ast)))
	    ast]
	   
	   [(zodiac:define-values-form? ast)
	    (zodiac:set-define-values-form-val! 
	     ast 
	     (transform! (zodiac:define-values-form-val ast)))
	    ast]

	   
	   ;;-----------------------------------------------------------------
	   ;; APPLICATIONS
	   ;;
	   ;; Now we should be applying closures to arguments.  The actual
	   ;; extraction of code and environment parts will happen in the 
	   ;; vm translation
	   ;;
	   [(zodiac:app? ast)
	    (zodiac:set-app-fun! ast (transform! (zodiac:app-fun ast)))
	    (zodiac:set-app-args! ast (map transform! (zodiac:app-args ast)))
	    ast]
	   
	   ;;-----------------------------------------------------------------
	   ;; STRUCT
	   ;;
	   [(zodiac:struct-form? ast)
	    (let ([super (zodiac:struct-form-super ast)])
	      (when super
		(zodiac:set-struct-form-super! ast (transform! super)))
	      ast)]

	   ;;------------------------------------------------------------------
	   ;; UNIT EXPRESSIONS
	   ;;
	   ;; We turn this into a make-closure form and catalogue the code body
	   ;;
	   [(zodiac:unit-form? ast)
	    (zodiac:set-unit-form-clauses! ast 
					   (map (lambda (body)
						  (transform! body))
						(zodiac:unit-form-clauses ast)))
	    (transform-closure! ast null)]

	   ;;-------------------------------------------------------------------
	   ;; COMPOUND UNIT
	   ;;
	   ;;
	   [(zodiac:compound-unit-form? ast)
	    (let loop ([l (zodiac:compound-unit-form-links ast)])
	      (unless (null? l)
		 (let ([link (car l)])
		   (set-car! (cdr link) (transform! (cadr link)))
		   (loop (cdr l)))))
	    ast]

	   ;;-----------------------------------------------------------
	   ;; INVOKE
	   ;;
	   [(zodiac:invoke-form? ast)
	    (zodiac:set-invoke-form-unit!
	     ast 
	     (transform! (zodiac:invoke-form-unit ast)))
	    
	    ast]
	   
	   ;;-----------------------------------------------------------------
	   ;; CLASS
	   ;;
	   [(zodiac:class*/names-form? ast)

	    (zodiac:set-class*/names-form-super-expr! 
	     ast 
	     (transform! (zodiac:class*/names-form-super-expr ast)))
	    (zodiac:set-class*/names-form-interfaces! 
	     ast
	     (map (lambda (i) (transform! i))
		  (zodiac:class*/names-form-interfaces ast)))

	    (let ([l (zodiac:sequence-clause-exprs
		      (car (zodiac:class*/names-form-inst-clauses ast)))])
	      (set-car! l (transform! (car l))))
	    
	    (transform-closure! ast 
				(cons (zodiac:class*/names-form-super-expr ast)
				      (zodiac:class*/names-form-interfaces ast)))]

	   ;;-------------------------------------------------------------------
	   ;; INTERFACE
	   ;;
	   ;;
	   [(zodiac:interface-form? ast)
	    (zodiac:set-interface-form-super-exprs!
	     ast
	     (map (lambda (expr) (transform! expr))
		  (zodiac:interface-form-super-exprs ast)))

	    ast]
	   
	   [else (compiler:internal-error 
		  ast 
		  (format
		   "closure-expression: form not supported: ~a" ast))]))])
    (lambda (ast) (transform! ast))))
)
	    
#|

(define (closure-go f . o)
  (set! driver:debug 'closure)
  (apply s:compile (cons f (cons 'c-only o)))
  driver:debug)

|#
