;; 1st analysis phase of the compiler [lexical analysis]
;; (c) 1996-7 Sebastian Good

(unit/sig
 compiler:analyze^
 (import (compiler:option : compiler:option^)
	 compiler:library^
	 compiler:cstructs^
	 (zodiac : zodiac:system^)
	 compiler:zlayer^
	 compiler:prephase^
	 compiler:anorm^
	 compiler:const^
	 compiler:rep^
	 compiler:driver^
	 mzlib:function^)

(define compiler:global-symbols (make-hash-table))
(define compiler:add-global-varref!
  (lambda (varref)
    (let ([v (zodiac:varref-var varref)])
      (hash-table-put! compiler:global-symbols v v))))

(define compiler:primitive-refs empty-set)
(define compiler:add-primitive-varref!
  (lambda (varref)
    (set! compiler:primitive-refs
	  (set-union-singleton compiler:primitive-refs
			       (zodiac:varref-var varref)))))

(define compiler:compounds null) ; built backwards, as usual
(define (compiler:add-compound-unit! c)
  (begin0
   (length compiler:compounds)
   (set! compiler:compounds (cons c compiler:compounds))))

(define compiler:interfaces null) ; built backwards, as usual
(define (compiler:add-interface! c)
  (begin0
   (length compiler:interfaces)
   (set! compiler:interfaces (cons c compiler:interfaces))))

(define compiler:max-arity-allowed 11739) ; this is a sane limit

(define compiler:define-list null)
(define compiler:per-load-define-list null)
(define compiler:local-define-list null)
(define compiler:local-per-load-define-list null)

(define (compiler:init-define-lists!)
  (set! compiler:define-list null)
  (set! compiler:per-load-define-list null)
  (set! compiler:global-symbols (make-hash-table))
  (set! compiler:primitive-refs empty-set)
  (set! compiler:compounds null)
  (set! compiler:interfaces null))

(define (compiler:add-local-per-load-define-list! def)
  (set! compiler:local-per-load-define-list 
	(cons def compiler:local-per-load-define-list)))
(define (compiler:add-local-define-list! def)
  (set! compiler:local-define-list 
	(cons def compiler:local-define-list)))

(define-struct case-info (body case-code global-vars captured-vars max-arity))

(define (list->zodiac:quote l ast)
  (zodiac:make-quote-form
   (zodiac:zodiac-origin ast)
   (zodiac:zodiac-start ast)
   (zodiac:zodiac-finish ast)
   (make-empty-box)
   (zodiac:structurize-syntax l ast)))

;;---------------------------------------------------------------------------
;; can-propagate-constant?
;;
;; Tells us which constants we can replace directly with their text
;;
(define (can-propagate-constant? ast)
  (or (zodiac:quote-form? ast)
      ;(zodiac:lexical-varref? ast) to do this, we must put renamed vars in
                                    ; environments as their old name...
      (and (zodiac:top-level-varref? ast)
	   (or (varref:has-attribute? ast varref:primitive)
	       (varref:has-attribute? ast varref:static)))))

;; helper functions for analyze-expression!
(define make-known-binding
  (lambda (bound val)
    (make-binding #f (prephase:is-mutable? bound) 
		  (prephase:is-unit-i/e? bound) (prephase:binding-anchor bound)
		  (prephase:is-ivar? bound)
		  #t val #f #f)))
(define make-unknown-binding
  (lambda (bound)
    (make-binding #f (prephase:is-mutable? bound) 
		  (prephase:is-unit-i/e? bound) (prephase:binding-anchor bound)
		  (prephase:is-ivar? bound)
		  #f #f #f #f)))
(define make-begin0-binding
  (lambda (bound)
    (make-binding #f #f #f #f #f
		  #f #f #f (make-rep:atomic 'begin0-saver))))

;; turns a 'bad' letrec into let+set!, also returning a procedure
;; to set! the body to the correct form, to avoid re-analyzing it
;; the 'body' is set to #%void 
(define letrec->let+set!
  (lambda (ast)
    (let* ([set-body! #f]
	   [body
	    (let linearize-set! ([vars (zodiac:letrec*-values-form-vars ast)]
				 [vals (zodiac:letrec*-values-form-vals ast)])
	      (if (null? vars)
		  (zodiac:make-special-constant 'void)
		  (let* ([var (car vars)] [val (car vals)]
			 [b (zodiac:make-begin-form
			     (zodiac:zodiac-origin ast)
			     (zodiac:zodiac-start ast)
			     (zodiac:zodiac-finish ast)
			     (make-empty-box)
			     (list
			      ; must turn set!-values into set! form
			      (prephase:multiple-set!->single-set!
			       (map zodiac:binding->lexical-varref var)
			       val
			       zodiac:make-set!-form 
			       #f
			       ast)
			      (linearize-set! (cdr vars) (cdr vals))))])
		    (when (null? (cdr vars))
		      (set! set-body! 
			    (lambda (v)
			      (set-car! (cdr (zodiac:begin-form-bodies b)) v))))
;			      (zodiac:set-begin-form-rest! b v))))
		    b)))]
	   [let-form
	    (zodiac:make-let-values-form 
	     (zodiac:zodiac-origin ast)
	     (zodiac:zodiac-start ast)
	     (zodiac:zodiac-finish ast)
	     (make-empty-box) ; (zodiac:parsed-back ast)
	     (zodiac:letrec*-values-form-vars ast)
	     (map (lambda (v) 
		    (if (= 1 (length v))
			(zodiac:make-special-constant 'undefined)
			(compiler:make-const-constructor 
			 ast
			 '#%values
			 (map (lambda (_) (zodiac:make-special-constant 
					   'undefined)) 
			      v))))    
		  (zodiac:letrec*-values-form-vars ast))
	     body)])
      (values let-form set-body!))))

(define (analyze:valueable? v)
  (cond
   [(zodiac:quote-form? v) #t]
   [(zodiac:bound-varref? v)
    ; only if the varref is not unit-i/e? or is already known
    (let ([b (get-annotation (zodiac:bound-varref-binding v))])
      (or (and (binding-properties? b)
	       (not (binding-properties-unit-i/e? b)))
	  (and (binding? b)
	       (or (not (binding-unit-i/e? b))
		   (binding-known? b)))))]
   [(zodiac:varref? v) #t]
   [(zodiac:case-lambda-form? v) #t]
   [(zodiac:unit-form? v) #t]
   [(zodiac:begin-form? v)
    (andmap analyze:valueable? (zodiac:begin-form-bodies v))]
   [(zodiac:begin0-form? v)
    (andmap analyze:valueable? (zodiac:begin0-form-bodies v))]
   [(zodiac:set!-form? v) #f] ; because it changes a variable
   [(zodiac:struct-form? v)
    (analyze:valueable? (zodiac:struct-form-super v))]
   [(zodiac:if-form? v)
    (and (analyze:valueable? (zodiac:if-form-test v))
	 (analyze:valueable? (zodiac:if-form-then v))
	 (analyze:valueable? (zodiac:if-form-else v)))]
   [(zodiac:let-values-form? v)
    (and (andmap analyze:valueable? (zodiac:let-values-form-vals v))
	 (analyze:valueable? (zodiac:let-values-form-body v)))]
   [(zodiac:letrec*-values-form? v)
    (and (andmap analyze:valueable? (zodiac:letrec*-values-form-vals v))
	 (analyze:valueable? (zodiac:letrec*-values-form-body v)))]
   [else #f]))

(define (extract-value v)
  (cond
   [(zodiac:set!-form? v) (zodiac:make-special-constant 'void)]
   [(zodiac:begin-form? v) (extract-value (car (last-pair (zodiac:begin-form-bodies v))))]
   [(zodiac:begin0-form? v) (extract-value (car (zodiac:begin0-form-bodies v)))]
   [(zodiac:let-values-form? v) (extract-value (zodiac:let-values-form-body v))]
   [(zodiac:letrec*-values-form? v) (extract-value (zodiac:letrec*-values-form-body v))]
   [else v]))

(define (extract-varref-known-val v)
  (let loop ([v v])
    (let ([binding (if (zodiac:binding? v)
		       (get-annotation v)
		       (get-annotation (zodiac:bound-varref-binding v)))])
      (and (binding-known? binding)
	   (let ([v (binding-val binding)])
	     (if (zodiac:bound-varref? v)
		 (loop v)
		 v))))))

;; analyze-expression takes 4 arguments
;;  1) an AST to transform
;;  2) a set of bound variables (the lexical environment)
;;  3) tail?
;;
;; it returns 6 values
;;  1) a destructively altered AST
;;  2) a set of variables occurring free in that expression
;;  3) a set of variables which are bound by lets in that expression
;;  4) global variables and mutable `constants' used in the expression
;;  5) those variables which are used or captured by nested lambdas (args and locals)
;;  6) maximum arity to a call made in this expression
;;
(define analyze-expression!
  (lambda (ast bound-vars tail?)
    (let ([local-vars bound-vars]
	  [locals-captured empty-set]
	  [free-vars empty-set]
	  [global-vars empty-set]
	  [max-arity 1])
      (letrec
	  ([add-local-var! (lambda (var)
			     (set! local-vars
				   (set-union-singleton local-vars var)))]
	   [remove-local-var! (lambda (var)
				(set! local-vars
				      (set-minus local-vars (make-singleton-set var))))]
	   [add-free-var! (lambda (var)
			    (set! free-vars
				  (set-union-singleton free-vars var)))]
	   [add-captured-var! (lambda (var)
				(set! locals-captured
				      (set-union-singleton locals-captured var)))]
	   [add-global-var! (lambda (var)
			      (set! global-vars
				    (set-union-singleton global-vars var)))]
	   [register-arity! (lambda (n) (set! max-arity (max n max-arity)))]  
	   [register-code-vars!
	    (lambda (code)
	      ; all variables which are free in nested closures are
	      ; free in this closure (since we need them in the
	      ; environment) except those that are already in the
	      ; environment: bound-vars (typically the arguments)
	      ; and local-vars.
	      (set! free-vars
		    (set-union (set-minus (code-free-vars code)
					  local-vars)
			       free-vars))
	      (set! locals-captured
		    (set-union (set-intersect local-vars
					      (code-free-vars code))
			       locals-captured))
	      
	      ; If there are no free vars, this closure will be lifted
	      ; out of the current expression, so don't add global
	      ; vars in that case, but do add const:the-per-load-statics-table
	      (if (set-empty? (code-free-vars code))
		  (add-global-var! const:the-per-load-statics-table)
		  (set! global-vars
			(set-union (code-global-vars code) global-vars))))]
	   [analyze-code-body!
	    (lambda (ast locals tail? code)
	      (let-values ([(body free-vars
				  local-vars 
				  global-vars 
				  captured-vars
				  L-max-arity)
			    (analyze-expression! ast
						 locals
						 tail?)])

		   (set-code-free-vars! code (set-union free-vars
							(code-free-vars code)))
		   (set-code-local-vars! code (set-union local-vars
							 (code-local-vars code)))
		   (set-code-global-vars! code (set-union global-vars
							  (code-global-vars code)))
		   (set-code-captured-vars! code (set-union captured-vars
							    (code-captured-vars code)))
		   (set-code-max-arity! code (max L-max-arity
						  (code-max-arity code)))
		   
		   body))]
	   [analyze-varref!
	    (lambda (ast tail? need-varref?)
	      (cond

	       ;;-----------------------------------------------------------------
	       ;; VARIABLE REFERENCES (A-VALUES)
	       ;;
	       ;;    We need to catalogue which variables are used in this 
	       ;;    expression.  if a lexical varref is not in the 
	       ;;    environment, it is free
	       ;;
	       ;;    if the bound structure indicates this variable is known at
	       ;;    compile time, replace the varref with the const-ref
	       ;;    since we can propagate varrefs, we need to make sure we
	       ;;    capture the right name in closures.
	       ;;
	       [(zodiac:bound-varref? ast)
		
		; check to see if it's known.  If so, just return it.
		
		(let* ([binding (compiler:bound-varref->binding ast)]
		       [known? (binding-known? binding)]
		       [val (binding-val binding)])
		  (if (and (compiler:option:propagate-constants)
			   known? 
			   (can-propagate-constant? val)
			   (not need-varref?))

		      ; Propogate a constant!
		      ; This could be a quote-form that was installed
		      ; as a known unit value before it was
		      ; analyzed. If so, extract the constructed
		      ; constant from the backbox.
		      ; In any case, check for adding PLS to the closure.
		      (let ([c (if (zodiac:quote-form? val)
				   
				   (let ([a (get-annotation val)])
				     (if (zodiac:varref? a)
					 a
					 val))
				   
				   val)])
			(when (and (zodiac:varref? c)
				   (varref:has-attribute? c varref:per-load-static))
			   (add-global-var! const:the-per-load-statics-table))
			c)
		      
		      ; otherwise we don't know the value -- therefore just
		      ; do the normal free-variable analysis
		      (if (not (set-memq? (zodiac:bound-varref-binding ast)
					  local-vars))
			  
			  (begin 
			    (add-free-var! (zodiac:bound-varref-binding ast))
			    (varref:add-attribute! ast varref:env)
			    
			    ; If this variable has an anchor, include it in the list of free vars
			    (let ([a (binding-anchor (get-annotation (zodiac:bound-varref-binding ast)))])
			      (when a
				    (add-free-var! a)))
			    
			    ast)
			  
			  (begin
			    (add-captured-var! (zodiac:bound-varref-binding ast))
			    
			    ; If this variable has an anchor, include it in the list of captured vars
			    (let ([a (binding-anchor (get-annotation (zodiac:bound-varref-binding ast)))])
			      (when a
				    (add-captured-var! a)))

			    ast))))]

	       [(zodiac:top-level-varref? ast)
		(if (varref:has-attribute? ast varref:primitive)
		    (compiler:add-primitive-varref! ast)
		    (add-global-var! (zodiac:varref-var ast)))
		(compiler:add-global-varref! ast)
		ast]
	       
	       [else (compiler:internal-error 
		      ast
		      "analyze: expected a variable; got ~a" ast)]))]
	       
	   ;;-----------------------------------------------------------------
	   ;; CONSTANTS (A-VALUES)
	   ;;   literal constants -- send them off to the constant
	   ;;   constructors!!  This produces code in b-normal form
	   ;;   and adds defines to the local-define-list. This list
	   ;;   must be reversed since the dependencies are backwards
	   ;;   ahh, the excitement of multiple values...
	   [analyze-quote!
	    (lambda (ast known-immutable?)
	      (set! compiler:local-define-list null)
	      (set! compiler:local-per-load-define-list null)
	      (let ([ret (compiler:construct-const-code! 
			  (zodiac:quote-form-expr ast)
			  known-immutable?)])
		; Put a pointer to the constructed constant in the quote-form's backbox
		(set-annotation! ast ret)

		(set! compiler:define-list
		      (append! compiler:define-list 
			       (reverse! compiler:local-define-list)))
		(set! compiler:per-load-define-list
		      (append! compiler:per-load-define-list 
			       (reverse! compiler:local-per-load-define-list)))
		
		;; If this `constant' is mutable, register the per-load
		;; statics pointer as a `global'
		(when (and (zodiac:top-level-varref/bind? ret)
			   (varref:has-attribute? ret varref:per-load-static))
		      (add-global-var! const:the-per-load-statics-table))
		
		ret))]

	   [analyze!
	    (lambda (ast tail?)
	      (when (compiler:option:debug)
		(zodiac:print-start! debug:port ast)
		(newline debug:port))
	      (cond
		
		;;-----------------------------------------------------------------
		;; CONSTANTS (A-VALUES)

		[(zodiac:quote-form? ast)
		 (analyze-quote! ast #f)]
			
		;;-----------------------------------------------------------------
		;; VARIABLE REFERENCES (A-VALUES)
		;;
		;;    We need to catalogue which variables are used in this 
		;;    expression.  if a lexical varref is not in the 
		;;    environment, it is free
		;;
		;;    if the bound structure indicates this variable is known at
		;;    compile time, replace the varref with the const-ref
		;;    since we can propagate varrefs, we need to make sure we
		;;    capture the right name in closures.
		;;
		[(zodiac:bound-varref? ast) 
		 (analyze-varref! ast tail? #f)]

		[(zodiac:top-level-varref? ast)
		 (analyze-varref! ast tail? #f)]
						
		;;--------------------------------------------------------------------
		;; LAMBDA EXPRESSIONS
		;;    with lambda, we need to make a recursive call.  We
		;;    extend the lexical environment with everything that's
		;;    been declared locally so far.  From the analyze-expression
		;;    we have information about what free variables they use	
		;;
		[(zodiac:case-lambda-form? ast)
		 (let ([case-infos
			(map
			 (lambda (args body)
			   (let ([args (zodiac:arglist-vars args)])
			     
			     ;; annotate each binding with our information
			     (for-each
			      (lambda (bound) (set-annotation! bound (make-unknown-binding bound)))
			      args)
			     
			     (let-values 
			      ([(lambda-body free-lambda-vars
					     local-lambda-vars 
					     global-lambda-vars 
					     captured-lambda-vars
					     L-max-arity)
				
				(analyze-expression! body
						     (improper-list->set args)
						     #t)])
			      
			      (let ([case-code (make-case-code free-lambda-vars
							       local-lambda-vars
							       global-lambda-vars)])
				(make-case-info lambda-body case-code 
						global-lambda-vars captured-lambda-vars 
						L-max-arity)))))
			 (zodiac:case-lambda-form-args ast)
			 (zodiac:case-lambda-form-bodies ast))])
		   (let loop ([code (make-procedure-code empty-set empty-set empty-set empty-set
							 #f #f #f #f 0 (get-annotation ast) ; ann. = name
							 (map case-info-case-code case-infos) #f)]
			      [l case-infos])
		     (if (null? l)
			 (begin
			   ; set the body
			   (zodiac:set-case-lambda-form-bodies! ast (map case-info-body case-infos))

			   ; now annotate this lambda form with the code
			   (set-annotation! ast code)

			   ; Propogate free and captured vars:
			   (register-code-vars! code)

			   ; finally return it
			   ast)

			 (loop
			  (make-procedure-code (set-union (case-code-free-vars
							   (case-info-case-code (car l)))
							  (code-free-vars code))
					       (set-union (case-code-local-vars
							   (case-info-case-code (car l)))
							  (code-local-vars code))
					       (set-union (case-info-global-vars (car l))
							  (code-global-vars code))
					       (set-union (case-info-captured-vars (car l))
							  (code-captured-vars code))
					       #f #f #f #f
					       (max (code-max-arity code)
						    (case-info-max-arity (car l)))
					       (code-name code)
					       (procedure-code-case-codes code) #f)
			  (cdr l)))))]
		
		;;--------------------------------------------------------------
		;; LET EXPRESSIONS
		;;    keep track of the bindings introduced so that each
		;;    expression can keep track of all the bindings it needs
		;;    this flattens environments
		;;    Several values may be bound at once.  In this case, 'known'
		;;    analysis is not done here.  Why are we worrying --- known
		;;    analysis is soon going to be thrown away anyway.
		;;
		;;    in let, variables are assumed to be
		;;    immutable and known; we store this information
		;;    in the binding structure in the compiler:bound structure..
		;;
		;;    (let ([x (set! y A)]) M) ->
		;;      (begin (set! y A) (let ([x #%void]) M))
		;;
		;;    if the variable bound is constant, the let is discarded,
		;;    and the value is naturally propagated.
		;;
		[(zodiac:let-values-form? ast)
		 (let* ([val (analyze! (car (zodiac:let-values-form-vals ast)) #f)]
			[vars (car (zodiac:let-values-form-vars ast))]
			[bindings 
			 (map 
			  (lambda (var)
			    (make-known-binding var (extract-value val)))
			  vars)]
			[convert-set!-val
			 (lambda ()   
			   (set-car! (zodiac:let-values-form-vals ast)
				     (zodiac:make-special-constant 
				      'void))
			   (zodiac:make-begin-form 	   
			    (zodiac:zodiac-origin ast)
			    (zodiac:zodiac-start ast)
			    (zodiac:zodiac-finish ast)
			    (make-empty-box)
			    (list val
				  ast)))])
		   
		   (if (= 1 (length (car (zodiac:let-values-form-vars ast))))
		       
		       ; this is a one-value binding let
		       (let* ([var (car vars)]
			      [known? (not (prephase:is-mutable? var))])
			 
			 (set-binding-mutable?! (car bindings) (not known?))
			 (when (not known?)
			   (set-binding-known?! (car bindings) #f))
			 (set-annotation! var (car bindings))

			 (add-local-var! var)
				 
			 (let ([body (analyze! (zodiac:let-values-form-body ast) tail?)])

			   (if (and (compiler:option:propagate-constants)
				    known? 
				    (can-propagate-constant? val)
				    ; can't eliminiate if used by invoke:
				    (not (binding-unit-i/e? (car bindings)))
				    ; can't eliminate if it is used by a bad application:
				    (not (binding-known-but-used? (car bindings))))

			       ; discard the let:
			       (begin
				 (remove-local-var! var)
				 body)

			       ; otherwise, process normally
			       (begin
				 
				 (set-car! (car (zodiac:let-values-form-vars ast)) 
					   var)
				 (zodiac:set-let-values-form-body! ast body)
				 
				 (if (zodiac:set!-form? val)
				     
				     ; if we're binding the result of a set!-form, 
				     ; turn it into
				     ; a void.
				     (convert-set!-val)
				     
				     ; if it's any other expression, we're done.
				     (begin
				       (set-car! (zodiac:let-values-form-vals ast)
						 val)
				       ast))))))
		       
		       ; this is a multiple (or zero) value binding let
		       ; the values are unknown to simple analysis so skip
		       ; that stuff
		       (begin
			 (for-each set-annotation! vars bindings)
			 ; nothing is known
			 (for-each (lambda (binding)
				     (set-binding-known?! binding #f))
				   bindings)
			 (set-car! (zodiac:let-values-form-vars ast) vars)
			 ; these are all new bindings
			 (for-each add-local-var! vars)
			 ; analyze the body
			 (let ([body (analyze! (zodiac:let-values-form-body ast) 
					       tail?)])
			   (zodiac:set-let-values-form-body! ast body))
			   
			 (if (zodiac:set!-form? val)
			     (begin
			       ((if (compiler:option:stupid) compiler:warning compiler:error)
				val
				(format
				 "returning 1 value (void) to a context expecting ~a values"
				 (length vars)))
			       (if (compiler:option:stupid)
				   (convert-set!-val)))
			     ; if it's any other option, we're done
			     (begin
			       (set-car! (zodiac:let-values-form-vals ast)
					 val)
			       ast)))		       
		       
		       ))]

		;;-----------------------------------------------------------------
		;; LETREC EXPRESSIONS
		;;
		;; getting known analysis right with LETREC is a bit more tricky
		;; if the letrec form binds only lambda values and those bindings
		;; are not mutable, we keep this as a letrec, otherwise we 
		;; transform it to a let+set! combination as R4RS.
		;;
		[(zodiac:letrec*-values-form? ast)

		 (if (and (andmap zodiac:case-lambda-form? 
				  (zodiac:letrec*-values-form-vals ast))
			  (andmap (lambda (l) (= 1 (length l))) 
				  (zodiac:letrec*-values-form-vars ast))
			  (andmap (lambda (l)
				    (not (ormap prephase:is-mutable? l)))
				  (zodiac:letrec*-values-form-vars ast)))

		     ;-----------------------------------------------------------
		     ; WELL-BEHAVED LETREC (incomplete bindings never exposed)
		     ;  mark appropriate variables as letrec bound
		     ;
		     (let* ([vars (map car (zodiac:letrec*-values-form-vars ast))]
			    [bindings (map (lambda (v) (make-unknown-binding v))
					   vars)])
		       (for-each set-annotation! vars bindings)
		       (set! local-vars (set-union (list->set vars) local-vars))
		       (let ([vals (map (lambda (val)
					  (analyze! val #f))
					(zodiac:letrec*-values-form-vals ast))]
			     [body (analyze! (zodiac:letrec*-values-form-body ast) 
					     tail?)])
			 
			 (for-each (lambda (binding val)
				     (set-binding-rec?! binding #t)
				     (set-binding-known?! binding #t)
				     (set-binding-val! binding val))
				   
				   bindings
				   vals)
			 (zodiac:set-letrec*-values-form-vals! ast vals)
			 (zodiac:set-letrec*-values-form-body! ast body)
			 ast))
			 
		     ;-----------------------------------------------------------
		     ; POSSIBLY POORLY BEHAVED LETREC
		     ;   rewrite as let+set!
		     ;
		     (begin
		       (compiler:warning ast "letrec will be rewritten with set!")
		       (debug "rewriting letrec~n")
		       (let-values ([(new-ast set-body!)
				     (letrec->let+set! ast)])
			 (debug "finished rewriting -- simulating prephase~n")
			 ; first make it look like this went through prephase!
			 (for-each 
			  (lambda (l)
			    (for-each 
			     (lambda (b) (prephase:set-mutable! b #t))
			     l))
			  (zodiac:let-values-form-vars new-ast))
			 (debug "now a-normalizing w/side-effects~n")
			 ; then a-normalize it, since it is a let, not a letrec
			 ; do it with side-effects, so set-body! still works
			 (set! new-ast (a-normalize! new-ast identity))
			 ; now replace the body (which was already a-normalized)
			 (debug "now setting body~n")
			 (set-body! (zodiac:letrec*-values-form-body ast))
			 ; then make a tail call to analyze this
			 (debug "reanalyzing...~n")

			 (analyze! new-ast tail?))))]
			   
		;;-----------------------------------------------------
		;; IF EXPRESSIONS
		;;
		;; just analyze the 3 branches.  Very easy
		[(zodiac:if-form? ast)
		 (zodiac:set-if-form-test! 
		  ast (analyze! (zodiac:if-form-test ast) #f))
		 (zodiac:set-if-form-then! 
		  ast (analyze! (zodiac:if-form-then ast) tail?))
		 (zodiac:set-if-form-else! 
		  ast (analyze! (zodiac:if-form-else ast) tail?))
		 ast]
		
		;;--------------------------------------------------------
		;; BEGIN EXPRESSIONS
		;;
		;; analyze the branches
		[(zodiac:begin-form? ast)

		 (let loop ([bodies (zodiac:begin-form-bodies ast)])
		   (if (null? (cdr bodies))
		       (set-car! bodies (analyze! (car bodies) tail?))
		       (begin
			 (set-car! bodies (analyze! (car bodies) #f))
			 (loop (cdr bodies)))))

		 ast]
		 
		
		;;--------------------------------------------------------
		;; BEGIN0 EXPRESSIONS
		;;
		;; analyze the branches
		[(zodiac:begin0-form? ast)
		 (zodiac:set-begin0-form-first!
		  ast (analyze! (zodiac:begin0-form-first ast) #f))
		 (zodiac:set-begin0-form-rest!
		  ast (analyze! (zodiac:begin0-form-rest ast) #f))
		 (let ([var (get-annotation ast)])
		   (add-local-var! var)
		   (set-annotation! var (make-begin0-binding var)))
		 ast]
		
		;;--------------------------------------------------------
		;; SET! EXPRESSIONS
		;;
		;; we analyze the target, which will register it as being
		;; mutable or used, as necessary.  Then we analyze the value.
		;;
		[(zodiac:set!-form? ast)

		 (let ([target (analyze-varref! (zodiac:set!-form-var ast) #f #t)])
		   (when (zodiac:bound-varref? target)
		     (let ([binding (compiler:bound-varref->binding target)])
		       (unless (or (binding-mutable? binding)
				   (and (binding-unit-i/e? binding)
					(prephase:set!-is-unit-definition? ast)))
			 (compiler:internal-error 
			  target 
			  (string-append
			   "analyze: variable found in set! but not"
			   " marked mutable by prephase!")))
		       (when (binding-mutable? binding)
			     (set-binding-known?! binding #f))))
		   (zodiac:set-set!-form-var! ast target)
		   (zodiac:set-set!-form-val! 
		    ast 
		    (analyze! (zodiac:set!-form-val ast) #f)))
		 
		 ast]
		
		;;---------------------------------------------------------
		;; DEFINE EXPRESSIONS
		;;
		;; defines are very tricky, eh what?
		;;
		[(zodiac:define-values-form? ast)
		 (zodiac:set-define-values-form-vars!
		  ast
		  (map (lambda (v) (analyze-varref! v #f #t)) 
		       (zodiac:define-values-form-vars ast)))
		 (zodiac:set-define-values-form-val! 
		  ast
		  (analyze! (zodiac:define-values-form-val ast) #f))
		 ast]
		
		;;-------------------------------------------------------------------
		;; APPLICATIONS
		;;   analyze all the parts.  Duh.  replace with a compiler:app
		;;   annotated with tail?
		;;  If this is a call to a primitive, check the arity.
		;;
		[(zodiac:app? ast)

		 (let* ([fun (let ([v (analyze! (zodiac:app-fun ast) #f)])
			       (if (zodiac:varref? v)
				   v
				   ; known non-procedure!
				   (let ([var (zodiac:app-fun ast)])
				     ((if (compiler:option:stupid) compiler:warning compiler:error )
				      ast 
				      "application of a non-procedure")
				     (set-binding-known-but-used?! 
				      (get-annotation (zodiac:bound-varref-binding var)) 
				      #t)
				     (analyze-varref! var #f #t))))]
			[args (map (lambda (arg)
				     (analyze! arg #f))
				   (zodiac:app-args ast))]
			[primfun? (and (zodiac:top-level-varref? fun)
				       (varref:has-attribute? fun varref:primitive)
				       (primitive? (global-defined-value (zodiac:varref-var fun))))])
		   
		   ;; check the arity for primitive apps -- just an error check
		   (when primfun?
		     (let* ([arity 
			     (arity (global-defined-value (zodiac:varref-var fun)))]
			    [num-args (length args)]
			    [arity-correct?
			     (lambda (n)
			       (or (and (number? n) (= n num-args))
				   (and (arity-at-least? n) (>= num-args
								(arity-at-least-value n)))))]
			    [arity-ok?
			     (if (list? arity)
				 (ormap arity-correct? arity)
				 (arity-correct? arity))])
				 
		       (unless arity-ok?
			 (compiler:error ast
					 (format "~a got ~a argument~a"
						 (zodiac:varref-var fun)
						 num-args
						 (if (= num-args 1)
						     ""
						     "s"))))))
		   ; for all functions, do this stuff
		   (zodiac:set-app-fun! ast fun)
		   (zodiac:set-app-args! ast args)
		   (set-annotation! 
		    ast 
		    (make-app tail? (and primfun? (zodiac:varref-var fun))))
		   (register-arity! (length args))
		   ast
		   )]
		
		;;-------------------------------------------------------------------
		;; STRUCT
		;;
		;; nothing much to do except analyze the super position
		;;
		[(zodiac:struct-form? ast)
		 (let ([super (zodiac:struct-form-super ast)])
		   (when super
		     (zodiac:set-struct-form-super! ast (analyze! super #f)))
		   ast)]

		;;--------------------------------------------------------------------
		;; UNIT
		;;    with unit, we need to make a recursive call.  We
		;;    extend the lexical environment with variables defined
		;;    in the unit and imported variables. Collect the free
		;;    variable information.
		;;
		[(zodiac:unit-form? ast)

		 (let* ([imports (zodiac:unit-form-imports ast)]
			[code (get-annotation ast)]
			[defines (unit-code-defines code)]
			[import-anchors (unit-code-import-anchors code)]
			[export-anchors (unit-code-export-anchors code)]
			[anchors (append import-anchors export-anchors)]
			[imports (zodiac:unit-form-imports ast)]
			[body (car (zodiac:unit-form-clauses ast))])

		   ;; annotate each binding
		   (for-each
		    (lambda (bound) (set-annotation! bound (make-unknown-binding bound)))
		    (append imports defines anchors))

		   ;; check the body to determine known information on the bindings;
		   ;; all bindings are known up to the first non-valuable expression
		   ;; in the unit
		   (let ([not-valueable void])
		     (let loop ([l (let ([v (car (zodiac:unit-form-clauses ast))])
				     (if (zodiac:begin-form? v)
					 (zodiac:begin-form-bodies v)
					 (list v)))])
		       (unless (null? l)
		         (let eloop ([v (car l)])
			   (cond
			    [(and (zodiac:set!-form? v)
				  (prephase:set!-is-unit-definition? v))
			     (if (analyze:valueable? (zodiac:set!-form-val v))
				 (let* ([var (zodiac:set!-form-var v)]
					[binding (get-annotation (zodiac:bound-varref-binding var))])
				   (set-binding-known?! binding #t)
				   ; The known `value' may be a quote form that hasn't yet been analyzed
				   ; (to obtain a constant varref). In this case, it will be cleaned
				   ; up by setting the constant as the annotation for the quote form
				   ; and using this annotation at constant-folding time
				   (set-binding-val! binding (extract-value (zodiac:set!-form-val v)))
				   (loop (cdr l)))
				 (not-valueable (zodiac:set!-form-val v)))]
			    [(zodiac:let-values-form? v) ; set! can be translated to (let ... (set! ...))
			     (if (andmap analyze:valueable? (zodiac:let-values-form-vals v))
				 (eloop (zodiac:let-values-form-body v))
				 (not-valueable v))]
			    [(analyze:valueable? v) (loop (cdr l))]
			    [else (not-valueable v)])))))

		   ;; recur on the body
		   (let ([body (analyze-code-body! body
						   (list->set (append imports defines anchors))
						   #t
						   code)])
		     (register-code-vars! code)
		     (set-car! (zodiac:unit-form-clauses ast) body))

		   (unless (set-empty? (set-minus (code-global-vars code)
						  (make-singleton-set
						   const:the-per-load-statics-table)))
			   (compiler:internal-error
			    ast
			    "global variables used in unit: ~a"
			    (set->list (code-global-vars code))))

		   ast)]

		;;-------------------------------------------------------------------
		;; COMPOUND UNIT
		;;
		;; nothing much to do except analyze the contituent exprs
		;;
		[(zodiac:compound-unit-form? ast)
		 (for-each (lambda (link)
			     (set-car! (cdr link)
				       (analyze! (cadr link) #f)))
			   (zodiac:compound-unit-form-links ast))
		 
		 (register-arity! (length (zodiac:compound-unit-form-links ast)))

		 (let* ([import-symbols
			 (map zodiac:binding-orig-name (zodiac:compound-unit-form-imports ast))]
			[export-symbols (map (lambda (e) (list*
							  (zodiac:read-object (car e))
							  (zodiac:read-object (cadr e))
							  (zodiac:read-object (cddr e))))
					     (zodiac:compound-unit-form-exports ast))]
			[link-symbols (map (lambda (link) 
					     (cons (zodiac:read-object (car link))
						   (map (lambda (l)
							  (if (zodiac:bound-varref? l)
							      (zodiac:binding-orig-name (zodiac:bound-varref-binding l))
							      (cons (zodiac:read-object (car l))
								    (zodiac:read-object (cdr l)))))
							(cddr link))))
					   (zodiac:compound-unit-form-links ast))]

			; Precompute refs to import names and refs to tags as integers
			; instead of names
			[tags (map car link-symbols)]
			[find-pos (lambda (s l)
				    (let loop ([l l][p 0])
				      (cond
				       [(null? l)
					(compiler:internal-error
					 ast
					 (format "can't find id ~a for building compound assembly" s))]
				       [(eq? s (car l)) p]
				       [else (loop (cdr l) (add1 p))])))]
					  
			[export-symbols
			 (map (lambda (tri)
				(cons (find-pos (car tri) tags) 
				      (let ([p (cdr tri)]) ; abbrev (a . a) as a
					(if (eq? (car p) (cdr p))
					    (car p)
					    p))))
			      export-symbols)]
			[link-symbols
			 (map (lambda (link)
				(cons (car link)
				      (map (lambda (l)
					     (if (symbol? l)
						 (find-pos l import-symbols)
						 (cons (find-pos (car l) tags)
						       (cdr l))))
					   (cdr link))))
			      link-symbols)]

			[exports (analyze-quote! 
				  (list->zodiac:quote export-symbols ast)
				  #t)]
			[imports (analyze-quote! 
				  (list->zodiac:quote (length import-symbols) ast)
				  #t)]
			[links (analyze-quote! 
				(list->zodiac:quote link-symbols ast)
				#t)])
		   
		   (let ([info (make-compound-info (compiler:add-compound-unit! ast)
						   imports
						   exports
						   links)])
		     (set-annotation! ast info)))
		 
		 ast]

		;;-----------------------------------------------------------
		;; INVOKE
		;;
		;; mark bindings as unit/i-e and remember anchors
		;; 
		[(zodiac:invoke-form? ast)
		 (zodiac:set-invoke-form-unit!
		  ast 
		  (analyze! (zodiac:invoke-form-unit ast) #f))

		 (set-annotation!
		  ast
		  (make-invoke-info
		   (map 
		    (lambda (v)
		      (let ([v (analyze-varref! v #f #t)])
			(if (zodiac:bound-varref? v)
			    (let ([binding (get-annotation (zodiac:bound-varref-binding v))])
			      (binding-anchor binding))
			    v)))
		    (zodiac:invoke-form-variables ast))))
		 
		 ; unit + vars + anchors as args:
		 (register-arity! (+ 1 (* 2 (length (zodiac:invoke-form-variables ast)))))
		 
		 ast]

		;;-----------------------------------------------------------
		;; CLASS
		;;
		[(zodiac:class*/names-form? ast)
		 (let* ([code (get-annotation ast)]
			[public-lookup-bindings (class-code-public-lookup-bindings code)]
			[public-define-bindings (class-code-public-define-bindings code)]
			[private-bindings (class-code-private-bindings code)]
			[inherit-bindings (class-code-inherit-bindings code)]
			[rename-bindings (class-code-rename-bindings code)]
			[bindings (append (list (zodiac:class*/names-form-this ast)
						(zodiac:class*/names-form-super-init ast))
					  (map
					   (lambda (b) (if (pair? b) (car b) b))
					   (zodiac:paroptarglist-vars
					    (zodiac:class*/names-form-init-vars ast)))
					  public-lookup-bindings
					  public-define-bindings
					  private-bindings
					  inherit-bindings
					  rename-bindings)])

		   ;; annotate each binding
		   (for-each
		    (lambda (bound) (set-annotation! bound (make-unknown-binding bound)))
		    bindings)
		   
		   ; Analyze super-expr & interfaces
		   (zodiac:set-class*/names-form-super-expr! 
		    ast 
		    (analyze! (zodiac:class*/names-form-super-expr ast) #f))
		   (zodiac:set-class*/names-form-interfaces! 
		    ast
		    (map (lambda (i) (analyze! i #f))
			 (zodiac:class*/names-form-interfaces ast)))
		   
		   ; To create the class assembly, super + interfaces are collected as args
		   (register-arity! (add1 (length (zodiac:class*/names-form-interfaces ast))))

		   ; Now body, collecting closure info
		   (let ([l (zodiac:sequence-clause-exprs
			     (car (zodiac:class*/names-form-inst-clauses ast)))])
		     (set-car! l
			       (analyze-code-body! (car l)
						   (list->set bindings)
						   #f
						   code)))

		   ; Now init args, collecting more closure info
		   (class-init-defaults-map!
		    ast
		    (lambda (var ast)
		      (analyze-code-body! ast
					  (list->set bindings)
					  #f
					  code)))

		   (register-code-vars! code)

		   ast)]

		;;-------------------------------------------------------------------
		;; INTERFACE
		;;
		;; nothing much to do except analyze the super exprs
		;;
		[(zodiac:interface-form? ast)
		 (zodiac:set-interface-form-super-exprs!
		  ast
		  (map (lambda (expr) (analyze! expr #f))
		       (zodiac:interface-form-super-exprs ast)))

		 (register-arity! (length (zodiac:interface-form-super-exprs ast)))
		 
		 (set-interface-info-assembly! (get-annotation ast) 
					       (compiler:add-interface! ast))
		 
		 ast]
		
		[else (compiler:internal-error
		       ast
		       (format "unsupported syntactic form (~a)"
			       (if (struct? ast)
				   (vector-ref (struct->vector ast) 0)
				   ast)))]))])
			  	
	;; analyze the expression and return it with the local variables
	;; it creates.  
	(let ([ast (analyze! ast tail?)])
	  (values ast
		  free-vars
		  local-vars
		  global-vars
		  locals-captured
		  max-arity))))))

)

#|

(define (analyze-go . f)
  (set! compiler:messages null)
  (let* ([p (if (null? f)
		(current-input-port)
		(open-input-file (car f) 'text))]
	 [r (zodiac:scheme-expand ((zodiac:read p)))]
	 [r (prephase! r)]
	 [x (a-normalize r identity)])
    (set! compiler:define-list null)
    (set! compiler:static-list null)
    (set! compiler:per-load-static-list null)
    (let-values ([(exp _ __ % %%) (analyze-expression! x empty-set #f)])
      (printf "ok")
      (append compiler:define-list (list exp)))))
|#
