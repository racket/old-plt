;; Known-value analysis
;; (c) 1996-7 Sebastian Good
;; (c) 1997-8 PLT, Rice University

;; Sets the the real annotation for zodiac:binding AST nodes,
;; setting the known? and known-val fields as possible.

;; Known-value analysis is used for constant propagation, but
;;  more importantly, it's used for compiling tail recusrsion
;;  as a goto. mzc can only compile tail recursion as a goto
;;  when it knows the actual destination of the jump.

;; Note that ``known'' means we know an AST that provides the
;; value; this AST could be arbitrarily complex, so we really
;; only know the value if the known AST is simple enough.

;;; Annotatitons: ----------------------------------------------
;;    binding - `binding' structure (replaces prephase 
;;      binding-properties structure)
;;    application - `app' structure
;;; ------------------------------------------------------------

(unit/sig compiler:known^
  (import (compiler:option : compiler:option^)
	  compiler:library^
	  compiler:cstructs^
	  (zodiac : zodiac:system^)
	  compiler:zlayer^
	  compiler:prephase^
	  compiler:anorm^
	  compiler:const^
	  compiler:closure^
	  compiler:rep^
	  compiler:driver^
	  mzlib:function^
	  (mrspidey : compiler:mrspidey^))
  
  ;; helper functions to create a binding annotation
  (define make-known-binding
    (lambda (bound val)
      (make-binding #f (prephase:is-mutable? bound) 
		    (prephase:is-unit-i/e? bound) (prephase:binding-anchor bound)
		    #f (prephase:is-ivar? bound)
		    #t val #f #f)))
  (define make-unknown-letbound-binding
    (lambda (mutable?)
      (make-binding #f mutable?
		    #f #f
		    #f #f
		    #f #f #f #f)))
  (define make-unknown-binding
    (lambda (bound)
      (make-binding #f (prephase:is-mutable? bound) 
		    (prephase:is-unit-i/e? bound) (prephase:binding-anchor bound)
		    #f (prephase:is-ivar? bound)
		    #f #f #f #f)))
  (define make-begin0-binding
    (lambda (bound)
      (make-binding #f #f #f #f #f #f
		    #f #f #f (make-rep:atomic 'begin0-saver))))
  (define make-wcm-binding
    (lambda (bound)
      (make-binding #f #f #f #f #f #f
		    #f #f #f (make-rep:atomic 'wcm-saver))))

  ;; Determine whether a varref is a known primitive
  (define (analyze:prim-fun fun)
    (and (zodiac:top-level-varref? fun)
	 (varref:has-attribute? fun varref:primitive)
	 (primitive? (global-defined-value (zodiac:varref-var fun)))
	 (zodiac:varref-var fun)))

  ;; Some prims call given procedures directly, some install procedures
  ;;  to be called later, and some call previously installed procedures.
  ;;  We care abot the installers and callers.
  (define prims-that-induce-procedure-calls
    '(#%apply #%map #%for-each #%andmap #%ormap #%make-promise
	      #%dynamic-wind #%thread #%call-in-nested-thread
	      #%make-object #%call-with-values #%time-apply
	      #%call-with-output-file #%call-with-input-file
	      #%with-output-to-file #%with-input-from-file
	      #%exit-handler #%current-eval #%current-exception-handler
	      #%current-prompt-read #%current-load
	      #%call-with-escape-continuation #%call-with-current-continuation
	      #%current-print #%port-display-handler #%port-write-handler
	      #%port-print-handler #%global-port-print-handler
	      #%error-display-handler #%error-escape-handler
	      #%port-read-handler #%error-value->string-handler
	      #%call/ec #%call/cc #%hash-table-get
	      #%hash-table-map #%hash-table-for-each #%make-input-port #%make-output-port))

  ;; The valueable? predicate is used to determine how many variables
  ;;  are reliably set in a mutatlly-recusrive binding context.
  (define (analyze:valueable? v extra-known-bindings extra-unknown-bindings known-lambdas)
    (let loop ([v v][extra-known-bindings extra-known-bindings])
      (cond
       [(and (zodiac:set!-form? v)
	     (prephase:set!-is-unit-definition? v))
	(if (loop (zodiac:set!-form-val v) extra-known-bindings)
	    (let* ([var (zodiac:set!-form-var v)]
		   [binding (get-annotation (zodiac:bound-varref-binding var))])
	      (unless (binding-mutable? binding)
		(set-binding-known?! binding #t)
		(let ([val (extract-ast-known-value (zodiac:set!-form-val v))])
		  (set-binding-val! binding val)))
	      #t)
	    #f)]
   
       [(zodiac:quote-form? v) #t]
       [(zodiac:bound-varref? v)
	; the varref must not be unit-i/e?, or it must be in extra-known-bindings
	; and it must not be in extra-unknown-bindings
	(let ([zbinding (zodiac:bound-varref-binding v)])
	  (and (not (memq zbinding extra-unknown-bindings))
	       (or (memq zbinding extra-known-bindings)
		   (let ([b (get-annotation zbinding)])
		     (or (and (binding-properties? b)
			      (not (binding-properties-unit-i/e? b)))
			 (and (binding? b)
			      (or (not (binding-unit-i/e? b))
				  (binding-known? b))))))))]
       [(zodiac:varref? v) #t]
       [(zodiac:case-lambda-form? v) #t]
       [(zodiac:unit-form? v) #t]
       [(zodiac:begin-form? v)
	(andmap (lambda (v) (loop v extra-known-bindings)) (zodiac:begin-form-bodies v))]
       [(zodiac:begin0-form? v)
	(andmap (lambda (v) (loop v extra-known-bindings)) (zodiac:begin0-form-bodies v))]
       [(zodiac:with-continuation-mark-form? v)
	(and (loop (zodiac:with-continuation-mark-form-key v) extra-known-bindings)
	     (loop (zodiac:with-continuation-mark-form-val v) extra-known-bindings)
	     (loop (zodiac:with-continuation-mark-form-body v) extra-known-bindings))]
       [(zodiac:set!-form? v) #f] ; because it changes a variable
       [(zodiac:struct-form? v)
	(loop (zodiac:struct-form-super v) extra-known-bindings)]
       [(zodiac:if-form? v)
	(and (loop (zodiac:if-form-test v) extra-known-bindings)
	     (loop (zodiac:if-form-then v) extra-known-bindings)
	     (loop (zodiac:if-form-else v) extra-known-bindings))]
       [(zodiac:let-values-form? v)
	(and (andmap (lambda (vars v) (if (loop v extra-known-bindings)
					  (begin
					    (when (= 1 (length vars))
					      (prephase:set-known-val! (car vars) v))
					    #t)
					  #f))
		     (zodiac:let-values-form-vars v)
		     (zodiac:let-values-form-vals v))
	     (loop (zodiac:let-values-form-body v)
		   (append (apply append (zodiac:let-values-form-vars v))
			   extra-known-bindings)))]
       [(zodiac:letrec*-values-form? v)
	(and (andmap (lambda (vars v) (if (loop v extra-known-bindings)
					  (begin
					    (when (= 1 (length vars))
					      (prephase:set-known-val! (car vars) v))
					    #t)
					  #f))
		     (zodiac:letrec*-values-form-vars v)
		     (zodiac:letrec*-values-form-vals v))
	     (loop (zodiac:letrec*-values-form-body v)
		   (append (apply append (zodiac:letrec*-values-form-vars v))
			   extra-known-bindings)))]
       [(zodiac:app? v)
	(let* ([fun (zodiac:app-fun v)]
	       [primfun (analyze:prim-fun fun)]
	       [args (zodiac:app-args v)]
	       [args-ok? (lambda ()
			   (andmap (lambda (v) (loop v extra-known-bindings)) args))])
	  (if primfun

	      ;; Check whether the primitive can call any procedures:
	      (and (not (memq primfun prims-that-induce-procedure-calls))
		   (args-ok?))
	      
	      ;; Interesting special case: call a known function
	      (and (loop fun extra-known-bindings)
		   (args-ok?)
		   (let ([simple-case-lambda?
			  ;; We know nothing about the args; still valueable?
			  ;; What if we're trying to check a recursive function?
			  ;;  If we encounter a cycle, the body is valueable.
			  (lambda (v)
			    (or (memq v known-lambdas)
				(andmap
				 (lambda (body) (analyze:valueable? 
						 body 
						 extra-known-bindings
						 extra-unknown-bindings
						 (cons v known-lambdas)))
				 (zodiac:case-lambda-form-bodies v))))])
		     (cond
		      [(zodiac:bound-varref? fun)
		       (let ([v (extract-varref-known-val fun)])
			 (and v 
			      (cond
			       [(zodiac:case-lambda-form? v)
				(simple-case-lambda? v)]
			       [(zodiac:top-level-varref? v)
				;; Could be a friendly primitive...
				(let ([primfun (analyze:prim-fun v)])
				  (and primfun
				       (not (memq primfun prims-that-induce-procedure-calls))))]
			       [else #f])))]
		      [(zodiac:case-lambda-form? fun) (simple-case-lambda? fun)]
		      [else #f])))))]

       [else #f])))

  ;; extract-ast-known-value tries to extract a useful value from a known-value AST
  (define (extract-ast-known-value v)
    (let extract-value ([v v])
      (cond
       [(zodiac:set!-form? v) (zodiac:make-special-constant 'void)]
       [(zodiac:begin-form? v) (extract-value (car (last-pair (zodiac:begin-form-bodies v))))]
       [(zodiac:begin0-form? v) (extract-value (car (zodiac:begin0-form-bodies v)))]
       [(zodiac:with-continuation-mark-form? v) (extract-value (zodiac:with-continuation-mark-form-body v))]
       [(zodiac:let-values-form? v) (extract-value (zodiac:let-values-form-body v))]
       [(zodiac:letrec*-values-form? v) (extract-value (zodiac:letrec*-values-form-body v))]
       [(zodiac:app? v)
	(let ([fun (analyze:prim-fun (zodiac:app-fun v))])
	  (if fun
	      (let ([args (map extract-value (zodiac:app-args v))])
		(case fun
		  [(#%void) (zodiac:make-special-constant 'void)]
		  [(#%char->integer) 
		   (with-handlers ([void (lambda (x) v)])
		     (let ([args (map (lambda (a) (zodiac:read-object (zodiac:quote-form-expr a))) args)])
		       (let ([new-v (apply (global-defined-value fun) args)])
			 (zodiac:make-quote-form
			  (zodiac:zodiac-origin v)
			  (zodiac:zodiac-start v)
			  (zodiac:zodiac-finish v)
			  (make-empty-box)
			  (zodiac:structurize-syntax new-v v)))))]
		  [else v]))
	      v))]
       [else v])))

  ;; extract-varref-known-val works for bindings, too.
  (define (extract-varref-known-val v)
    (if (top-level-varref/bind-from-lift? v)
	(top-level-varref/bind-from-lift-lambda v)
	(let loop ([v v])
	  (let* ([zbinding (if (zodiac:binding? v)
			       v
			       (zodiac:bound-varref-binding v))]
		 [binding (get-annotation zbinding)]
		 [result (lambda (v)
			   (cond
			    [(top-level-varref/bind-from-lift? v)
			     (extract-varref-known-val v)]
			    [(or (zodiac:bound-varref? v)
				 (zodiac:binding? v))
			     (loop v)]
			    [else v]))])
	    (and binding
		 (cond
		  [(binding? binding)
		   (and (binding-known? binding)
			(result (binding-val binding)))]
		  [else
		   (result (prephase:known-val zbinding))]))))))

  ;; analyze-knowns! sets the annotation for binding occurrences, setting information
  ;; about known variables. Also sets the annotation for applications.
  (define analyze-knowns!
    (letrec ([analyze!
	      (lambda (ast)
		(when (compiler:option:debug)
		  (zodiac:print-start! debug:port ast)
		  (newline debug:port))
		
		(cond
		
		 ;;-----------------------------------------------------------------
		 ;; CONSTANTS (A-VALUES)
		 [(zodiac:quote-form? ast) ast]
			
		 ;;-----------------------------------------------------------------
		 ;; VARIABLE REFERENCES (A-VALUES)
		 ;;
		 [(zodiac:bound-varref? ast) ast]
		 [(zodiac:top-level-varref? ast) 
		  ;; Make sure no unit defs slipped through:
		  (when (and (zodiac:top-level-varref/bind/unit? ast)
			     (zodiac:top-level-varref/bind/unit-unit? ast))
		    (compiler:internal-error ast "unit-top-level found after prephase"))
		  ast]
						
		 ;;--------------------------------------------------------------------
		 ;; LAMBDA EXPRESSIONS
		 ;;    analyze the bodies, and set binding info for the binding vars
		 ;;
		 [(zodiac:case-lambda-form? ast)
		  (zodiac:set-case-lambda-form-bodies! 
		   ast 
		   (map
		    (lambda (args body)
		      
		      ;; annotate each binding with our information
		      (for-each
		       (lambda (bound) (set-annotation! bound (make-known-binding bound #f)))
		       (zodiac:arglist-vars args))
		      
		      (analyze! body))
		    (zodiac:case-lambda-form-args ast)
		    (zodiac:case-lambda-form-bodies ast)))

		  ast]
		
		 ;;--------------------------------------------------------------
		 ;; LET EXPRESSIONS
		 ;;    Several values may be bound at once, in which case 'known'
		 ;;    analysis is not performed.
		 ;;
		 ;;   Variables are assumed to be immutable and known, unless
		 ;;    proven otherwise; we store this information
		 ;;    in the binding structure in the compiler:bound structure.
		 ;;
		 [(zodiac:let-values-form? ast)
		  (let* ([val (analyze! (car (zodiac:let-values-form-vals ast)))]
			 [vars (car (zodiac:let-values-form-vars ast))]
			 [bindings (map 
				    (lambda (var)
				      (make-known-binding var (extract-ast-known-value val)))
				    vars)])
		   
		    (for-each set-annotation! vars bindings)
		    (set-car! (zodiac:let-values-form-vals ast) val)
		    
		    (if (= 1 (length vars))
		       
			; this is a one-value binding let
			(let* ([var (car vars)])
			  
			  (when (binding-mutable? (car bindings))
			    (set-binding-known?! (car bindings) #f)))
			
			; this is a multiple (or zero) value binding let
			; the values are unknown to simple analysis so skip
			; that stuff;
			; nothing is known
			(for-each (lambda (binding) (set-binding-known?! binding #f))
				  bindings))

		    ; analyze the body
		    (let ([body (analyze! (zodiac:let-values-form-body ast))])
		      (zodiac:set-let-values-form-body! ast body)))
		      
		  ast]

		 ;;-----------------------------------------------------------------
		 ;; LETREC EXPRESSIONS
		 ;;
		 [(zodiac:letrec*-values-form? ast)
		  
		  (let* ([varses (zodiac:letrec*-values-form-vars ast)]
			 [vals (zodiac:letrec*-values-form-vals ast)])
		 
		    ; Annotate each binding occurrence
		    (for-each
		     (lambda (vars)
		       (for-each (lambda (var)
				   (let ([binding (make-unknown-binding var)])
				     (set-annotation! var binding)))
				 vars))
		     varses)
		    
		    ; Mark known letrec-bound vars
		    (let loop ([varses varses][vals vals][done-vars null])
		      (unless (null? vals)
			(when (analyze:valueable? (car vals) done-vars (apply append varses) null)
			  
			  ; Continue known marking
			  (let ([vars (car varses)])
			    (when (= 1 (length vars))
			     (let ([binding (get-annotation (car vars))])
			       (unless (binding-mutable? binding)
				 (set-binding-known?! binding #t)
				 (set-binding-val! binding (car vals)))))
			    (loop (cdr varses) (cdr vals)
				  (append vars done-vars))))))

		    (zodiac:set-letrec*-values-form-vals! ast (map analyze! vals))
		    
		    (zodiac:set-letrec*-values-form-body!
		     ast
		     (analyze! (zodiac:letrec*-values-form-body ast)))
		    
		    ast)]
		 
		 ;;-----------------------------------------------------
		 ;; IF EXPRESSIONS
		 ;;
		 ;;  analyze the 3 branches.
		 ;;
		 [(zodiac:if-form? ast)
		  (zodiac:set-if-form-test! ast (analyze! (zodiac:if-form-test ast)))
		  (let ([then (analyze! (zodiac:if-form-then ast))]
			[else (analyze! (zodiac:if-form-else ast))])
		    (zodiac:set-if-form-then! ast then)
		    (zodiac:set-if-form-else! ast else)
		   
		    ast)]
		
		 ;;--------------------------------------------------------
		 ;; BEGIN EXPRESSIONS
		 ;;
		 ;; analyze the branches
		 [(zodiac:begin-form? ast)
		  
		  (let loop ([bodies (zodiac:begin-form-bodies ast)])
		    (if (null? (cdr bodies))
			(let ([e (analyze! (car bodies))])
			  (set-car! bodies e))
			(begin
			  (set-car! bodies (analyze! (car bodies)))
			  (loop (cdr bodies)))))
		  
		  ast]
		 
		
		 ;;--------------------------------------------------------
		 ;; BEGIN0 EXPRESSIONS
		 ;;
		 ;; analyze the branches
		 [(zodiac:begin0-form? ast)
		  (zodiac:set-begin0-form-first! ast (analyze! (zodiac:begin0-form-first ast)))
		  (zodiac:set-begin0-form-rest! ast (analyze! (zodiac:begin0-form-rest ast)))
		  (let ([var (get-annotation ast)])
		    (set-annotation! var (make-begin0-binding var)))
		  ast]
		
		 ;;--------------------------------------------------------
		 ;; SET! EXPRESSIONS
		 ;;
		 ;; we analyze the target, which will register it as being
		 ;; mutable or used, as necessary.  Then we analyze the value.
		 ;;
		 [(zodiac:set!-form? ast)

		  (let ([target (analyze! (zodiac:set!-form-var ast))])
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
		     (analyze! (zodiac:set!-form-val ast))))
		 
		  ast]
		 
		 ;;---------------------------------------------------------
		 ;; DEFINE EXPRESSIONS
		 ;;
		 [(zodiac:define-values-form? ast)
		  (zodiac:set-define-values-form-vars!
		   ast
		   (map (lambda (v) (analyze! v))
			(zodiac:define-values-form-vars ast)))
		  (zodiac:set-define-values-form-val! 
		   ast
		   (analyze! (zodiac:define-values-form-val ast)))
		  ast]
		
		 ;;-------------------------------------------------------------------
		 ;; APPLICATIONS
		 ;;  analyze all the parts, and note whether the rator is
		 ;;  a primitive;
		 ;;  if this is a call to a primitive, check the arity.
		 ;;
		 [(zodiac:app? ast)
		  
		  (let* ([fun (analyze! (zodiac:app-fun ast))]
			 [args (map (lambda (arg) (analyze! arg))
				    (zodiac:app-args ast))]
			 [primfun (analyze:prim-fun fun)]
			 [primfun-arity-ok?
			  ;; check the arity for primitive apps -- just an error check
			  (and primfun
			       (let* ([num-args (length args)]
				      [arity-ok? (procedure-arity-includes?
						  (global-defined-value primfun)
						  num-args)])
				 (unless arity-ok?
				   ((if (compiler:option:stupid)
					compiler:warning
					compiler:error)
				    ast
				    (format "~a got ~a argument~a"
					    (zodiac:varref-var fun)
					    num-args
					    (if (= num-args 1)
						""
						"s"))))
				 arity-ok?))]
			 [prim? (and primfun primfun-arity-ok?)])
		    
		    ; for all functions, do this stuff
		    (zodiac:set-app-fun! ast fun)
		    (zodiac:set-app-args! ast args)
		    (set-annotation! 
		     ast 
		     (make-app #f prim? (and prim? primfun)))
		    
		    ast)]
		
		 ;;-------------------------------------------------------------------
		 ;; STRUCT
		 ;;
		 ;; nothing much to do except analyze the super position
		 ;;
		 [(zodiac:struct-form? ast)
		  (let ([super (zodiac:struct-form-super ast)])
		    (when super
		      (zodiac:set-struct-form-super! ast (analyze! super)))
		    ast)]

		 ;;--------------------------------------------------------------------
		 ;; UNIT
		 ;;
		 [(zodiac:unit-form? ast)
		  
		  (let* ([code (get-annotation ast)]
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
		    (let ([not-valueable 
			   (lambda (v)
			     (compiler:warning 
			      v
			      "end known definitions in unit"))])
		      (let loop ([l (let ([v (car (zodiac:unit-form-clauses ast))])
				      (if (zodiac:begin-form? v)
					  (zodiac:begin-form-bodies v)
					  (list v)))])
			(unless (null? l)
			  (let eloop ([v (car l)][extra-known-bindings imports])
			    (cond
			     ; analyze:valueable? also annotates unit-definition-set!ed 
			     ;  bindings as known
			     [(analyze:valueable? v extra-known-bindings null null)
			      (loop (cdr l))]
			     [else (not-valueable v)])))))

		    ;; recur on the body
		    (let ([body (analyze! body)])
		      (set-car! (zodiac:unit-form-clauses ast) body))

		    ast)]

		 ;;-------------------------------------------------------------------
		 ;; COMPOUND UNIT
		 ;;
		 ;; nothing much to do except analyze the contituent exprs
		 ;;
		 [(zodiac:compound-unit-form? ast)
		  
		  (for-each (lambda (link)
			      (set-car! (cdr link)
					(analyze! (cadr link))))
			    (zodiac:compound-unit-form-links ast))
		  
		  ast]

		 ;;-----------------------------------------------------------
		 ;; INVOKE
		 ;;
		 [(zodiac:invoke-unit-form? ast)
		  (zodiac:set-invoke-unit-form-unit!
		   ast 
		   (analyze! (zodiac:invoke-unit-form-unit ast)))
		  
		  ast]
		 
		 ;;-----------------------------------------------------------
		 ;; CLASS
		 ;;
		 [(zodiac:class*/names-form? ast)
		  (let* ([code (get-annotation ast)]
			 [public-lookup-bindings (class-code-public-lookup-bindings code)]
			 [public-define-bindings (class-code-public-define-bindings code)]
			 [override-lookup-bindings (class-code-override-lookup-bindings code)]
			 [override-define-bindings (class-code-override-define-bindings code)]
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
					   override-lookup-bindings
					   override-define-bindings
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
		     (analyze! (zodiac:class*/names-form-super-expr ast)))
		    (zodiac:set-class*/names-form-interfaces! 
		     ast
		     (map (lambda (i) (analyze! i))
			  (zodiac:class*/names-form-interfaces ast)))
		    
		    ; Now body
		    (let ([l (zodiac:sequence-clause-exprs
			      (car (zodiac:class*/names-form-inst-clauses ast)))])
		      (set-car! l (analyze! (car l))))
		    
		    ; Now analyze init arg defaults
		    (class-init-defaults-map!
		     ast
		     (lambda (var ast) (analyze! ast)))
		    
		    ast)]
		 
		 ;;-------------------------------------------------------------------
		 ;; INTERFACE
		 ;;
		 ;; analyze the super exprs
		 ;;
		 [(zodiac:interface-form? ast)
		  (zodiac:set-interface-form-super-exprs!
		   ast
		   (map (lambda (expr) (analyze! expr))
			(zodiac:interface-form-super-exprs ast)))

		  ast]

		 ;;-------------------------------------------------------------------
		 ;; WITH-CONTINUATION-MARK
		 ;;
		 ;; analyze the key, val, and body, and the binding in the annotation
		 ;;
		 [(zodiac:with-continuation-mark-form? ast)
		  
		  (zodiac:set-with-continuation-mark-form-key!
		   ast
		   (analyze! (zodiac:with-continuation-mark-form-key ast)))
		 
		  (zodiac:set-with-continuation-mark-form-val!
		   ast
		   (analyze! (zodiac:with-continuation-mark-form-val ast)))
		 
		  (zodiac:set-with-continuation-mark-form-body!
		   ast
		   (analyze! (zodiac:with-continuation-mark-form-body ast)))

		  (let ([var (get-annotation ast)])
		    (set-annotation! var (make-wcm-binding var)))
		  
		  ast]
		
		 [else (compiler:internal-error
			ast
			(format "unsupported syntactic form (~a)"
				(if (struct? ast)
				    (vector-ref (struct->vector ast) 0)
				    ast)))]))])
			  	
      (lambda (ast)
	(analyze! ast))))

)
