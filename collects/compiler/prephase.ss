;; pre-compilation scan
;; (c) 1996-7 Sebastian Good

(unit/sig
 compiler:prephase^
 (import (compiler:option : compiler:option^)
	 compiler:library^
	 compiler:cstructs^
	 (zodiac : zodiac:system^)
	 compiler:zlayer^
	 compiler:driver^
	 (mrspidey : compiler:mrspidey^))

;; notes mutability of lexical variables
;; flags forms that are not supported by the compiler
;; flags redefinitions of global variables
;; can rewrite stuff if it wants

(define-struct binding-properties (mutable? unit-i/e? ivar? anchor))

(define (prephase:init-binding-properties! binding mutable? unit-i/e? ivar?)
  (set-annotation! binding (make-binding-properties mutable? unit-i/e? ivar? #f)))

(define (prephase:set-mutable! binding mutable?)
  (set-binding-properties-mutable?! (get-annotation binding) mutable?))
(define (prephase:set-binding-anchor! binding a)
  (set-binding-properties-anchor! (get-annotation binding) a))
(define (prephase:set-unit-i/e?! binding i/e?)
  (set-binding-properties-unit-i/e?! (get-annotation binding) i/e?))

(define (prephase:is-mutable? binding)
  (let ([p (get-annotation binding)])
    (and p (binding-properties-mutable? p))))
(define (prephase:is-unit-i/e? binding)
  (let ([p (get-annotation binding)])
    (and p (binding-properties-unit-i/e? p))))
(define (prephase:is-ivar? binding)
  (let ([p (get-annotation binding)])
    (and p (binding-properties-ivar? p))))
(define (prephase:binding-anchor binding)
  (let ([p (get-annotation binding)])
    (and p (binding-properties-anchor p))))

;; what can be thrown away in a begin?

(define prephase:dead-expression?
  (one-of zodiac:bound-varref? zodiac:quote-form?
	  zodiac:case-lambda-form? zodiac:struct-form?))

;; what can be pushed in a begin0?

(define prephase:begin0-pushable?
  (one-of zodiac:case-lambda-form? zodiac:quote-form?))

;; returns a true value if the symbol is a primitive function.
;; if the assume-primitives? option is true, it set! the name
;; to the #% form.
(define prephase:primitive-name?!
  (lambda (ast in-unit?)
    (let* ([sym (zodiac:varref-var ast)]
	   [str (symbol->string sym)]
	   [len (string-length str)]
	   [built-in (built-in-name sym)]
	   [hash-percent? (eq? sym built-in)])
      
      (if (and built-in (or (compiler:option:assume-primitives)
			    in-unit?))
	  
	  (begin
	    (zodiac:set-varref-var! ast (built-in-name sym))
	    #t)
	       
	  hash-percent?))))

;; takes a vars=val statement and allows only one set! at a time
;;
(define prephase:multiple-set!->single-set!
  (lambda (vars val make-set! list? ast)
    (cond
      [(null? vars)
       ; zero value set!  -- weirdos
       (mrspidey:copy-annotations!
	(zodiac:make-let-values-form
	 (zodiac:zodiac-origin ast)
	 (zodiac:zodiac-start ast)
	 (zodiac:zodiac-finish ast)
	 (make-empty-box)
	 (list null)
	 (list val)
	 (zodiac:make-special-constant 'void))
	ast)]
      
      [(null? (cdr vars))
       ; single value set! anyway -- common case, make it simple
       (make-set! (zodiac:zodiac-origin ast)
		  (zodiac:zodiac-start ast)
		  (zodiac:zodiac-finish ast)
		  (zodiac:parsed-back ast)
		  (if list? (list (car vars)) (car vars))
		  val)]
      
      [else
       ; multiple value set! 
       (let* ([names (map (lambda (_) (gensym)) vars)]
	      [bindings (map (lambda (name)
			       (let ([b (zodiac:make-lexical-binding
					 (zodiac:zodiac-origin ast)
					 (zodiac:zodiac-start ast)
					 (zodiac:zodiac-finish ast)
					 (make-empty-box)
					 name
					 name)])
				 (prephase:init-binding-properties! b #f #f #f)
				 b))
			     names)])
	 (mrspidey:copy-annotations!
	  (zodiac:make-let-values-form
	   (zodiac:zodiac-origin ast)
	   (zodiac:zodiac-start ast)
	   (zodiac:zodiac-finish ast)
	   (make-empty-box)
	   (list bindings)
	   (list val)
	   (let loop ([bindings bindings] [vars vars])
	     (if (null? bindings)
		 (zodiac:make-special-constant 'void)
		 (zodiac:make-begin-form
		  (zodiac:zodiac-origin ast)
		  (zodiac:zodiac-start ast)
		  (zodiac:zodiac-finish ast)
		  (zodiac:parsed-back ast)
		  (list
		   (make-set!
		    (zodiac:zodiac-origin ast)
		    (zodiac:zodiac-start ast)
		    (zodiac:zodiac-finish ast)
		    (make-empty-box)
		    (if list? (list (car vars)) (car vars))
		    (zodiac:binding->lexical-varref
		     (car bindings)))
		   (loop (cdr bindings) (cdr vars)))))))
	  ast))])))

(define (new-binding gensym set-box!)
  (lambda (tlv)
    (let ([bind (zodiac:make-lexical-binding
		 (zodiac:zodiac-origin tlv)
		 (zodiac:zodiac-start tlv)
		 (zodiac:zodiac-finish tlv)
		 (make-empty-box)
		 (gensym (zodiac:varref-var tlv))
		 (zodiac:varref-var tlv))])
      (set-box! (zodiac:top-level-varref/bind-slot tlv) bind)
      (mrspidey:copy-annotations! bind tlv)
      bind)))

(define (new-varref binding)
  (mrspidey:copy-annotations!
   (zodiac:make-lexical-varref
    (zodiac:zodiac-origin binding)
    (zodiac:zodiac-start binding)
    (zodiac:zodiac-finish binding)
    (make-empty-box)
    (zodiac:binding-var binding)
    binding)
   binding))

(define (make-unitdef-set!-form . args)
  (let ([ast (apply zodiac:make-set!-form args)])
    (set-annotation! ast 'unit-def)
    ast))

(define (prephase:set!-is-unit-definition? ast)
  (eq? (get-annotation ast) 'unit-def))

(define ugensym (lambda (s) (gensym (symbol-append 'u s))))

(define (prephase:unit-vars->lexical-vars ast)
  (let loop ([l (zodiac:unit-form-clauses ast)]
	     [c-acc null]
	     [dv-acc null])
    (if (null? l)
	(values (reverse! c-acc) dv-acc)
	(let ([ast (car l)]
	      [rest (cdr l)])
	  (cond
	   [(zodiac:define-values-form? ast)
	    (let* ([tlvs (zodiac:define-values-form-vars ast)]
		   [dvar-bindings (map (new-binding ugensym set-box!) tlvs)]
		   [dvars (map new-varref dvar-bindings)]
		   [set!s
		    (if (= (length dvars) 1)
			(make-unitdef-set!-form
			 (zodiac:zodiac-origin ast)
			 (zodiac:zodiac-start ast)
			 (zodiac:zodiac-finish ast)
			 (zodiac:parsed-back ast)
			 (car dvars)
			 (zodiac:define-values-form-val ast))
			(let* ([inter-bindings (map (new-binding ugensym void) tlvs)]
			       [inters (map new-varref inter-bindings)])
			  (zodiac:make-let-values-form
			   (zodiac:zodiac-origin ast)
			   (zodiac:zodiac-start ast)
			   (zodiac:zodiac-finish ast)
			   (make-empty-box)
			   (list inter-bindings)
			   (list (zodiac:define-values-form-val ast))
			   (zodiac:make-begin-form
			    (zodiac:zodiac-origin ast)
			    (zodiac:zodiac-start ast)
			    (zodiac:zodiac-finish ast)
			    (make-empty-box)
			    (map
			     (lambda (inter dvar)
			       (make-unitdef-set!-form
				(zodiac:zodiac-origin ast)
				(zodiac:zodiac-start ast)
				(zodiac:zodiac-finish ast)
				(zodiac:parsed-back ast)
				dvar
				inter))
			     inters
			     dvars)))))])
	      (loop rest
		    (cons set!s c-acc)
		    (append dvar-bindings dv-acc)))]
	   [(zodiac:begin-form? ast)
	    ; inline the body and try again
	    (loop (append
		   (zodiac:begin-form-bodies ast)
		   rest)
		  c-acc
		  dv-acc)]
	   [else
	    (loop rest
		  (cons ast c-acc)
		  dv-acc)])))))

; Returns (values public-lookup-bindings
;                 public-define-bindings
;		  private-bindings
;		  inherit-bindings
;		  rename-bindings
;		  body)
(define (prephase:class-clauses->begin ast)
  (let* ([plb-acc null] 
	 [pdb-acc null]
	 [pri-acc null] 
	 [inh-acc null]
	 [ren-acc null]
	 [body-acc null]
	 [add-set! (lambda (varref expr)
		     (set! body-acc
			   (cons
			    (zodiac:make-set!-form
			     (zodiac:zodiac-origin expr)
			     (zodiac:zodiac-start expr)
			     (zodiac:zodiac-finish expr)
			     (zodiac:parsed-back expr)
			     varref
			     expr)
			    body-acc)))])
    (for-each
     (lambda (clause)
       (cond
	[(zodiac:public-clause? clause)
	 ; For each, make a new binding for setting the public ivar
	 ; Set the orig-name of the lookup binding to the ivar external name
	 (for-each
	  (lambda (ext lookup-binding expr)
	    (let* ([ext-name (zodiac:read-object ext)]
		   [define-binding (zodiac:make-lexical-binding
				    (zodiac:zodiac-origin lookup-binding)
				    (zodiac:zodiac-start lookup-binding)
				    (zodiac:zodiac-finish lookup-binding)
				    (make-empty-box)
				    (symbol-append (zodiac:binding-var lookup-binding) 'def)
				    ext-name)]
		   [define-varref (new-varref define-binding)])
	      (zodiac:set-binding-orig-name! lookup-binding ext-name)
	      (set! plb-acc (cons lookup-binding plb-acc))
	      (set! pdb-acc (cons define-binding pdb-acc))
	      (add-set! define-varref expr)))
	  (zodiac:public-clause-exports clause)
	  (zodiac:public-clause-internals clause)
	  (zodiac:public-clause-exprs clause))]
	[(zodiac:private-clause? clause)
	 (for-each
	  (lambda (binding expr)
	    (set! pri-acc (cons binding pri-acc))
	    (add-set! (new-varref binding) expr))
	  (zodiac:private-clause-internals clause)
	  (zodiac:private-clause-exprs clause))]
	[(or (zodiac:inherit-clause? clause)
	     (zodiac:rename-clause? clause))
	 (let-values ([(get-internals get-imports set-acc!)
		       (if (zodiac:inherit-clause? clause)
			   (values zodiac:inherit-clause-internals
				   zodiac:inherit-clause-imports
				   (lambda (v) (set! inh-acc (cons v inh-acc))))
			   (values zodiac:rename-clause-internals
				   zodiac:rename-clause-imports
				   (lambda (v) (set! ren-acc (cons v ren-acc)))))])
	    (for-each
	     (lambda (binding import)
	       ; Set the binding's original name to import
	       (zodiac:set-binding-orig-name! binding (zodiac:read-object import))
	       (set-acc! binding))
	     (get-internals clause)
	     (get-imports clause)))]
	[(zodiac:sequence-clause? clause)
	 (set! body-acc
	       (append
		(reverse (zodiac:sequence-clause-exprs clause))
		body-acc))]
	[else (compiler:internal-error #f "unknown class clause ~a" clause)]))
     (zodiac:class*/names-form-inst-clauses ast))

    (values plb-acc
	    pdb-acc
	    pri-acc
	    inh-acc
	    ren-acc
	    (zodiac:make-begin-form
	     (zodiac:zodiac-origin ast)
	     (zodiac:zodiac-start ast)
	     (zodiac:zodiac-finish ast)
	     (make-empty-box)
	     (if (null? body-acc)
		 (list (zodiac:make-special-constant 'void))
		 (reverse! body-acc))))))

(define (preprocess:adhoc-app-optimization ast prephase-it)
  (let ([fun (zodiac:app-fun ast)])
    (and (zodiac:top-level-varref? fun)
	 (let ([name (zodiac:varref-var fun)]
	       [args (zodiac:app-args ast)])
	   (case name
	     [(#%void) (if (null? args)
			   (prephase-it (zodiac:make-special-constant 'void))
			   #f)]
	     [(#%list) (if (null? args)
			   (prephase-it (zodiac:make-special-constant 'null))
			   #f)]
	     [(#%+ #%-) (when (and (= 2 (length args))
				   (zodiac:quote-form? (cadr args))
				   (= 1 (zodiac:read-object (zodiac:quote-form-expr (cadr args)))))
			  (let ([newname (if (eq? name '#%+) '#%add1 '#%sub1)])
			    (zodiac:set-app-fun! ast
						 (prephase-it
						  (zodiac:make-top-level-varref
						   (zodiac:zodiac-origin fun)
						   (zodiac:zodiac-start fun)
						   (zodiac:zodiac-finish fun)
						   (make-empty-box)
						   newname)))
			    (zodiac:set-app-args! ast
						  (list (car args)))))
			#f] ; always return #f => use the (possibly mutated) ast
	     [else #f])))))

;;----------------------------------------------------------------------------
;; PREPHASE MAIN FUNCTION
;;
;; conses units on to s:unit-list
;;
;; Collect names for procedures, classes, and units

(define curry-prephase!
  (lambda ()
    (letrec ([prephase!
	      (lambda (ast in-unit? need-val? name)
		(when (compiler:option:debug)
		  (zodiac:print-start! debug:port ast)
		  (newline debug:port))
		(cond
		  ;;----------------------------------------------------------
		  ;; CONSTANTS
		  ;;
		  [(zodiac:quote-form? ast) ast]
	  
		  ;;----------------------------------------------------------
		  ;; VARIABLE REFERENCES
		  ;;
		  ;; set up all varrefs with an attribute set
		  ;; note all #%... varrefs as primitives
		  ;; change unit-bound `top-levels' to lexicals
		  ;;
		  [(zodiac:varref? ast)
		   
		   (if (and (zodiac:top-level-varref/bind? ast)
			    (zodiac:lexical-binding? 
			     (unbox (zodiac:top-level-varref/bind-slot ast))))
		       ; This is a unit-`defined' variable; change to lexical
		       (let* ([binding (unbox (zodiac:top-level-varref/bind-slot ast))]
			      [ref (zodiac:make-lexical-varref
				    (zodiac:zodiac-origin ast)
				    (zodiac:zodiac-start ast)
				    (zodiac:zodiac-finish ast)
				    (make-empty-box)
				    (zodiac:binding-var binding)
				    binding)])
			 (mrspidey:copy-annotations! ref ast)
			 (prephase! ref in-unit? need-val? name))
		       
		       (begin
			 (set-annotation! ast (varref:empty-attributes))
			 
			 (when (and (zodiac:top-level-varref? ast)
				    (prephase:primitive-name?! ast in-unit?))
			       (varref:add-attribute! ast varref:primitive))
			 
			 ast))]
		         
		  ;;----------------------------------------------------------
		  ;; LAMBDA EXPRESSIONS
		  ;;
		  [(zodiac:case-lambda-form? ast)
		   (let ([args (zodiac:case-lambda-form-args ast)]
			 [bodies (zodiac:case-lambda-form-bodies ast)])
		     (for-each
		      (lambda (args)
			(for-each (lambda (b) (prephase:init-binding-properties! b #f #f #f))
				  (zodiac:arglist-vars args)))
		      args)
		     (let ([ast (zodiac:make-case-lambda-form
				 (zodiac:zodiac-origin ast)
				 (zodiac:zodiac-start ast)
				 (zodiac:zodiac-finish ast)
				 (zodiac:parsed-back ast)
				 args
				 (begin-map (lambda (e) ((curry-prephase!) e in-unit? #f #f))
					    (lambda (e) ((curry-prephase!) e in-unit? #t #f))
					    bodies))])
		       (set-annotation! ast name)
		       ast))]
		  
		  ;;----------------------------------------------------------
		  ;; LET EXPRESSIONS
		  ;;
		  [(zodiac:let-values-form? ast)
		   (for-each 
		    (lambda (l)
		      (for-each (lambda (b) (prephase:init-binding-properties! b #f #f #f))
				l))
		    (zodiac:let-values-form-vars ast))
		   (zodiac:set-let-values-form-vals!
		    ast
		    (map (lambda (e name) ((curry-prephase!) e in-unit? #t name))
			 (zodiac:let-values-form-vals ast)
			 (zodiac:let-values-form-vars ast)))
		   (zodiac:set-let-values-form-body!
		    ast
		    ((curry-prephase!) (zodiac:let-values-form-body ast) in-unit? need-val? name))
		   ast]
		  
		  ;;-----------------------------------------------------------
		  ;; LETREC EXPRESSIONS
		  ;;
		  [(zodiac:letrec*-values-form? ast)
		   (for-each (lambda (l)
			       (for-each (lambda (b) 
					   (prephase:init-binding-properties! b #f #f #f))
					 l))
			     (zodiac:letrec*-values-form-vars ast))
		   (zodiac:set-letrec*-values-form-vals!
		    ast
		    (map (lambda (e name) ((curry-prephase!) e in-unit? #t name))
			 (zodiac:letrec*-values-form-vals ast)
			 (zodiac:letrec*-values-form-vars ast)))
		   ; this will mark the letrec so it is NOT retraversed by
		   ; a possible future call to a-normalize! (the mutating version)
		   (set-annotation! ast #f)
		   (zodiac:set-letrec*-values-form-body!
		    ast
		    ((curry-prephase!) 
		     (zodiac:letrec*-values-form-body ast)
		     in-unit? 
		     need-val?
		     name))
		   ast]
		  
		  ;;-----------------------------------------------------------
		  ;; IF EXPRESSIONS
		  ;;
		  [(zodiac:if-form? ast)
		   (zodiac:set-if-form-test! 
		    ast
		    ((curry-prephase!) (zodiac:if-form-test ast) in-unit? #t #f))
		   (zodiac:set-if-form-then!
		    ast
		    ((curry-prephase!) (zodiac:if-form-then ast) in-unit? need-val? name))
		   (zodiac:set-if-form-else!
		    ast
		    ((curry-prephase!) (zodiac:if-form-else ast) in-unit? need-val? name))

		   ;; Ad hoc optimization: (if (#%not x) y z) => (if x z y)
		   (let ([test (zodiac:if-form-test ast)])
		     (when (and (zodiac:app? test)
				(zodiac:top-level-varref? (zodiac:app-fun test))
				(eq? '#%not (zodiac:varref-var (zodiac:app-fun test)))
				(= 1 (length (zodiac:app-args test))))
		       (let ([then (zodiac:if-form-then ast)]
			     [else (zodiac:if-form-else ast)])
			 (zodiac:set-if-form-test! ast (car (zodiac:app-args test)))
			 (zodiac:set-if-form-then! ast else)
			 (zodiac:set-if-form-else! ast then))))

		   ast]
      
		  ;;-----------------------------------------------------------
		  ;; BEGIN EXPRESSIONS
		  ;; 
		  ;; flatten, throw away dead values
		  ;;
		  [(zodiac:begin-form? ast)
		   (let ([bodies (zodiac:begin-form-bodies ast)])
		     (begin-map! (lambda (e) ((curry-prephase!) e in-unit? #f #f))
				 (lambda (e) ((curry-prephase!) e in-unit? need-val? name))
				 bodies)
		     (let ([final-bodies
			    (let loop ([bodies bodies])
			      (cond
				; last expr in begin, finished
				[(null? (cdr bodies)) bodies]
				
				; flatten begins
				[(zodiac:begin-form? (car bodies))
				 (loop (append! (zodiac:begin-form-bodies (car bodies))
						(cdr bodies)))]
				
				; flatten begin0s, too
				[(zodiac:begin0-form? (car bodies))
				 (loop (append! (zodiac:begin0-form-bodies (car bodies))
						(cdr bodies)))]
				
				; throw away dead values if possible
				[(prephase:dead-expression? (car bodies))
				 (loop (cdr bodies))]
				
				; otherwise
				[else (cons (car bodies) (loop (cdr bodies)))]))])
		       (if (null? (cdr final-bodies))
			   (car final-bodies)
			   (begin
			     (zodiac:set-begin-form-bodies! ast final-bodies)
			     ast))))]

		  ;;-----------------------------------------------------------
		  ;; BEGIN0 EXPRESSIONS
		  ;; 
		  ;; the 1st place is special -- the rest is just a begin
		  ;; do our begin rewrites, then transform to a general form
		  ;; if necessary
		  ;;
		  ;; if the value isn't going to be used, then the whole thing
		  ;; is a begin
		  ;;
		  [(zodiac:begin0-form? ast)
		   (if (not need-val?)
		       ((curry-prephase!)
			(zodiac:make-begin-form (zodiac:zodiac-origin ast)
						(zodiac:zodiac-start ast)
						(zodiac:zodiac-finish ast)
						(zodiac:parsed-back ast)
						(zodiac:begin0-form-bodies ast))
			in-unit? 
			#f
			#f)

		       (let ([ast
			  (let ([make-begin
				 (lambda (bodies)
				   (zodiac:make-begin-form (zodiac:zodiac-origin ast)
							   (zodiac:zodiac-start ast)
							   (zodiac:zodiac-finish ast)
							   (zodiac:parsed-back ast)
							   bodies))]
				[bodies (zodiac:begin0-form-bodies ast)])
			    
			    ; simplify the first position
			    (set-car! bodies ((curry-prephase!) (car bodies) in-unit? need-val? name))
			    
			    ; then simplify the begin0
			    (cond
			      
			      ; (begin0 M) --> M
			      [(null? (cdr bodies)) (car bodies)]
			      
			      ; (begin0 <push> ...) --> (begin ... <push>))
			      [(prephase:begin0-pushable? (car bodies))
			       ((curry-prephase!) 
				(make-begin (append (cdr bodies) (list (car bodies))))
				in-unit? 
				need-val?
				name)]
			      
			      ; (begin0 M ...) --> (begin0 M (begin ...))
			      [else
			       (set-cdr!
				(zodiac:begin0-form-bodies ast)
				(list ((curry-prephase!) (make-begin (cdr bodies)) in-unit? #f #f)))
			       ast]
			      
			      ))])
			 (if (zodiac:begin0-form? ast)
			     ast ; (prephase:convert-begin0 ast)
			     ast)))]
		     
			 

		  
		  ;;-----------------------------------------------------------
		  ;; SET! EXPRESSIONS
		  ;;
		  ;; Thank goodness -- no set!-values
		  ;;
		  [(zodiac:set!-form? ast)
		   (zodiac:set-set!-form-var! ast
					      ((curry-prephase!)
					       (zodiac:set!-form-var ast)
					       in-unit? 
					       #t
					       #f))
		   (let ([target (zodiac:set!-form-var ast)])
		     (when (and (zodiac:bound-varref? target)
				(not (prephase:set!-is-unit-definition? ast)))
		       (prephase:set-mutable! 
			(zodiac:bound-varref-binding target) #t))
		     
		     (zodiac:set-set!-form-val! ast 
						((curry-prephase!) 
						 (zodiac:set!-form-val ast)
						 in-unit? 
						 #t
						 (zodiac:set!-form-var ast)))
		     ast)]		  
		  
		  ;;-----------------------------------------------------------
		  ;; DEFINE EXPRESSIONS
		  ;;
		  ;;
		  [(zodiac:define-values-form? ast)
		   (let ([symbols 
			  (map zodiac:varref-var 
			       (zodiac:define-values-form-vars ast))])
		     (for-each
		      (lambda (symbol)
			
			(when (built-in-name symbol)
			  (compiler:warning
			   ast
			   (format 
			    "redefinition of primitive '~a'~a"
			    symbol
			    (if (compiler:option:assume-primitives)
				", will have no effect"
				"")))))
		      symbols))
		   (if (null? (zodiac:define-values-form-vars ast))
		       (zodiac:make-let-values-form 
			(zodiac:zodiac-origin ast)
			(zodiac:zodiac-start ast)
			(zodiac:zodiac-finish ast)
			(zodiac:parsed-back ast)
			(list null)
			(list ((curry-prephase!) (zodiac:define-values-form-val ast) in-unit? #t #f))
			(zodiac:make-special-constant 'void))
		       (begin
			 (zodiac:set-define-values-form-vars!
			  ast
			  (map (lambda (e) ((curry-prephase!) e in-unit? #t #f))
			       (zodiac:define-values-form-vars ast)))
			 (zodiac:set-define-values-form-val!
			  ast
			  ((curry-prephase!) (zodiac:define-values-form-val ast) in-unit? #t (zodiac:define-values-form-vars ast)))
			 ast))]
		    		  
		  ;;-----------------------------------------------------------
		  ;; APPLICATIONS
		  ;;
		  ;; check for unsupported syntactic forms that end up
		  ;; looking like applications
		  ;;
		  ;; We'll hack in a rewrite here that turns
		  ;; ((lambda (x*) M) y*) -> (let ([x y]*) M)
		  ;;
		  [(zodiac:app? ast)

		   (let ([process-normally
			  (lambda ()
			    (zodiac:set-app-fun!
			     ast
			     ((curry-prephase!) (zodiac:app-fun ast) in-unit? #t #f))
			    (let ([adhoc (preprocess:adhoc-app-optimization 
					  ast
					  (lambda (x)
					    ((curry-prephase!) x in-unit? #t #f)))])
			      (if adhoc
				  ((curry-prephase!) adhoc in-unit? need-val? name)
				  (begin
				    (zodiac:set-app-args!
				     ast
				     (map (lambda (e) ((curry-prephase!) e in-unit? #t #f))
					  (zodiac:app-args ast)))
				    ast))))])
		   
		     (if (and (zodiac:case-lambda-form? (zodiac:app-fun ast))
			      (= 1 (length (zodiac:case-lambda-form-args 
					    (zodiac:app-fun ast))))
			      (zodiac:list-arglist? 
			       (car (zodiac:case-lambda-form-args
				     (zodiac:app-fun ast)))))
			 
			 ;; optimize to let
			 (let* ([L (zodiac:app-fun ast)]
				[args (zodiac:app-args ast)]
				[ids (zodiac:arglist-vars 
				      (car (zodiac:case-lambda-form-args L)))]
				[body (car (zodiac:case-lambda-form-bodies L))]
				[ok? (= (length ids) (length args))])
			   (unless ok?
			     ((if (compiler:option:stupid) compiler:warning compiler:error)
			      ast 
			      "wrong number of arguments to literal function"))
			   (if (not ok?)
			       (process-normally)
			       ((curry-prephase!)
				(zodiac:make-let-values-form
				 (zodiac:zodiac-origin ast)
				 (zodiac:zodiac-start ast)
				 (zodiac:zodiac-finish ast)
				 (zodiac:parsed-back ast)
				 (map list ids)
				 args
				 body)
				in-unit? 
				need-val?
				name)))
		       
			 ;; don't optimize
			 (process-normally)))]
		  
		  ;;-----------------------------------------------------------
		  ;; STRUCTS
		  ;;
		  [(zodiac:struct-form? ast)
		   (let ([super (zodiac:struct-form-super ast)])
		     (when super
		       (zodiac:set-struct-form-super!
			ast
			((curry-prephase!) super in-unit? #t #f)))
		     ast)]
		  
		  ;;-----------------------------------------------------------
		  ;; UNITS
		  ;;
		  ;; defines in the top-level of a unit are identified by zodiac
		  ;; with top-level-varref structures. But they're really lexical
		  ;; entities. We'll change all the top-level-varref's to
		  ;; lexical-bindings and lecial-varrefs. The lexical bindings
		  ;; will be called mutable, but the value may still be known...
		  ;; (see analyze.ss)
		  ;;
		  [(zodiac:unit-form? ast)
		   (let-values ([(clauses defined-bindings)
				 (prephase:unit-vars->lexical-vars ast)])

		     (let* ([imported-bindings (zodiac:unit-form-imports ast)]
			    [exported-bindings
			     (map
			      (lambda (e)
				; The top-level-varrefs are have the new lexcial-binding in the slot box
				(unbox (zodiac:top-level-varref/bind-slot (car e))))
			      (zodiac:unit-form-exports ast))]
			    [anchored-bindings (append imported-bindings exported-bindings)]
			    [_ (for-each
				(lambda (b)
				  (prephase:init-binding-properties! b #f #t #f))
				(append imported-bindings defined-bindings))]
			    [make-anchors
			     (lambda (anchored-bindings)
			       (map
				(lambda (binding)
				  (let ([anchor (zodiac:make-lexical-binding
						 (zodiac:zodiac-origin binding)
						 (zodiac:zodiac-start binding)
						 (zodiac:zodiac-finish binding)
						 (make-empty-box)
						 (symbol-append (zodiac:binding-var binding)
								'anchor)
						 (symbol-append (zodiac:binding-orig-name binding)
								'-anchor))])
				    (prephase:set-binding-anchor! binding anchor)
				    (prephase:init-binding-properties! anchor #f #f #f)
				    anchor))
				anchored-bindings))]
			    [import-anchors (make-anchors imported-bindings)]
			    [export-anchors (make-anchors exported-bindings)])
		       
		       ; prephase the clauses, change top-level-varrefs into
		       ; lexical varrefs, and collapse the body into a begin
		       (zodiac:set-unit-form-clauses!
			ast
			(list
			 ((curry-prephase!) 
			  (zodiac:make-begin-form
			   (zodiac:zodiac-origin ast)
			   (zodiac:zodiac-start ast)
			   (zodiac:zodiac-finish ast)
			   (make-empty-box)
			   (if (null? clauses)
			       (list (zodiac:make-special-constant 'void))
			       clauses))
			  #t #t #f)))
		       ; annotate this dude
		       (set-annotation! ast (make-unit-code
					     empty-set empty-set empty-set empty-set empty-set
					     #f #f #f #f
					     0 'possible name
					     defined-bindings
					     exported-bindings
					     import-anchors
					     export-anchors
					     #f)))

		     ast)]

		  ;;-----------------------------------------------------------
		  ;; COMPOUND UNIT
		  ;;
		  [(zodiac:compound-unit-form? ast)
		   (for-each (lambda (link)
			       (set-car!
				(cdr link)
				((curry-prephase!) (cadr link) in-unit? #t #f)))
			     (zodiac:compound-unit-form-links ast))
		     
		   ast]

		  ;;-----------------------------------------------------------
		  ;; INVOKE
		  ;;
		  ;; If we're in a unit, then replace links to #% globals with
		  ;; let-bound locals
		  ;;
		  [(zodiac:invoke-form? ast)
		   (let ([link-vars (zodiac:invoke-form-variables ast)])
		     (if (and in-unit? (ormap zodiac:top-level-varref? link-vars))
			 
			 ; Generate locals for linking and prephase again
			 ((curry-prephase!)
			  (let loop ([l link-vars][l-acc null])
			    (cond
			     [(null? l)
			      (zodiac:set-invoke-form-variables! 
			       ast
			       (reverse! l-acc))
			      ast]
			     [(zodiac:top-level-varref? (car l))
			      (let* ([b (zodiac:make-lexical-binding
					 (zodiac:zodiac-origin ast)
					 (zodiac:zodiac-start ast)
					 (zodiac:zodiac-finish ast)
					 (make-empty-box)
					 (gensym 'invokeLinkGlobal)
					 'invokeLinkGlobal)]
				     [v (zodiac:binding->lexical-varref b)])
				(zodiac:make-let-values-form
				 (zodiac:zodiac-origin (car l))
				 (zodiac:zodiac-start (car l))
				 (zodiac:zodiac-finish (car l))
				 (make-empty-box)
				 (list (list b))
				 (list (car l))
				 (loop (cdr l) (cons v l-acc))))]
			     [else
			      (loop (cdr l) (cons (car l) l-acc))]))
			  #t need-val? name)
			 
			 ; Normal handling
			 (begin
			   (zodiac:set-invoke-form-unit! 
			    ast 
			    ((curry-prephase!) (zodiac:invoke-form-unit ast) in-unit? #t #f))
			   (zodiac:set-invoke-form-variables! 
			    ast
			    (map (lambda (v) 
				   (when (zodiac:bound-varref? v)
					 (prephase:set-unit-i/e?! (zodiac:bound-varref-binding v) #t))
				   ((curry-prephase!) v in-unit? #t #f))
				 (zodiac:invoke-form-variables ast)))
		   
			   ast)))]
		
		  ;;-----------------------------------------------------------
		  ;; CLASS
		  ;;
		  ;; Change public definitions to set!s in the usual way,
		  ;; distinguishing between bindings for getting the values
		  ;; of public variables and bindings for setting the values.
		  ;; The result is a class with only one sequence clause, and
		  ;; that clause contains only a begin expression.
		  ;; 
		  [(zodiac:class*/names-form? ast)

		   ; Init bindings for simple lexicals
		   (prephase:init-binding-properties!
		    (zodiac:class*/names-form-this ast)
		    #f #f #f)
		   (prephase:init-binding-properties!
		    (zodiac:class*/names-form-super-init ast)
		    #f #f #f)
		   (for-each (lambda (b) 
			       (let ([b (if (pair? b) (car b) b)])
				 (prephase:init-binding-properties! b #f #f #f)))
			     (zodiac:paroptarglist-vars
			      (zodiac:class*/names-form-init-vars ast)))

		   ; Prephase superclass and interfaces expressions
		   (zodiac:set-class*/names-form-super-expr!
		    ast
		    ((curry-prephase!) (zodiac:class*/names-form-super-expr ast) 
				       in-unit? #t #f))
		   (zodiac:set-class*/names-form-interfaces!
		    ast
		    (map (lambda (i) ((curry-prephase!) i in-unit? #t #f))
			 (zodiac:class*/names-form-interfaces ast)))

		   ; Prephase the initialization defaults
		   (let* ([arglist (zodiac:class*/names-form-init-vars ast)]
			  [args (zodiac:paroptarglist-vars arglist)])
		     (let loop ([args args])
		       (unless (null? args)
			  (when (pair? (car args))
				(set-cdr! (car args)
					  ((curry-prephase!) (cdar args) in-unit? #t (caar args))))
			  (loop (cdr args)))))
		   
		   ; Transform the clauses
		   (let-values ([(public-lookup-bindings
				  public-define-bindings
				  private-bindings
				  inherit-bindings
				  rename-bindings
				  body) 
				 (prephase:class-clauses->begin ast)])

		      ; Init public bindings
		      (for-each (lambda (b)
				  (prephase:init-binding-properties! b #f #f #t))
				(append public-lookup-bindings
					public-define-bindings
					inherit-bindings
					rename-bindings))
		      ; Init privates
		      (for-each (lambda (b)
				  (prephase:init-binding-properties! b #f #f #f))
				(append private-bindings))

		      ; Prephase the body:
		      (let ([body ((curry-prephase!) body in-unit? #f #f)])
			(zodiac:set-class*/names-form-inst-clauses!
			 ast
			 (list (zodiac:make-sequence-clause
				(list body)))))

		      (set-annotation! ast
				       (make-class-code
					empty-set empty-set empty-set empty-set empty-set
					#f #f #f #f
					0 'possible name
					public-lookup-bindings
					public-define-bindings
					private-bindings
					inherit-bindings
					rename-bindings
					#f)))

		   ast]

		  ;;-----------------------------------------------------------
		  ;; INTERFACE
		  ;;
		  [(zodiac:interface-form? ast)

		   (set-annotation! ast (make-interface-info #f name))

		   (zodiac:set-interface-form-super-exprs!
		    ast
		    (map (lambda (expr) ((curry-prephase!) expr in-unit? #t #f))
			 (zodiac:interface-form-super-exprs ast)))

		   ast]

		  ;;-----------------------------------------------------------
		  ;; MrSpidey forms
		  ;;  MrSpidye is done, so we can just get rid of them
		  [(zodiac::-form? ast)
		   (zodiac::-form-exp ast)]
		  [(zodiac:poly-form? ast)
		   (zodiac:poly-form-exp ast)]

		  ;;-----------------------------------------------------------
		  ;; Unsupported forms
		  ;;
		  [else (compiler:fatal-error 
			 ast 
			 (format "unsupported syntactic form ~a" ast))
			ast]))])
				 
						      
      prephase!)))

(define prephase! (curry-prephase!))

)
