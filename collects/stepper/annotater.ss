(unit/sig stepper:annotate^
  (import [z : zodiac:system^]
	  mzlib:function^
	  [e : stepper:error^]
	  stepper:shared^
	  stepper:reconstruct)
  
    ; ANNOTATE SOURCE CODE
  
  ; gensyms for annotation:
  
  ; the mutator-gensym is used in building the mutators that go into certain marks.
  ; (define mutator-gensym (gensym "mutator-"))
  
  ; the `closure-temp' symbol is used for the let which wraps created closures, so
  ; that we can stuff them into the hash table.
  
  (define closure-temp (gensym "closure-temp-"))
  
  ; the `if-temp' symbol is used for the temp which gets the value of the test
  ; expression.  It exists so that we can do a runtime check to insure that it's 
  ; a boolean (required in some language levels).
  
  (define if-temp (gensym "if-temp-"))
   
  ; make-debug-info takes a list of variables and an expression and
  ; creates a thunk closed over the expression and (if bindings-needed is true) 
  ; the following information for each variable in kept-vars:
  ; 1) the name of the variable (could actually be inferred)
  ; 2) the value of the variable
  ; 3) a mutator for the variable, if it appears in mutated-vars.
  ; (The reason for the third of these is actually that it can be used
  ;  in the stepper to determine which bindings refer to the same location,
  ;  as per Matthew's suggestion.)
  ; 
  ; as an optimization:
  ; note that the mutators are needed only for the bindings which appear in
  ; closures; no location ambiguity can occur in the 'currently-live' bindings,
  ; since at most one location can exist for any given stack binding.  That is,
  ; using the source, I can tell whether variables referenced directly in the
  ; continuation chain refer to the same location.
  
  ; okay, things have changed a bit.  For this iteration, I'm simply not going to 
  ; store mutators.  later, I'll add them in.
 
  #| (define (make-debug-info vars bindings-needed source)
    (let* ([kept-vars (if bindings-needed vars null)]
	   [var-clauses (map (lambda (x) 
			       (let ([var (varref-var x)])
				 `(cons (#%quote ,var)
					(cons ,var
					      ,(if (varref-mutated? x)
						   `(lambda (,mutator-gensym)
						      (set! ,var ,mutator-gensym))
						   `null)))))
			     kept-vars)])
      `(#%lambda () (list ,source ,@var-clauses)))) |#
  
  (define (make-debug-info vars bindings-needed source label)
    (let* ([kept-vars (if bindings-needed vars null)]
	   [var-clauses (map (lambda (x) 
			       (let ([var (varref-var x)])
				 `(cons ,var
					(cons ,x
					      null))))
			     kept-vars)])
      `(#%lambda () (list ,(z:offset (z:start source)) ,label ,@var-clauses))))
    
  ; var-set-union takes some lists of varrefs where no element appears twice in one list, and 
  ; forms a new list which is the union of the sets.  the elements are 
  ; compared using the first element of the varref
  
  (define var-set-union
    (lambda args
      (foldl (lambda (a b) 
	       (append a (remove* a b (lambda (x y) 
					(= (varref-var x)
					   (varref-var y))))))
	     null
	     args)))
  
  ; set-union test: (relies on current implementation of set-union) (and is broken now)
  
  #| (andmap (lambda (x) (apply equal? x))
	  `((,(set-union) ,null)
	    (,(set-union '(3 2 foo)) (3 2 foo))
	    (,(set-union '(3 9 12 97 4) '(2 1 98 3 9) '(2 19 97 4))
	     (3 9 12 97 4 2 1 98 19))))
  |#
    
  #| .
     somehow, we need to translate zodiac structures back into scheme structures so that 
     we can hand them off to mzscheme.  By rights, that's an aries-like job.  So, there
     are two ways we could do this.
     
     First, we could subsume the needed forms into zodiac, so that this
     module could simply deliver the annotated code to aries, BUT we really don't want aries
     to further annotate it with additional source-position information. At least, that 
     seems a bit silly to me.
     
     The other alternative, which I'm tentatively pursuing, is to do an aries-like translation
     right here, but it means I have to COPY CODE from aries.  In particular, I need this
     arglist->ilist function. Ick.
     |#
  
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
		  (e:internal-error id "Given in check-for-keyword")])])
	  (when (and (keyword-name? real-id)
		     (or disallow-procedures?
			 (let ([gdv (global-defined-value real-id)])
			   (or (syntax? gdv)
			       (macro? gdv)))))
	    (e:static-error id "Invalid use of keyword ~s"
			    real-id))))))
  
  (define check-for-keyword (check-for-keyword/both #t))
  (define check-for-keyword/proc (check-for-keyword/both #f))
  
  

  ; Here's more code copied directly from aries.  This is getting worse and worse.
  
  (define the-undefined-value (letrec ((x x)) x))
  
  (define-struct (undefined struct:exn) (id))
  (define signal-undefined (make-parameter #t))
  (define undefined-error-format
    "Variable ~s referenced before definition or initialization")
  
  (define-struct (not-boolean struct:exn) (val))
  (define signal-not-boolean (make-parameter #f))
  (define not-boolean-error-format "Condition value is neither #t nor #f: ~e")

  ; and yet more copied code.  Ugh.
  
  (define (is-unit-bound? varref)
    (and (z:top-level-varref/bind/unit? varref)
	 (z:top-level-varref/bind/unit-unit? varref)))
  
  
  (define-values (never-undefined? mark-never-undefined)
    (let-values ([(getter setter) (z:register-client 'aries:never-undefined (lambda () #f))])
      (values
       (lambda (parsed) (getter (z:parsed-back parsed)))
       (lambda (parsed) (setter (z:parsed-back parsed) #t)))))

  ; no-label : an instance of the no-label structure
  
  (define no-label (make-no-label))

  ; How do we know which bindings we need?  For every lambda body, there is a
  ; `tail-spine' of expressions which is the smallest set including:
  ; a) the body itself
  ; b) an expression in tail position relative to a member of the tail-spine.
  ;
  ; I'm using `tail position' in a slightly non-standard way here.  Under my
  ; definition, A is in tail position relative to B if 
  ; a) A is contained in B
  ; b) if A is evaluated, the result of evaluating A will be the result of
  ;    evaluating B.
  ;
  ; So, if I've defined this correctly, note that an if expression has two tail
  ; positions, whereas an application has none.

  ; annotate takes an expression to annotate and a `break' function which will be inserted in
  ; the code.  It returns an annotated expression, ready for evaluation.
  
  (define (annotate text break)
    (local
	
	; exprs : the result of doing zodiac's read on the text
	
	((define exprs
	   (let ([reader (z:read text)])
	     (let read-loop ([new-expr (reader)])
	       (if (z:eof? new-expr)
		   ()
		   (cons new-expr (read-loop (reader)))))))
			
	 ; debug-key: this key will be used to register the source expr with reconstructr.ss
	 ; and as a key for the continuation marks.
	 
	 (define debug-key (gensym "debug-key-"))
	 
	 ; expr-source-offset : take a parsed expression and find its offset in the source
	 ; (z:zodiac -> num)
	 
	 (define (expr-source-offset expr)
	   (z:location-offset (z:zodiac-start expr)))
	 
	 ; comes-from-cond : determines whether an expression is expanded from a cond in the source
	 
	 (define (comes-from-cond? expr)
	   (let ([read (find-source-expr debug-key (expr-source-offset expr))])
	     (and (z:sequence? read)
		  (z:list? read)
		  (let ([first (car (z:read-object read))])
		    (and (z:scalar? first)
			 (z:symbol? first)
			 (eq? (z:symbol-orig-name first) 'cond))))))
	 
	 ; locate-cond-clause: take a cond expression's start location
	 ; and a test expression's start location and figure out 
	 ; which clause of the cond the test comes from.
	 
	 (define (find-cond-clause cond-expr test-expr)
	   (let* ([target-offset (expr-source-offset test-expr)]
		  [cond-source (find-source-expr debug-key (expr-source-offset cond-expr))]
		  [cond-clauses (cdr (z:read-object cond-source))]
		  [test-exprs (map car (z:read-object cond-clauses))])
	     (let loop ([test-exprs test-exprs] [index 0])
	       (if (null? test-exprs)
		   (e:static-error test-location "test expression not found in cond expression")
		   (if (= target-offset (z:location-offset (z:zodiac-start (car test-exprs))))
		       index
		       (loop (cdr test-exprs) (+ index 1)))))))
	 
	 ; wrap creates the w-c-m expression.
	 
	 (define (wrap debug-info expr)
	   (let ([with-break `(#%begin (,break) ,expr)])
	     `(#%with-continuation-mark (#%quote ,debug-key) ,debug-info ,with-break)))
	 
	 ; annotate/inner takes an expression to annotate and a boolean
	 ; indicating whether this expression lies on the evaluation spine.  It returns two things;
	 ; an annotated expression, and a list of varref's.	 
	 
	 ; annotate/inner: (z:zodiac bool -> sexp (listof varref))
	 
	 (define (annotate/inner expr on-spine?)
	   
	   ; translate-varref: (bool bool -> sexp (listof varref))
	   
	   (let ([translate-varref
		  (lambda (maybe-undef? top-level?)
		    (let* ([v (z:varref-var expr)]
			   [real-v (if (z:top-level-varref? expr)
				       v
				       (z:binding-orig-name
					(z:bound-varref-binding expr)))]
			   [free-vars (list (make-varref v top-level?))]
			   [debug-info (make-debug-info free-vars on-spine? expr no-label)]
			   [annotated (if (and maybe-undef? (signal-undefined))
					  `(#%if (#%eq? ,v ,the-undefined-value)
					    (#%raise (,make-undefined
						      ,(format undefined-error-format real-v)
						      ((#%debug-info-handler))
						      (#%quote ,v)))
					    ,v)
					  v)])
		      (values (wrap debug-info annotated) free-vars)))])
	     
	     
	     (cond
	       
	       ; the variable forms 
	       
	       [(z:bound-varref? expr)
		(translate-varref 
		 (not (never-undefined? (z:bound-varref-binding expr)))
		 #f)]
	       
	       [(z:top-level-varref? expr)
		(if (is-unit-bound? expr)
		    (translate-varref #t #f)
		    (begin
		      (check-for-keyword/proc expr)
		      (translate-varref #f #t)))]
	       
	       [(z:app? expr)
		(let+
		 ([val sub-exprs (cons (z:app-fun expr) (z:app-args expr))]
		  [val arg-sym-list (build-list (length sub-exprs) get-arg-symbol)]
		  [val let-clauses (map (lambda (sym) `(,sym (#%quote ,*unevaluated*))) arg-sym-list)]
		  [val pile-of-values
		       (map (lambda (expr bound) 
			      (let-values ([(annotated free) (annotate/inner expr #f)])
				(list annotated free)))
			    sub-exprs)]
		  [val annotated-sub-exprs (map car pile-of-values)]
		  [val free-vars (apply var-set-union (map cadr pile-of-values))]
		  [val set!-list (map (lambda (arg-symbol annotated-sub-expr)
					`(#%set! ,arg-symbol ,annotated-sub-expr))
				      arg-sym-list annotated-sub-exprs)]
		  [val app-debug-info (make-debug-info arg-sym-list on-spine? expr no-label)]
		  [val final-app (wrap app-debug-info arg-sym-list)]
		  [val debug-info (make-debug-info (var-set-union arg-sym-list free-vars) on-spine? expr no-label)]
		  [val let-body (wrap debug-info `(#%begin ,@set!-list ,final-app))])
		 (values `(#%let ,let-clauses ,let-body) free-vars))]
	       
	       [(z:struct-form? expr)
		(let ([super-expr (z:struct-form-super expr)]
		      [raw-type (read->raw (z:struct-form-type expr))]
		      [raw-fields (map read->raw (z:struct-form-fields expr))])
		  (if super-expr
		      (let+ ([val (values annotated-super-expr free-vars-super-expr) 
				  (annotate/inner super-expr on-spine?)]
			     [val annotated
				  `(#%struct 
				    ,(list raw-type annotated-super-expr)
				    ,raw-fields)])
			    (values annotated free-vars-super-expr))
		      (values `(#%struct ,raw-type ,raw-fields))))]
	       
	       [(z:if-form? expr) 
		(let+
		 ([val (values annotated-test free-vars-test) 
		       (annotate/inner (z:if-form-test expr) #f)]
		  [val (values annotated-then free-vars-then) 
		       (annotate/inner (z:if-form-then expr) on-spine?)]
		  [val (values annotated-else free-vars-else) 
		       (annotate/inner (z:if-form-else expr) on-spine?)]
		  ; in beginner-mode, we must insert the boolean-test
		  [val annotated `(#%let (,if-temp ,annotated-test)
				   (#%if (#%boolean? ,if-temp)
				    (#%if ,if-temp
				     ,annotated-then
				     ,annotated-else)
				    (#%raise (,make-not-boolean
					      (#%format ,not-boolean-error-format
					       ,if-temp)
					      ((#%debug-info-handler))
					      ,if-temp))))]
		  [val free-vars (var-set-union free-vars-test free-vars-then free-vars-else)]
		  [val label (if (comes-from-cond? expr)
				 (make-cond-label (find-cond-clause expr (z:if-form-test expr)))
				 #f)]
		  [val debug-info (make-debug-info free-vars on-spine? expr label)])
		 (values (wrap debug-info annotated) free-vars))]
	       
	       [(z:quote-form? expr)
		(values (wrap (make-debug-info null on-spine? expr no-label) 
			      `(#%quote ,(read->raw (z:quote-form-expr expr))))
			null)]
	       
	       ; there is no begin, begin0, or let in beginner. but can they be generated? 
	       ; for instance by macros? Maybe.
	       
	       [(z:define-values-form? expr)
		(let+ ([val vars (z:define-values-form-vars expr)]
		       [val _ (map check-for-keyword vars)]
		       [val var-names (map z:varref-var vars)]
		       [val (values annotated-val free-vars-val)
			    (annotate/inner (z:define-values-form-val expr) on-spine?)]
		       [val free-vars (remq* var-names free-vars-val)]
		       [val annotated `(#%define-values ,var-names ,annotated-val)])
		      (values annotated free-vars))]
	       
	       ; there is no set! in beginner level
	       
	       [(z:case-lambda-form? expr)
		(let* ([annotate-case
			(lambda (arglist body)
			  (let ([var-list (z:arglist-vars arglist)])
			    (let-values ([(annotated free-vars)
					  (annotate/inner body #t)])
			      (let ([new-free-vars (remq* var-list free-vars)]
				    [new-annotated (list (arglist->ilist arglist) annotated)])
				(list new-annotated new-free-vars)))))]
		       [pile-of-results (map annotate-case 
					     (z:case-lambda-form-args expr)
					     (z:case-lambda-form-bodies expr))]
		       [annotated-bodies (map car pile-of-results)]
		       [annotated-case-lambda (list '#%case-lambda annotated-bodies)] 
		       [new-free-vars (apply var-set-union (map cadr pile-of-results))]
		       [debug-info (make-debug-info new-free-vars null on-spine? expr no-label)]
		       [closure-info (make-debug-info new-free-vars #t expr no-label)]
		       [hash-wrapped `(#%let ([,closure-temp ,annotated-case-lambda])
				       ; that closure-table-put! thing needs to be protected
				       (,closure-table-put! ,(closure-key-maker closure-temp) ,closure-info)
				       ,closure-temp)])
		  (values (wrap debug-info hash-wrapped)
			  new-free-vars))]
	       
	       ; there's no with-continuation-mark in beginner level.
	       
	       ; there are _definitely_ no units or classes
	       
	       [else
		(print-struct #t)
		(e:internal-error
		 expr
		 (format "stepper:annotate/inner: unknown object to annotate, ~a~n" expr))]))))
      
      ; body of local
      
      (register debug-key exprs)
      (map (lambda (expr) (annotate/inner expr #t)) exprs)))
	 
  
