; stepper.ss 

(require-library "letplsrc.ss")

(unit/sig stepper^
  (import [e : zodiac:interface^]
	  mzlib:function^
	  [z : zodiac:system^])
  
  (define (stepper-start string)
    (z:read (open-input-string string)))
  
  (define (stepper-step)
    null)
  
  (define (stepper-stop)
    null)
  
  ; HASH SOURCE LOCATIONS: 
  
  (define-values (source-table-build source-table-lookup)
    (let ([source-table #f])
      (values
       (lambda (read-exp)
	 (set! source-table (make-hash-table-weak))
	 (let build-table ((exp read-exp))
	   (let ([start (z:zodiac-start read-exp)])
	     (hash-table-put! source-table start read-exp)
	     (cond
	       ((z:scalar? read-exp)
		null)
	       ((z:sequence? read-exp)
		(let ([object (z:read-object read-exp)])
		  (cond
		    ((z:list? read-exp)
		     (for-each build-table object))
		    ((z:vector? read-exp)
		     (for-each build-table (vector->list object)))
		    ((z:improper-list? read-exp)
		     (for-each build-table object))
		    (else (e:static-error "unknown sequence type in zodiac:read expression")))))
	       (else (e:static-error "unknown zodiac:read type in expression"))))))
       (lambda (start-pos)
	 (hash-table-get source-table start-pos)))))
  
  
  ; ANNOTATE SOURCE CODE
  
  ; gensyms for annotation:
  
  ; the debug-key is the key attached to our marks
  (define debug-key (gensym "debug-key-"))
  
  ; the mutator-gensym is used in building the mutators that go into certain marks.
  (define mutator-gensym (gensym "mutator-"))
  
  ; *unevaluated* is the value assigned to temps before they are evaluated.
  (define *unevaluated* (gensym "unevaluated-"))
  
  ; the `closure-temp' symbol is used for the let which wraps created closures, so
  ; that we can stuff them into the hash table.
  (define closure-temp (gensym "closure-temp-"))
  
  #| .
     the `if-temp' symbol is used for the temp which gets the value of the test
     expression.  It exists so that we can do a runtime check to insure that it's 
     a boolean (required in some language levels).
     |#
  (define if-temp (gensym "if-temp-"))
   
  
  ; get-arg-symbol maintains a list of gensyms associated with the non-negative
  ; integers.  These symbols are used in the elaboration of applications; the nth
  ; in the application is evaluated and stored in a variable whose name is the nth
  ; gensym supplied by get-arg-symbol.
  
  ; I'm just going to implement this with a simple assq list. if this isn't good
  ; enough, it can always be improved later.
  
  (define get-arg-symbol
    (let ([assoc-list null])
      (lambda (arg-num)
	(let ([entry (assq arg-num assoc-list)])
	  (if entry
	      (cadr entry)
	      (begin
		(let ([new-sym (gensym (string-append "arg" (number->string arg-num) "-"))])
		  (set! assoc-list `((,arg-num ,new-sym) ,@assoc-list))
		  new-sym)))))))
  
  ; test cases: (returns #t on success)
  ;(let ([arg3 (get-arg-symbol 3)]
  ;      [arg2 (get-arg-symbol 2)]
  ;      [arg1 (get-arg-symbol 1)]
  ;      [arg2p (get-arg-symbol 2)])
  ;  (and (not (eq? arg3 arg2))
  ;       (not (eq? arg3 arg1))
  ;       (not (eq? arg3 arg2p))
  ;       (not (eq? arg2 arg1))
  ;       (eq? arg2 arg2p)
  ;       (not (eq? arg1 arg2p))))
  
  
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
  #| .
     okay, hang on just a second here. why do we need mutators here? Why not just
     test with eq? to see if two things are shared? I'm taking the mutators out,
     and I'm officially confused. jbc, 3/99
 
     (define (make-debug-info vars mutated-vars bindings-needed source)
       (let* ([kept-vars (if bindings-needed vars null)]
	      [var-clauses (map (lambda (x) 
				  `(cons (#%quote ,x)
					 (cons ,x
					       ,(if (memq x mutated-vars)
						    `(lambda (,mutator-gensym)
						       (set! ,x ,mutator-gensym))
						    `null))))
				kept-vars)])
	 `(#%lambda () (list ,source ,@var-clauses))))
     
     |#
  
  (define (make-debug-info vars bindings-needed source)
    (let* ([kept-vars (if bindings-needed vars null)]
	   [var-clauses (map (lambda (x) 
			       `(cons (#%quote ,x)
				      (cons ,x null)))
			     kept-vars)])
      `(#%lambda () (list ,source ,@var-clauses))))
  
  ; wrap creates the w-c-m expression.
  
  (define (wrap debug-info expr)
    (let ([with-break `(#%begin (,break) ,expr)])
      `(#%with-continuation-mark (#%quote ,debug-key) ,debug-info ,with-break)))
  
  ; set-union takes some lists where no element appears twice in one list, and 
  ; forms a new list which is the union of the sets.  the elements are 
  ; compared using eq?
  
  (define set-union
    (lambda args
      (foldl (lambda (a b) (append a (remq* a b)))
	     null
	     args)))
  
  ; set-union test: (relies on current implementation of set-union
  
  (andmap (lambda (x) (apply equal? x))
	  `((,(set-union) ,null)
	    (,(set-union '(3 2 foo)) (3 2 foo))
	    (,(set-union '(3 9 12 97 4) '(2 1 98 3 9) '(2 19 97 4))
	     (3 9 12 97 4 2 1 98 19))))
  
  #| closure-key-maker:
     closure-key-maker is a piece of code which, when executed at runtime, will map a
     closure to a key that we can hash on.
     In our case, (conservative GC), the value of the closure itself serves this purpose
     admirably, and can also be used with a weak-key hash table to protect the
     tail-recursive properties of the language.
  
  (define (closure-key-maker closure-name) closure-name)
  
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
  
  ; okay, here's _still_more_ copied code, and now I'm inserting it first to preserve
  ; its ordering in aries.
  
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
		  (z:interface:internal-error id
					      "Given in check-for-keyword")])])
	  (when (and (keyword-name? real-id)
		     (or disallow-procedures?
			 (let ([gdv (global-defined-value real-id)])
			   (or (syntax? gdv)
			       (macro? gdv)))))
	    (z:interface:static-error id "Invalid use of keyword ~s"
				      real-id))))))
  
  (define check-for-keyword (check-for-keyword/both #t))
  (define check-for-keyword/proc (check-for-keyword/both #f))
  
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

  
  ; annotate:
  
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
  
  ; annotate takes an expression to annotate, the list of bound variables, and a boolean
  ; indicating whether this expression lies on the evaluation spine.  It returns two things;
  ; an annotated expression, and a list of the bound variables which occur free.
  
  (define (annotate expr bound-vars on-spine)

    (let ([translate-varref
	   (lambda (maybe-undef?)
	     (let* ([v (z:varref-var expr)]
		    [real-v (if (z:top-level-varref? expr)
				v
				(z:binding-orig-name
				 (z:bound-varref-binding expr)))]
		    [free-vars (if (memq? v bound-vars)
				   (list v)
				   null)]
		    [debug-info (make-debug-info free-vars on-spine expr)]
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
	
	[(z:case-lambda-form? expr)
	 (let* ([annotate-case
		 (lambda (arglist body)
		   (let ([var-list (z:arglist-vars arglist)])
		     (let-values ([(annotated free-vars)
				   (annotate body (set-union var-list bound-vars) #t)])
		       (let ([new-free-vars (remq* var-list free-vars)]
			     [new-annotated (list (arglist->ilist arglist) annotated)])
			 (list new-annotated new-free-vars)))))]
		[pile-of-results (map annotate-case 
				      (z:case-lambda-form-args expr)
				      (z:case-lambda-form-bodies expr))]
		[annotated-bodies (map car pile-of-results)]
		[annotated-case-lambda (list '#%case-lambda annotated-bodies)] 
		[free-vars (apply set-union (map cadr pile-of-results))]
		[debug-info (make-debug-info free-vars null on-spine expr)]
		[closure-info (make-debug-info free-vars mutated-vars #t expr)]
		[hash-wrapped `(#%let ([,closure-temp ,annotated-case-lambda])
				(closure-table-put! ,(closure-key-maker closure-temp) ,closure-info)
				,closure-temp)])
	   (values (wrap debug-info hash-wrapped)
		   new-free-vars))]
	
	[(z:if-form? expr) 
	 (let+
	  ([val (values annotated-test free-vars-test) 
		(annotate (z:if-form-test expr) bound-vars #f)]
	   [val (values annotated-then free-vars-then) 
		(annotate (z:if-form-then expr) bound-vars on-spine)]
	   [val (values annotated-else free-vars-else) 
		(annotate (z:if-form-else expr) bound-vars on-spine)]
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
	   [val free-vars (set-union free-vars-test free-vars-then free-vars-else)]
	   [val debug-info (make-debug-info free-vars on-spine expr)])
	  (values (wrap debug-info annotated) free-vars))]
	
	[(app? expr)
	 (let+
	  ([val sub-exprs (cons (z:app-fun expr) (z:app-args expr))]
	   [val arg-sym-list (build-list (length sub-exprs) get-arg-symbol)]
	   [val let-clauses (map (lambda (sym) `(,sym (#%quote ,*unevaluated*))) arg-sym-list)]
	   [val pile-of-values
		(map (lambda (expr bound) 
		       (annotate expr bound #f))
		     sub-exprs)]
	   [val annotated-sub-exprs (map car pile-of-values)]
	   [val free-vars (apply set-union (map cadr pile-of-values))]
	   [val set!-list (map (lambda (arg-symbol annotated-sub-expr)
				 `(#%set! ,arg-symbol ,annotated-sub-expr))
			       arg-sym-list annotated-sub-exprs)]
	   [val app-debug-info (make-debug-info arg-sym-list on-spine expr)]
	   [val final-app (wrap app-debug-info arg-sym-list)]
	   [val debug-info (make-debug-info (set-union arg-sym-list free-vars) on-spine expr)]
	   [val let-body (wrap debug-info `(#%begin ,@set!-list ,final-app))])
	  (values `(let ,let-clauses ,let-body) free-vars))]

	; the variable form 

	[(z:bound-varref? expr)
	 (translate-varref 
	  expr
	  (not (never-undefined? (z:bound-varref-binding expr)))
	  on-spine)]
	
	[(z:top-level-varref? expr)
	 (if (is-unit-bound? expr)
	     (translate-varref expr #t on-spine)
	     (begin
	       (check-for-keyword/proc expr)
	       (translate-varref expr #f on-spine)))]

	; I AM RIGHT HERE
	 
	 ; other constants
	 
	 [else
	  (let ([debug-info (make-debug-info null #f sexp)])
	    (values (wrap debug-info sexp) null null))])]))
  
  
  )



