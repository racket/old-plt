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
  
  ; HASH SOURCE LOCATIONS
  
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
  
  (define debug-key (gensym "debug-key-"))
  (define mutator-gensym (gensym "mutator-"))
  
  ; make-debug-info takes a list of variables and an expression and
  ; creates a thunk closed over the expression and (if bindings-needed is true) 
  ; the following information for each variable in kept-vars:
  ; 1) the name of the variable (could actually be inferred)
  ; 2) the value of the variable
  ; 3) a mutator for the variable, if it appears in mutated-vars.
  ; (The reason for the third of these is actually that it can be used
  ;  in the stepper to determine which bindings refer to the same location,
  ;  as per Matthew's suggestion.)
  ; note that the mutators are needed only for the bindings which appear in
  ; closures; no location ambiguity can occur in the 'currently-live' bindings,
  ; since at most one location can exist for any given 
  
  (define (make-debug-info vars mutated-vars bindings-needed start)
    (let* ([kept-vars (if bindings-needed vars null)]
	   [var-clauses (map (lambda (x) 
			       `(cons (#%quote ,x)
				      (cons ,x
					    ,(if (memq x mutated-vars)
						 `(lambda (,mutator-gensym)
						    (set! ,x ,mutator-gensym))
						 `null))))
			     kept-vars)])
      `(#%lambda () (list ,start ,@var-clauses))))
  
  ; wrap creates the w-c-m expression.

  ; NB: I don't know how to protect the 'break' routine.  It can be
  ; redefined as written here, which would cause major havoc. Probably this
  ; is best done with some kind of ... no, no, I'm not sure.
  
  (define (wrap debug-info expr)
    (let ([with-break `(#%begin (break) ,expr)])
      `(#%with-continuation-mark (#%quote ,debug-key) ,debug-info ,with-break)))

  
  )



