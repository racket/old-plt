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
 
  ; tri-map is of type: (('a -> ('b x 'c x 'd)) x a' list) -> ('b list x 'c list x 'd list)
  
  (define (tri-map fun . lists)
    (if (null? alist)
	(values null null null)
	(let-values ([(a b c) (apply fun (map car lists))]
		     [(a-rest b-rest c-rest) (tri-map fun (cdr alist))])
	  (values ((cons a a-rest)
		   (cons b b-rest)
		   (cons c c-rest))))))
  
  ; tri-map test:
  
  (andmap (lambda (x) (apply equal? x))
	  `((,(tri-map (lambda (x) (values 1 2 3)) '(a b c))
	     ,(values (list 1 1 1) (list 2 2 2) (list 3 3 3)))
	    (,(tri-map (lambda (x y) (values x y (+ x y))) '(2 3 4) '(8 98 1))
	     ,(values (list 2 3 4) (list 8 98 1) (list 10 101 5)))))
  
  ; closure-key takes a closure and returns an unchanging value that we can hash on.
  ; In our case, (conservative GC), the value of the closure itself serves this purpose
  ; admirably, and can also be used with a weak-key hash table to protect the
  ; tail-recursive properties of the language.
  
  (define closure-key (lambda (x) x))
  
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
  ; indicating whether this expression lies on the evaluation spine.  It returns three things;
  ; an annotated expression, a list of the bound variables which occur free, and a list of the 
  ; bound variables which occur free and may be mutated.
  
  (define (annotate expr bound-vars on-spine)
    (cond
      
      [(z:case-lambda-form? expr)
       (let* ([annotate-case
	       (lambda (arglist body)
		 (let ([var-list (z:arglist-vars arglist)])
		   (let-values ([(annotated free-vars mutated-vars)
				 (annotate body (set-union var-list bound-vars) #t)])
		     (let ([new-free-vars (remq* var-list free-vars)]
			   [new-mutated-vars (remq* var-list mutated-vars)]
			   [new-annotated (list (arglist->ilist arglist) annotated)])
		       (list new-annotated new-free-vars new-mutated-vars)))))]
	      [pile-of-results (map annotate-case 
				    (z:case-lambda-form-args expr)
				    (z:case-lambda-form-bodies expr))]
	      [annotated-bodies (map car pile-of-results)]
	      [annotated-case-lambda (list 'case-lambda annotated-bodies)] 
	      [free-vars (foldl set-union null (map cadr pile-of-results))]
	      [mutated-vars (foldl set-union null (map caddr pile-of-results))]
	      [debug-info (make-debug-info free-vars null on-spine expr)]
	      [closure-info (make-debug-info free-vars mutated-vars #t expr)]
	      [hash-wrapped `(let ([,closure-temp ,annotated-case-lambda])
			       (closure-table-put! ,closure-temp ,closure-info)
			       ,closure-temp)])
	 (values (wrap debug-info hash-wrapped)
		 new-free-vars
		 new-mutated-vars))]
			 
	   
      [`(#%if ,test ,then ,else)
       (let+
	([val (values annotated-test free-vars-test mutated-vars-test) (annotate test bound-vars #f)]
	 [val (values annotated-then free-vars-then mutated-vars-then) (annotate then bound-vars on-spine)]
	 [val (values annotated-else free-vars-else mutated-vars-else) (annotate else bound-vars on-spine)]
	 [val annotated `(#%if ,annotated-test ,annotated-then ,annotated-else)]
	 [val free-vars (foldl set-union free-vars-test (list free-vars-then free-vars-else))]
	 [val mutated-vars (foldl set-union mutated-vars-test (list mutated-vars-then mutated-vars-else))]
	 [val debug-info (make-debug-info free-vars on-spine sexp)])
	(values (wrap debug-info annotated)
		free-vars))]
      [other
       (cond
	 
	 ; the application form: there are a lot of questions here about just how optimized
	 ; I want to make this on the first go-round.  To simplify the first implementation,
	 ; I'm just going to annotate all sub-expressions with source positions, despite 
	 ; the potential elision of such marks.
	 
	 [(list? other) 
	  (let+
	   ([val arg-sym-list (build-list (length sexp) get-arg-symbol)]
	    [val let-clauses (map (lambda (sym) `(,sym (#%quote ,*unevaluated*))) arg-sym-list)]
	    [val multi-annotate
		 (letrec
		     ; NB: this would be shorter if fold could handle multiple values. oh well.
		     
		     ([multi-annotate
		       (lambda (expr-list)
			 (if (null? expr-list)
			     (values null null null)
			     (let-values (((annotated free-vars mutated-vars) 
					   (annotate (car expr-list) bound-vars #f))
					  ((ann-list fv-total mv-total) (multi-annotate (cdr expr-list))))
			       (values (cons annotated ann-list) 
				       (set-union free-vars fv-total)
				       (set-union mutated-vars mv-total)))))])
		   multi-annotate)]
	    [val (values ann-exprs free-vars mutated-vars) (multi-annotate sexp)]
	    [val set!-list (map (lambda (arg-symbol ann-expr)
				  `(#%set! ,arg-symbol ,ann-expr))
				arg-sym-list ann-exprs)]
	    [val app-debug-info (make-debug-info arg-sym-list null on-spine sexp)]
	    [val final-app (wrap app-debug-info arg-sym-list)]
	    [val debug-info (make-debug-info (set-union arg-sym-list free-vars) null on-spine sexp)]
	    [val let-body (wrap debug-info `(#%begin ,@set!-list ,final-app))])
	   (values `(let ,let-clauses ,let-body) free-vars mutated-vars))]
	 
	 ; the variable form 
	 
	 [(symbol? other)
	  (let+
	   ([val free-vars (if (memq other bound-vars)
			       (list other)
			       null)]
	    [val debug-info (make-debug-info free-vars null on-spine other)])
	   (values (wrap debug-info other)
		   free-vars
		   null))]
	 
	 ; other constants
	 
	 [else
	  (let ([debug-info (make-debug-info null #f sexp)])
	    (values (wrap debug-info sexp) null null))])]))
  
  
  )



