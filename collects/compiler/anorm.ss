;; A-Normalizer
;; (c) 1996-7 Sebastian Good
;; (c) 1997-8 PLT, Rice University

; This file contains an "a-normalizer" for Zodiac abstract
;  syntax trees for Scheme.
; This linear time algorithm is adapted from "The Essence
;  of Compiling with Continuations"(Flanagan/Sabry/Duba/Felleisen)

; For unknown historical reasons, this phase is implemented as a
;  non-destructive procedure on ASTs.

; An expressions is given a name when
;  1) it is not already the RHS of a let-assignment
;  2) it is not a tail expression
;  3) the value is not known to be ignored
; There's also a special hack for the test part of an
;  `if' expression: it might be preserved as an
;  application inlined in the `if' form.

; After a-normalizations, all let expressions are "linearized": one
;  binding clause for each let-values expression. (Of course, the
;  single clause can bind multiple variables.) This linearization does
;  not apply to letrec expressions.

;;; Annotatitons: ----------------------------------------------
;;    begin0 - lexical-binding for storing 0th expression
;;; ------------------------------------------------------------

(unit/sig
 compiler:anorm^
 (import (compiler:option : compiler:option^)
	 compiler:library^
	 compiler:cstructs^
	 (zodiac : zodiac:system^)
	 compiler:zlayer^
	 compiler:driver^
	 mzlib:function^
	 (mrspidey : compiler:mrspidey^))

(define compiler:a-value?
  (one-of zodiac:quote-form? zodiac:varref?))

(define a-normalize
    (letrec ([linearize-let-values
	      (lambda (ast)
		(let ([vars (zodiac:let-values-form-vars ast)])
		  (cond
		    [(null? (cdr vars)) ast] ; to prevent N^2 behavior
		    [else
		     (let linear ([vars vars]
				  [vals (zodiac:let-values-form-vals ast)])
		       (if (null? vars)
			   (zodiac:let-values-form-body ast)
			   (zodiac:make-let-values-form (zodiac:zodiac-origin ast)
							(zodiac:zodiac-start 
							 ast)
							(zodiac:zodiac-finish 
							 ast)
							(make-empty-box)
							(list (car vars))
							(list (car vals))
							(linear (cdr vars) 
								(cdr vals)))))])))]
	     [normalize-name
	      (lambda (ast k)
		(normalize-name/special-a-values ast k (lambda (x) #f)))]
	     [normalize-name/special-a-values
	      ; The magic goodie that names expressions.  If the expression
	      ; handed in is not an immediate a-value, it is named and the
	      ; computation continues; syntax correlation exists!
	      (lambda (ast k special-a-value?)
		(a-normalize
		 ast
		 (lambda (exp)
		   (if (or (compiler:a-value? exp) (special-a-value? exp))
		       (k exp)
		       (let* ([tname (gensym)]
			      [tbound (zodiac:make-lexical-binding
				       (zodiac:zodiac-origin exp)
				       (zodiac:zodiac-start exp)
				       (zodiac:zodiac-finish exp)
				       (make-empty-box)
				       tname
				       tname)]
			      [varref (zodiac:binding->lexical-varref tbound)])
			 (mrspidey:copy-annotations! tbound exp)
			 (mrspidey:copy-annotations! varref exp)
			 ; hack: #f annotation => not mutable, or anything else
			 ; (The hack is resolved by the prephase:is-mutable?, etc.
			 ; procedures.)
			 (set-annotation! tbound #f) 
			 (let ([body (k varref)])
			   (mrspidey:copy-annotations!
			    (zodiac:make-let-values-form
			     (zodiac:zodiac-origin exp)
			     (zodiac:zodiac-start exp)
			     (zodiac:zodiac-finish exp)
			     (make-empty-box)
			     (list (list tbound))
			     (list exp)
			     body)
			    body)))))))]
	     ; This names a list of expressions (eg argument list)
	     [normalize-name*
	      (lambda (ast* k)
		(if (null? ast*)
		    (k null)
		    (normalize-name
		     (car ast*)
		     (lambda (term)
		       (normalize-name* (cdr ast*)
					(lambda (term*)
					  (k (cons term term*))))))))]

	     [a-normalize-init-args
	      (lambda (ast)
		(let* ([arglist (zodiac:class*/names-form-init-vars ast)]
		       [args (zodiac:paroptarglist-vars arglist)])
		  ((cond
		    [(zodiac:sym-paroptarglist? arglist) zodiac:make-sym-paroptarglist]
		    [(zodiac:list-paroptarglist? arglist) zodiac:make-list-paroptarglist]
		    [else zodiac:make-ilist-paroptarglist])
		   (map (lambda (a)
			  (if (pair? a)
			      (cons (car a) (a-normalize (cdr a) identity))
			      a))
			args))))]
	     	     

	     [a-normalize
	      (lambda (ast k)
		(when (compiler:option:debug)
		  (zodiac:print-start! debug:port ast)
		  (newline debug:port))
		(cond 
		  
		  ;;----------------------------------------------------------------
		  ;; LAMBDA EXPRESSIONS
		  ;;    We must make a recursive call to normalize the body.
		  ;;    Otherwise, we just pass them on.  Lambda must be queried
		  ;;    before a-value, since lambda might be an a-value
		  ;;
		  ;; (norm (lambda x M)) -> (lambda x (norm M))
		  ;;
		  [(zodiac:case-lambda-form? ast)
		   (k (zodiac:make-case-lambda-form
		       (zodiac:zodiac-origin ast)
		       (zodiac:zodiac-start ast)
		       (zodiac:zodiac-finish ast)
		       (zodiac:parsed-back ast)
		       (zodiac:case-lambda-form-args ast)
		       (map (lambda (body)
			      (a-normalize body identity))
			    (zodiac:case-lambda-form-bodies ast))))]
		  
		  ;;--------------------------------------------------------------
		  ;; A-VALUES
		  ;;    a-values are passed along unharmed.  We have to handle
		  ;;    lambda separately above, but otherwise
		  ;;
		  ;; (norm a-value) -> a-value
		  ;;
		  [(compiler:a-value? ast) (k ast)]
		  
		  ;;--------------------------------------------------------------
		  ;; LET EXPRESSIONS
		  ;;    with let, we must normalize the bound expressions
		  ;;    as well as the body.  We only bind one variable per
		  ;;    let in Core Scheme, so we have to expand these out
		  ;;    Zodiac already tells us if something is unbound, so we
		  ;;    can linearize this let as we like.
		  ;;
		  ;;    we treat letrec separately to reduce the cost of 
		  ;;    optimization
		  ;;    later.  We don't have to look for special cases of set!
		  ;;    we do not guarantee a-values in the vals slot of the letrec
		  ;;    since we do each of those in its own context, otherwise we
		  ;;    can get bindings messed up.
		  ;;
		  ;; (norm (let x M B) k) -> 
		  ;;       (norm M (lambda V (let x V (norm B k))))
		  ;; (norm (letrec [x M] ... B)) -> 
		  ;;     (letrec [x (norm M)] ... (norm B))
		  ;;
		  [(zodiac:let-values-form? ast)
		   (if (null? (zodiac:let-values-form-vars ast))
		       (a-normalize (zodiac:let-values-form-body ast) k)
		       (let ([linear (linearize-let-values ast)])
			 (a-normalize
			  (car (zodiac:let-values-form-vals ast))
			  (lambda (V)
			    (zodiac:make-let-values-form 
			     (zodiac:zodiac-origin linear)
			     (zodiac:zodiac-start linear)
			     (zodiac:zodiac-finish linear)
			     (zodiac:parsed-back linear)
			     (zodiac:let-values-form-vars 
			      linear)
			     (list V)
			     (a-normalize 
			      (zodiac:let-values-form-body 
			       linear)
			      k))))))]
		  
		  [(zodiac:letrec*-values-form? ast)
		   (let ([vals (map (lambda (val) (a-normalize val identity))
				    (zodiac:letrec*-values-form-vals ast))])
		     (zodiac:make-letrec*-values-form
		      (zodiac:zodiac-origin ast)
		      (zodiac:zodiac-start ast)
		      (zodiac:zodiac-finish ast)
		      (zodiac:parsed-back ast)
		      (zodiac:letrec*-values-form-vars ast)
		      vals
		      (a-normalize (zodiac:letrec*-values-form-body ast) k)))]
		  
		  ;;---------------------------------------------------------------
		  ;; IF EXPRESSIONS
		  ;;
		  ;; We do not make a recursive call for the test since it is in the
		  ;; current 'context'.  We want only a-values in the test slot,
		  ;; or an application of a primitive function to a-values. 
		  ;;
		  ;; We specially allow primitive applications
		  ;; of a-values so the optimizer can recognize tests that cen be
		  ;; implemented primitively, e.g., (#%zero? x)
		  ;;
		  ;; (norm (if A B C) k) ->
		  ;;   (name A (lambda test (k (if test (norm B) (norm C)))))
		  ;;
		  [(zodiac:if-form? ast)
		   (normalize-name/special-a-values
		    (zodiac:if-form-test ast)
		    (lambda (test)
		      (k (zodiac:make-if-form (zodiac:zodiac-origin ast)
					      (zodiac:zodiac-start ast)
					      (zodiac:zodiac-finish ast)
					      (zodiac:parsed-back ast)
					      test    
					      (a-normalize (zodiac:if-form-then ast)
							   identity)
					      (a-normalize (zodiac:if-form-else ast)
							   identity))))
		    (lambda (x)
		      (and (zodiac:app? x)
			   (let ([fun (zodiac:app-fun x)])
			     (and (zodiac:top-level-varref? fun)
				  (varref:has-attribute? fun varref:primitive))))))]
		  
		  ;;----------------------------------------------------------------
		  ;; BEGIN EXPRESSIONS
		  ;;
		  ;;    Begins pass through as begins, but every body is 
		  ;;    a-normalized.
		  ;;    We are guaranteed no empty begins
		  ;;
		  ;; (norm (begin A B) k) ->
		  ;;    (norm A (lambda first (begin first (norm B k))))
		  ;;
		  [(zodiac:begin-form? ast)   
		   (k (zodiac:make-begin-form
		       (zodiac:zodiac-origin ast)
		       (zodiac:zodiac-start ast)
		       (zodiac:zodiac-finish ast)
		       (zodiac:parsed-back ast)
		       (map (lambda (b) (a-normalize b identity)) 
			    (zodiac:begin-form-bodies ast))))]
		  
		  ;;----------------------------------------------------------------
		  ;; BEGIN0 EXPRESSIONS
		  ;;
		  ;;    The first is named in a special way, and the rest passes through
		  ;;
		  ;; (norm (begin0 A B) k) ->
		  ;;    (norm A (lambda first (begin0 first (norm B k))))
		  ;;
		  [(zodiac:begin0-form? ast)
		   (let* ([tname (gensym)]
			  [tbound (zodiac:make-lexical-binding
				   (zodiac:zodiac-origin ast)
				   (zodiac:zodiac-start ast)
				   (zodiac:zodiac-finish ast)
				   (make-empty-box)
				   tname
				   tname)]
			  [begin0-exp
			   (zodiac:make-begin0-form
			    (zodiac:zodiac-origin ast)
			    (zodiac:zodiac-start ast)
			    (zodiac:zodiac-finish ast)
			    (zodiac:parsed-back ast)
			    (list
			     (a-normalize (zodiac:begin0-form-first ast) identity)
			     (a-normalize (zodiac:begin0-form-rest ast) identity)))])
		     (set-annotation! begin0-exp tbound)
		     (k begin0-exp))]

		  ;;---------------------------------------------------------------
		  ;; SET! EXPRESSIONS / DEFINE EXPRESSIONS
		  ;;    
		  ;; (norm (set! x M)) -> (name M (lambda val (set! x M)))
		  ;; (norm (define x M))->(define x (norm M identity))
		  ;;
		  [(zodiac:set!-form? ast)
		   (normalize-name 
		    (zodiac:set!-form-val ast)
		    (lambda (norm-val)
		      (k (zodiac:make-set!-form
			  (zodiac:zodiac-origin ast)
			  (zodiac:zodiac-start ast)
			  (zodiac:zodiac-finish ast)
			  (zodiac:parsed-back ast)
			  (zodiac:set!-form-var ast)
			  norm-val))))]
		  
		  [(zodiac:define-values-form? ast)
		   (k (zodiac:make-define-values-form
		       (zodiac:zodiac-origin ast)
		       (zodiac:zodiac-start ast)
		       (zodiac:zodiac-finish ast)
		       (zodiac:parsed-back ast)
		       (zodiac:define-values-form-vars ast)
		       (a-normalize (zodiac:define-values-form-val ast) identity)))]
		  
		  ;;---------------------------------------------------------------
		  ;; APPLICATIONS
		  ;;    We will always apply the a-normalization to the function
		  ;;    position of arguments
		  ;;    first normalize the function, then the list of arguments
		  ;;
		  ;; (norm (M A ...) k) ->
		  ;;  (name M 
		  ;;       (lambda fun (name* A .. (lambda term .. (fun term ..)))))
		  [(zodiac:app? ast)
		   (normalize-name 
		    (zodiac:app-fun ast)
		    (lambda (norm-fun)
		      (normalize-name*
		       (zodiac:app-args ast)
		       (lambda (norm-terms)
			 (k (zodiac:make-app (zodiac:zodiac-origin ast)
					     (zodiac:zodiac-start ast)
					     (zodiac:zodiac-finish ast)
					     (zodiac:parsed-back ast)
					     norm-fun
					     norm-terms))))))]
		  
		  ;;----------------------------------------------------------------
		  ;; STRUCT
		  ;;   a-normalize the super position if it exists
		  ;; 
		  ;; (norm (struct (x M) ...)) ->
		  ;;   (name M (lambda A (struct (x A) ...)))
		  ;; (norm (struct x ...) -> (struct x ...)
		  ;;
		  [(zodiac:struct-form? ast)
		   (let ([super (zodiac:struct-form-super ast)])
		     (if super
			 (normalize-name 
			  super
			  (lambda (norm-super)
			    (k (zodiac:make-struct-form (zodiac:zodiac-origin ast)
							(zodiac:zodiac-start ast)
							(zodiac:zodiac-finish ast)
							(zodiac:parsed-back ast)
							(zodiac:struct-form-type ast)
							norm-super
							(zodiac:struct-form-fields ast)))))
			 (k ast)))]
		  
		  ;;---------------------------------------------------------------
		  ;; UNITS
		  ;;
		  ;; a-normalize each clause in the body, otherwise nothing
		  ;;
		  [(zodiac:unit-form? ast)
		   (k
		    (zodiac:make-unit-form
		     (zodiac:zodiac-origin ast)
		     (zodiac:zodiac-start ast)
		     (zodiac:zodiac-finish ast)
		     (zodiac:parsed-back ast)
		     (zodiac:unit-form-imports ast)
		     (zodiac:unit-form-exports ast)
		     (map (lambda (M) (a-normalize M identity))
			  (zodiac:unit-form-clauses ast))))]

		  ;;-------------------------------------------------------------------
		  ;; COMPOUND UNIT
		  ;;
		  ;; nothing much to do except analyze the constituent exprs
		  ;;
		  [(zodiac:compound-unit-form? ast)
		   (let loop ([l (zodiac:compound-unit-form-links ast)]
			      [acc null])
		     (if (null? l)
			 (k
			  (zodiac:make-compound-unit-form
			   (zodiac:zodiac-origin ast)
			   (zodiac:zodiac-start ast)
			   (zodiac:zodiac-finish ast)
			   (zodiac:parsed-back ast)
			   (zodiac:compound-unit-form-imports ast)
			   (reverse acc)
			   (zodiac:compound-unit-form-exports ast)))
			 (let ([link (car l)])
			   (normalize-name
			    (cadr link)
			    (lambda (norm-expr)
			      (loop (cdr l)
				    (cons (list* (car link)
						 norm-expr
						 (cddr link))
					  acc)))))))]

		  ;;-----------------------------------------------------------
		  ;; INVOKE
		  ;;
		  [(zodiac:invoke-form? ast)
		   (normalize-name
		    (zodiac:invoke-form-unit ast)
		    (lambda (unit)
		      (k (if (zodiac:invoke-unit-form? ast)
			     (zodiac:make-invoke-unit-form
			      (zodiac:zodiac-origin ast)
			      (zodiac:zodiac-start ast)
			      (zodiac:zodiac-finish ast)
			      (zodiac:parsed-back ast)
			      unit
			      (zodiac:invoke-unit-form-variables ast))
			     (zodiac:make-invoke-open-unit-form
			      (zodiac:zodiac-origin ast)
			      (zodiac:zodiac-start ast)
			      (zodiac:zodiac-finish ast)
			      (zodiac:parsed-back ast)
			      unit
			      (zodiac:invoke-open-unit-form-name-specifier ast)
			      (zodiac:invoke-open-unit-form-variables ast))))))]

		  ;;-----------------------------------------------------------
		  ;; CLASS
		  ;;
		  [(zodiac:class*/names-form? ast)
		   (normalize-name
		    (zodiac:class*/names-form-super-expr ast)
		    (lambda (super-expr)
		      (normalize-name*
		       (zodiac:class*/names-form-interfaces ast)
		       (lambda (interfaces)
			 (k (zodiac:make-class*/names-form
			     (zodiac:zodiac-origin ast)
			     (zodiac:zodiac-start ast)
			     (zodiac:zodiac-finish ast)
			     (zodiac:parsed-back ast)
			     (zodiac:class*/names-form-this ast)
			     (zodiac:class*/names-form-super-init ast)
			     super-expr
			     interfaces
			     (a-normalize-init-args ast)
			     (list
			      (zodiac:make-sequence-clause
			       (list
				(a-normalize
				 (car (zodiac:sequence-clause-exprs
				       (car (zodiac:class*/names-form-inst-clauses ast))))
				 identity))))))))))]

		  ;;-----------------------------------------------------------
		  ;; INTERFACE
		  ;;
		  [(zodiac:interface-form? ast)
		   (normalize-name*
		    (zodiac:interface-form-super-exprs ast)
		    (lambda (interfaces)
		      (k 
		       (zodiac:make-interface-form
			(zodiac:zodiac-origin ast)
			(zodiac:zodiac-start ast)
			(zodiac:zodiac-finish ast)
			(zodiac:parsed-back ast)
			interfaces
			(zodiac:interface-form-variables ast)))))]

		  [else (error 'a-normalize "unsupported ~a" ast)]))])
      a-normalize))
      
)
