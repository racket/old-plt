#|

Rough BNF

(class/d
 super-expresion
 init-args
 ((public var ...)
  (override var ...)
  (inherit var ...)
  (rename (var var) ...))
 
  definitions-and-expressions ...)

;; only thing wrong with above bnf is that the public, etc. clauses
;; can appear multiple times in that same section. 

|#

(module classd mzscheme

  (require "class.ss")
  (require-for-syntax (lib "kerncase.ss" "syntax"))

  (define-syntax class/d*/names
    (lambda (stx)
      (syntax-case stx ()
	[(_  (this-id super-id) super-expression (interface-expr ...) init-args
	     (spec ...)
	     defn-or-expr
	     ...)
	 (let ([specs (syntax->list (syntax (spec ...)))]
	       [defn-and-exprs (syntax->list (syntax (defn-or-expr ...)))])
	   (unless (identifier? (syntax this-id))
	     (raise-syntax-error
	      'class/d*/names
	      "not an identifier for `this'"
	      stx
	      (syntax this-id)))
	   (unless (identifier? (syntax super-id))
	     (raise-syntax-error
	      'class/d*/names
	      "not an identifier for `super-init'"
	      stx
	      (syntax super-id)))
	   
	   (let ([init-names (let loop ([args (syntax init-args)])
			       (syntax-case args ()
				 [() null]
				 [id (identifier? args) (list args)]
				 [(id . rest)
				  (identifier? (syntax id)) 
				  (cons (syntax id) (loop (syntax rest)))]
				 [([id def] . rest)
				  (identifier? (syntax id)) 
				  (cons (syntax id) (loop (syntax rest)))]
				 [_else 
				  (raise-syntax-error
				   'class/d
				   "bad initialization arguments"
				   stx
				   (syntax init-args))]))])
	     ;; Syntax check on specs:
	     (for-each (lambda (spec)
			 (syntax-case spec (public override inherit rename)
			   [(public var ...)
			    (andmap identifier? (syntax->list (syntax (var ...))))
			    'ok]
			   [(override var ...)
			    (andmap identifier? (syntax->list (syntax (var ...))))
			    'ok]
			   [(inherit var ...)
			    (andmap identifier? (syntax->list (syntax (var ...))))
			    'ok]
			   [(rename (var rvar) ...)
			    (andmap identifier? (syntax->list (syntax (var ... rvar ...))))
			    'ok]
			   [_else (raise-syntax-error
				   'class/d
				   "bad specification" 
				   stx
				   spec)]))
		       specs)
	     (let ([public-names 
		    (apply
		     append
		     (map (lambda (spec)
			    (syntax-case spec (public)
			      [(public var ...)
			       (syntax->list (syntax (var ...)))]
			      [else null]))
			  specs))]
		   [override-names
		    (apply
		     append
		     (map (lambda (spec)
			    (syntax-case spec (override)
			      [(override var ...)
			       (syntax->list (syntax (var ...)))]
			      [else null]))
			  specs))]
		   [inherit-names
		    (apply
		     append
		     (map (lambda (spec)
			    (syntax-case spec (inherit)
			      [(inherit var ...)
			       (syntax->list (syntax (var ...)))]
			      [else null]))
			  specs))]
		   [rename-pairs
		    (apply
		     append
		     (map (lambda (spec)
			    (syntax-case spec (rename)
			      [(rename pair ...)
			       (syntax->list (syntax (pair ...)))]
			      [else null]))
			  specs))]
		   [rename-names
		    (apply
		     append
		     (map (lambda (spec)
			    (syntax-case spec (rename)
			      [(rename (var rvar) ...)
			       (syntax->list (syntax (var ...)))]
			      [else null]))
			  specs))]
		   [declared-names 
		    (apply
		     append
		     (list (syntax this-id) (syntax super-id))
		     init-names
		     (map (lambda (spec)
			    (syntax-case spec (public override inherit rename)
			      [(rename (var rvar) ...)
			       (syntax->list (syntax (var ...)))]
			      [(_ var ...)
			       (syntax->list (syntax (var ...)))]))
			  specs))])
	       ;; Expand definitions:
	       (let ([defn-and-exprs (map
				      (lambda (defn-or-expr)
					(local-expand
					 defn-or-expr
					 (append
					  (kernel-form-identifier-list (quote-syntax here))
					  declared-names)))
				      defn-and-exprs)])
		 ;; Check defined variables:
		 (let ([defvars (apply
				 append
				 (map (lambda (defn-or-expr)
					(syntax-case defn-or-expr (define-values)
					  [(define-values (id ...) expr)
					   (syntax->list (syntax (id ...)))]
					  [(define-values . _)
					   (raise-syntax-error
					    'class/d
					    "bad definition"
					    stx
					    defn-or-expr)]
					  [else null]))
				      defn-and-exprs))])
		   ;; Duplicate defs?
		   (let ([dup (check-duplicate-identifier 
			       (append (list (syntax this-id)
					     (syntax super-id))
				       init-names inherit-names rename-names 
				       defvars))])
		     (when dup
		       (raise-syntax-error
			'class/d
			"duplicate definition for identifier"
			stx
			dup)))
		   ;; All declared defined?
		   (for-each (lambda (name)
			       (unless (ormap (lambda (x) (bound-identifier=? x name))
					      defvars)
				 (raise-syntax-error
				  'class/d
				  "name declared but not defined"
				  stx
				  name)))
			     (append public-names override-names))
		   
		   (with-syntax ([clauses
				  (apply
				   append
				   (map
				    syntax->list
				    (map (lambda (defn-or-expr)
					   (syntax-case defn-or-expr (define-values)
					     [(define-values (id ...) expr)
					      (with-syntax ([(tmp-id ...) (generate-temporaries (syntax (id ...)))]
							    [(decl-kind ...)
							     (map (lambda (id)
								    (cond
								     [(ormap (lambda (x) (bound-identifier=? x id))
									     public-names)
								      (quote-syntax public)]
								     [(ormap (lambda (x) (bound-identifier=? x id))
									     override-names)
								      (quote-syntax override)]
								     [else
								      (quote-syntax private)]))
								  (syntax->list (syntax (id ...))))])
						(syntax
						 ((private tmp-id ...)
						  (sequence
						    (set!-values (tmp-id ...) expr))
						  (decl-kind [id tmp-id])
						  ...)))]
					     [else
					      (with-syntax ([expr defn-or-expr])
						(syntax ((sequence expr))))]))
					 defn-and-exprs)))]
				 [inherit-names inherit-names]
				 [rename-pairs rename-pairs])
		     (syntax
		      (class*/names (this-id super-id) super-expression (interface-expr ...) 
				    init-args
				    (rename . rename-pairs)
				    (inherit . inherit-names)
				    .
				    clauses))))))))])))

      (define-syntax class/d*
	(lambda (stx)
	  (syntax-case stx ()
	    [(form  super-expresion (interface-expr ...) init-args
		    (spec ...)
		    defn-or-expr
		    ...)
	     (with-syntax ([this-id (datum->syntax-object (syntax form) 'this #f)]
			   [super-id (datum->syntax-object (syntax form) 'super-init #f)])
	       (syntax (class/d*/names (this-id super-id) super-expresion (interface-expr ...) init-args
				       (spec ...)
				       defn-or-expr
				       ...)))])))

      (define-syntax class/d
	(lambda (stx)
	  (syntax-case stx ()
	    [(form super-expresion init-args
		   (spec ...)
		   defn-or-expr
		   ...)
	     (with-syntax ([class/d* (datum->syntax-object (syntax form) 'class/d* #f)])
	       (syntax (class/d* super-expresion () init-args
				 (spec ...)
				 defn-or-expr
				 ...)))])))

      (provide class/d class/d* class/d*/names))

    
