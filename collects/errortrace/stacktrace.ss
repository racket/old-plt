(module stacktrace mzscheme
  (require (lib "unitsig.ss")
           (lib "kerncase.ss" "syntax")
           (lib "stx.ss" "syntax"))
  
  (provide stacktrace@ stacktrace^ stacktrace-imports^)
  
  (define-signature stacktrace-imports^ (with-mark profile-point))
  (define-signature stacktrace^ (annotate-top annotate))
  
  (define stacktrace@
    (unit/sig stacktrace^
      (import stacktrace-imports^)
      
      ;; Result doesn't have a `lambda', so it works
      ;; for case-lambda
      (define (annotate-lambda name expr args body trans?)
	(with-syntax ([body
		       (profile-point 
			(map (lambda (e) (annotate e trans?)) (stx->list body))
			name expr
			trans?)]
		      [args args])
            (syntax (args . body))))
      
      (define (keep-method-property orig new)
        (let ([p (syntax-property orig 'method-arity-error)])
          (if p
              (syntax-property new 'method-arity-error p)
              new)))
      
      (define (annotate-let rec? trans? varsl rhsl bodyl)
        (let ([varses (syntax->list varsl)]
              [rhses (syntax->list rhsl)]
              [bodies (syntax->list bodyl)])
	  (with-syntax ([(rhs ...)
			 (map
			  (lambda (vars rhs)
			    (annotate-named
			     (syntax-case vars ()
			       [(id)
				(syntax id)]
			       [_else #f])
			     rhs
			     trans?))
			  varses 
			  rhses)]
			[(body ...)
			 (map
			  (lambda (body)
			    (annotate body trans?))
			  bodies)]
			[(vars ...) varses]
			[let (if rec? 
				 (quote-syntax letrec-values)
				 (quote-syntax let-values))])
	    (syntax (let ([vars rhs] ...)
		      body ...)))))
      
      (define (annotate-seq trans? expr who bodyl annotate)
        (with-syntax ([who who]
                      [bodyl
                       (map (lambda (b)
                              (annotate b trans?))
                            (syntax->list bodyl))])
          (syntax/loc expr (who . bodyl))))
      
      (define (make-annotate top? name)
        (lambda (expr trans?)
          (kernel-syntax-case expr trans?
	    [_
	     (identifier? expr)
	     (if (eq? 'lexical (identifier-binding expr))
		 ;; lexical variable - no error possile
		 expr
		 ;; might be undefined/uninitialized
		 (with-mark expr expr))]
	    
	    [(#%top . _)
	     ;; might be undefined/uninitialized
	     (with-mark expr expr)]
	    [(#%datum . _)
	     ;; no error possible
	     expr]
	    
	    ;; Can't put annotation on the outside
	    [(define-values names rhs)
	     top?
	     (with-syntax ([marked (with-mark expr
					      (annotate-named
					       (syntax-case (syntax names) ()
						 [(id)
						  (syntax id)]
						 [_else #f])
					       (syntax rhs)
					       trans?))])
	       (syntax/loc expr (define-values names marked)))]
	    [(begin . exprs)
	     top?
	     (annotate-seq
	      trans? expr (quote-syntax begin)
	      (syntax exprs)
	      annotate-top)]
	    [(define-syntaxes (name ...) rhs)
	     top?
	     (with-syntax ([marked (with-mark expr
					      (annotate-named
					       (let ([l (syntax->list (syntax (name ...)))])
						 (and (pair? l)
						      (null? (cdr l))
						      (car l)))
					       (syntax rhs)
					       #t))])
	       (syntax/loc expr (define-syntaxes (name ...) marked)))]
	    
	    ;; Just wrap body expressions
	    [(module name init-import (#%plain-module-begin body ...))
	     top?
	     (with-syntax ([bodyl
			    (map (lambda (b)
				   (annotate-top b trans?))
				 (syntax->list (syntax (body ...))))])
	       (datum->syntax-object
		expr
		;; Preserve original #%module-begin:
		(list (syntax module) (syntax name) (syntax init-import) 
		      (cons (syntax #%plain-module-begin) (syntax bodyl)))
		expr))]
	    
	    ;; No way to wrap
	    [(require i ...) expr]
	    [(require-for-syntax i ...) expr]
	    ;; No error possible (and no way to wrap)
	    [(provide i ...) expr]
	    
	    ;; No error possible
	    [(quote _)
	     expr]
	    [(quote-syntax _)
	     expr]
	    
	    ;; Wrap body, also a profile point
	    [(lambda args . body)
	     (with-syntax ([cl (annotate-lambda name expr 
						(syntax args) (syntax body) 
						trans?)])
	       (keep-method-property expr (syntax/loc expr (lambda . cl))))]
	    [(case-lambda [args . body] ...)
	     (with-syntax ([clauses
			    (map
			     (lambda (args body)
			       (annotate-lambda name expr args body trans?))
			     (syntax->list (syntax (args ...))) 
			     (syntax->list (syntax (body ...))))])
	       (keep-method-property expr (syntax/loc expr (case-lambda . clauses))))]
	    
	    ;; Wrap RHSs and body
	    [(let-values ([vars rhs] ...) . body)
	     (with-mark expr 
			(annotate-let #f trans?
				      (syntax (vars ...))
				      (syntax (rhs ...))
				      (syntax body)))]
	    [(letrec-values ([vars rhs] ...) . body)
	     (with-mark expr 
			(annotate-let #t trans?
				      (syntax (vars ...))
				      (syntax (rhs ...))
				      (syntax body)))]
	    
	    ;; Wrap RHS
	    [(set! var rhs)
	     (with-syntax ([rhs (annotate-named 
				 (syntax var)
				 (syntax rhs)
				 trans?)])
	       ;; set! might fail on undefined variable, or too many values:
	       (with-mark expr (syntax/loc expr (set! var rhs))))]
	    
	    ;; Wrap subexpressions only
	    [(begin . body)
	     (with-mark expr
			(annotate-seq trans? expr (syntax begin) (syntax body) annotate))]
	    [(begin0 . body)
	     (with-mark expr
			(annotate-seq trans? expr (syntax begin0) (syntax body) annotate))]
	    [(if . body)
	     (with-mark expr (annotate-seq trans? expr (syntax if) (syntax body) annotate))]
	    [(with-continuation-mark . body)
	     (with-mark expr
			(annotate-seq 
			 trans? expr (syntax with-continuation-mark) (syntax body) annotate))]
	    
	    ;; Wrap whole application, plus subexpressions
	    [(#%app . body)
	     (if (stx-null? (syntax body))
		 ;; It's a null:
		 expr
		 (with-mark expr
			    (annotate-seq trans? expr 
					  (syntax #%app) (syntax body) 
					  annotate)))]
	    
	    [_else
	     (error 'errortrace
		    "unrecognized expression form~a: ~e"
		    (if top? " at top-level" "")
		    (syntax-object->datum expr))])))
      
      (define annotate (make-annotate #f #f))
      (define annotate-top (make-annotate #t #f))
      (define annotate-named (lambda (name expr trans?) ((make-annotate #t name) expr trans?))))))
