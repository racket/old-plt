;; `package' and `open' correspond to Chez's `module' and `import' ---
;; without making `import' a part of the primitive expander mechanism,
;; which would require special handling for anything that uses
;; `local-expand'.
;;
;; The main idea is to hide package definitions by "introducing" the
;; identifier (i.e., applying a fresh mark for each definition).
;;
;; Beyond the main strategy, there are two major problems:
;;
;;  1. Making `package' declarations available to immediately
;;     following `open' declarations in an internal-definition
;;     context: (let () (package p ...) (open p) ...)
;;
;;     The problem is that `open' needs to inspect the package
;;     to decide what variables it binds, but the package
;;     descriptor isn't executed until the defn context has
;;     dertemined the full set of names to be defined.
;;
;;     We work around this problem by keeping our own table
;;     of "recently" processed `package' declarations. The
;;     `syntax-local-context' function lets us key this to
;;     specific internal-definition contexts.
;;
;;  2. Implementing the binding effect of an `open', which needs
;;     to expose the bindings hidden by a `package', but also
;;     needs to override shadowing.
;;
;;     The `syntax-local-get-shadower' MzScheme function provides
;;     the key ingredient for this part, but it doesn't quite work
;;     when `open' appears within `package'. In that case, we
;;     need to first take into account the package's introductions
;;     that hide definitions.

(module package mzscheme
  (require (lib "etc.ss"))
  (require-for-syntax "private/package-helper.ss"
                      (lib "kerncase.ss" "syntax")
                      (lib "stx.ss" "syntax")
		      (lib "boundmap.ss" "syntax")
		      (lib "context.ss" "syntax")
                      (lib "list.ss"))
  
  (provide package package*
	   open define-dot
	   open* define*-dot
	   dot 
	   define*-syntax define*
	   define*-syntaxes define*-values
	   open/derived open*/derived package/derived
	   define-dot/derived define*-dot/derived
	   rename-potential-package rename*-potential-package)

  ;; Used with `fluid-let-syntax' to communicate to `open'
  ;; when an expression is within the body of a `package' declaration.
  ;; This matters for choosing the right shadower of an id.
  (define-syntax current-package #f)

  (define-syntax-set (define*-syntaxes
		       define*-values
		       define*-syntax
		       define*)
    (define (check-formals s)
      (let loop ([s s])
	(cond
	 [(stx-null? s) #t]
	 [(identifier? s) #t]
	 [(and (stx-pair? s)
	       (identifier? (stx-car s)))
	  (loop (stx-cdr s))]
	 [else #f])))

    (define (multi stx def)
      (syntax-case stx ()
	((_ (id ...) body) 
	 (andmap identifier? (syntax->list #'(id ...)))
	 (quasisyntax/loc stx (#,def (id ...) body)))))

    (define (define*-syntaxes/proc stx)
      (multi stx #'define-syntaxes))
    
    (define (define*-values/proc stx)
      (multi stx #'define-values))
  
    (define (single stx def-vals)
      (syntax-case stx ()
	((_ id body) 
	 (identifier? #'id)
	 (quasisyntax/loc stx (#,def-vals (id) body)))
	((_ (id . formals) body1 body ...) 
	 (and (identifier? #'id)
	      (check-formals #'formals))
	 (quasisyntax/loc stx (#,def-vals (id) (lambda formals body1 body ...))))))

    (define (define*-syntax/proc stx)
      (single stx #'define*-syntaxes))
    
    (define (define*/proc stx)
      (single stx #'define*-values)))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; The main `package' implementation (actually, package/derived)
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define-syntax-set (package/derived)

    (define kernel-form-identifier-list+defines
      (append (list #'define*-values #'define*-syntaxes)
	      (kernel-form-identifier-list #'here)))

    (define (remove-begins def)
      (kernel-syntax-case def #f
        ((begin defs ...)
         (apply append (map remove-begins (syntax->list #'(defs ...)))))
        (_ (list def))))
    
    (define (fix-expr e)
      (kernel-syntax-case e #f
        ((define-values x y) e)
        ((define-syntaxes x y) e)
        ((d x y) (or (module-identifier=? (quote-syntax define*-values) #'d)
                     (module-identifier=? (quote-syntax define*-syntaxes) #'d))
         e)
        (x #`(define-values () (begin x (values))))))
    
    ;; Partially expands all body expressions, and wraps expressions
    ;; in empty `define-values'; the result is a list of definitions
    (define (get-defs expand-context defs exports)
      (let ([stop-list (append kernel-form-identifier-list+defines
			       exports)])
	(map fix-expr
	     (apply append
		    (map (letrec ([ex
				   (lambda (d)
				     (let ([e (local-expand 
					       d
					       expand-context
					       stop-list)])
				       (syntax-case e (begin)
					 [(begin e ...)
					  (apply
					   append
					   (map ex (syntax->list #'(e ...))))]
					 [else (list e)])))])
			   ex)
			 defs)))))
    
    ;; Extracts all defined names, and also checks for duplicates
    ;; in the * forms.
    (define (extract-ids defs stx)
      (let loop ([defs defs][normal-defs null][let*-defs null])
	(if (null? defs)
	    (values normal-defs let*-defs)
	    (syntax-case (car defs) ()
	      [(dv (id ...) expr)
	       (and (identifier? #'dv)
		    (or (module-identifier=? #'dv #'define-values)
			(module-identifier=? #'dv #'define-syntaxes))
		    (andmap identifier? (syntax->list #'(id ...))))
	       (loop (cdr defs)
		     (append normal-defs (syntax->list #'(id ...)))
		     let*-defs)]
	      [(dv . _)
	       (and (identifier? #'dv)
		    (or (module-identifier=? #'dv #'define-values)
			(module-identifier=? #'dv #'define-syntaxes)))
	       (raise-syntax-error #f "bad syntax" (car defs))]
	      [(dv (id ...) expr)
	       (and (identifier? #'dv)
		    (or (module-identifier=? #'dv #'define*-values)
			(module-identifier=? #'dv #'define*-syntaxes))
		    (andmap identifier? (syntax->list #'(id ...))))
	       ;; Check that the identifiers in a single set are distinct
	       (let ([ids (syntax->list #'(id ...))])
		 (let ([dup (check-duplicate-identifier ids)])
		   (when dup
		     (raise-syntax-error
		      #f
		      "identifier defined multiple times in a single set"
		      stx
		      dup)))
		 (loop (cdr defs)
		       normal-defs
		       (append let*-defs ids)))]
	      [(dv . _)
	       (and (identifier? #'dv)
		    (or (module-identifier=? #'dv #'define*-values)
			(module-identifier=? #'dv #'define*-syntaxes)))
	       (raise-syntax-error #f "illegal definition form" (car defs))]))))

    ;; Extracts one set of starred names:
    (define (get/let*-ids def)
      (syntax-case def ()
        ((d vars body) (or (module-identifier=? (quote-syntax define*-values) #'d)
                           (module-identifier=? (quote-syntax define*-syntaxes) #'d))
         (syntax->list #'vars))
        (_ null)))
    
    ;; Combines parts of a transformed definition in a package:
    (define (rebuild-def orig package-name kw ids body compile-time?)
      (datum->syntax-object
       orig
       `(,kw ,ids ,(if compile-time?
		       body
		       #`(fluid-let-syntax ([#,(syntax-local-introduce #'current-package)
					     (quote-syntax #,package-name)])
			   #,body)))
       orig
       orig))

    ;; Convert a definition from a package body, and add marks as
    ;; appropriate to map to hidden names within the package. Also
    ;; accumulate new hidden names from starred bindings.
    (define (mark-ids def introducers package-name)
      (let ((new-ids (map (lambda (id) (cons id (make-syntax-introducer)))
                          (get/let*-ids def))))
        (values
         (syntax-case def ()
           ((ds vars body) 
	    (module-identifier=? (quote-syntax define-syntaxes) #'ds)
            (rebuild-def def package-name
			 #'ds 
			 (mark-to-localize #'vars (append new-ids introducers) #'protect) 
			 (mark-to-localize #'body (append new-ids introducers) #'protect)
			 #t))
           ((dv vars body)
	    (module-identifier=? (quote-syntax define-values) #'dv)
            (rebuild-def def package-name
			 #'dv 
			 (mark-to-localize #'vars (append new-ids introducers) #'protect) 
			 (mark-to-localize #'body (append new-ids introducers) #'protect)
			 #f))
           ((d vars body) 
	    (module-identifier=? (quote-syntax define*-values) #'d)
            (rebuild-def def package-name
			 #'d
			 (mark-to-localize #'vars (append new-ids introducers) #'protect)
			 (mark-to-localize #'body introducers #'protect)
			 #t))
           ((d vars body) 
	    (module-identifier=? (quote-syntax define*-syntaxes) #'d)
            (rebuild-def def package-name
			 #'d
			 (mark-to-localize #'vars (append new-ids introducers) #'protect)
			 (mark-to-localize #'body introducers #'protect)
			 #t)))
	 new-ids)))
        
    ;; For top-level definitions, we need to "declare"
    ;; the defined variables before we might use them.
    ;; We declare the variable by compiling a dummy
    ;; define-values expression.
    (define (extract-declarations converted-defs)
      (let loop ([converted-defs converted-defs]
		 [pre-accum null])
	(if (null? converted-defs)
	    (values (reverse pre-accum))
	    (syntax-case (car converted-defs) (define-values)
	      [(define-values (id ...) body)
	       (loop (cdr converted-defs)
		     (list* #'(define-syntaxes (id ...) (values))
			    pre-accum))]
	      [_ (loop (cdr converted-defs)
		       pre-accum)]))))
    
    ;; The main package/derived transformer:
    (define (package/derived/proc derived-stx)
      (syntax-case derived-stx ()
        ((_ orig-stx name provides body ...)
         (let ([stx #'orig-stx])
	   (check-defn-context stx)
           (unless (identifier? #'name)
             (raise-syntax-error #f "structure name must be an identifier" stx #'name))
	   (unless (or (and (identifier? #'provides)
			    (module-identifier=? (quote-syntax all-defined) #'provides))
		       (and (stx-list? #'provides)
			    (andmap identifier? (stx->list #'provides))))
	     (if (eq? 'all-defined (syntax-e #'provides))
		 (raise-syntax-error
		  #f
		  "`all-defined' keyword has a binding, so it is disallowed as an export"
		  stx
		  #'provides)
		 (raise-syntax-error
		  #f
		  "exports must have the form `all-defined' or `(identifier ...)'"
		  stx
		  #'provides)))
	   (let ([specific-exports (if (identifier? #'provides)
				       #f
				       (syntax->list #'provides))])
	     (when specific-exports
	       (let ([dup (check-duplicate-identifier specific-exports)])
		 (when dup
		   (raise-syntax-error
		    #f
		    "identifier exported multiple times"
		    stx
		    dup))))
	     (let*-values ([(expand-context) (build-expand-context (gensym 'package-define))]
			   [(defs) (get-defs expand-context
					     (syntax->list #'(body ...))
					     (or specific-exports
						 null))]
			   ;; normal-ids and let*-ids are in same order as in package:
			   [(normal-ids let*-ids) (extract-ids defs stx)]
			   [(bt) (make-bound-identifier-mapping)])
	       (for-each (lambda (id)
			   (when (bound-identifier-mapping-get bt id (lambda () #f))
			     (raise-syntax-error
			      #f
			      "identifier defined multiple times"
			      stx
			      id))
			   (bound-identifier-mapping-put! bt id #t))
			 normal-ids)
	       (for-each (lambda (id)
			   (when (bound-identifier-mapping-get bt id (lambda () #f))
			     (raise-syntax-error
			      #f
			      "identifier for * definition has a non-* definition"
			      stx
			      id)))
			 let*-ids)
	       (let-values ([(converted-defs defined-ids)
			     (let loop ((defined-ids (map (lambda (id) (cons id (make-syntax-introducer)))
							  normal-ids))
					(defs defs)
					(accum null))
			       (cond
				((null? defs)
				 (values (reverse accum) defined-ids))
				(else
				 (let-values (((marked-def new-defined-ids)
					       (mark-ids (car defs) defined-ids #'name)))
				   (loop (append new-defined-ids defined-ids)
					 (cdr defs)
					 (cons marked-def accum))))))]
			    [(reverse-orig-ids) (reverse (append normal-ids let*-ids))])
		 (let ([export-renames
			(remove-dups
			       (cond
				[(not specific-exports)
				 (map (lambda (id)
					(cons (car id)
					      ((cdr id) (car id))))
				      defined-ids)]
				[else
				 (map (lambda (provide)
					(let ((introducer (stx-assoc provide defined-ids)))
					  (unless introducer
					    (raise-syntax-error
					     #f
					     "exported identifier not defined"
					     stx
					       provide))
					    (cons (car introducer)
						  ((cdr introducer) provide))))
					specific-exports)]))]
		       [all-renames (map (lambda (id)
					   (cons (car id)
						 ((cdr id) (car id))))
					 defined-ids)])
		   (let ([pre-decls
			  (if (eq? 'top-level (syntax-local-context))
			      (extract-declarations converted-defs)
			      null)]
			 [converted-syntax-defs (filter (lambda (def)
							  (or (module-identifier=? (stx-car def) #'define-syntaxes)
							      (module-identifier=? (stx-car def) #'define*-syntaxes)))
							converted-defs)]
			 [converted-value-defs (filter (lambda (def)
							 (or (module-identifier=? (stx-car def) #'define-values)
							     (module-identifier=? (stx-car def) #'define*-values)))
						       converted-defs)])
		     (pre-register-package expand-context #'name export-renames all-renames defined-ids #'protect)
		     #`(begin
			 (define-syntaxes (name)
			   (make-str (list #,@(map (lambda (i)
						     ;; use of `protect' keeps the id from being localized
						     ;; if this package is in another
						     #`(cons (protect #,(car i))
							     (quote-syntax #,(cdr i))))
						   export-renames))
				     (list #,@(map (lambda (i)
						     #`(cons (protect #,(car i))
							     (quote-syntax #,(cdr i))))
						   all-renames))))			 
			 #,@pre-decls
			 #,@converted-syntax-defs
			 #,@converted-value-defs))))))))))
    )

  (define-syntax (package* stx)
    (syntax-case stx ()
      [(package* name exports body ...)
       (with-syntax ([this-pkg (car (generate-temporaries '(this-pkg)))])
	 #`(begin
	     (package/derived #,stx this-pkg exports
			      body ...)
	     (rename*-potential-package name this-pkg)))]))
  
  (define-syntax (package stx)
    (syntax-case stx ()
      [(package* name exports body ...)
       #`(package/derived #,stx name exports
			  body ...)]))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; The main `open' implementation
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define-syntax-set (open/derived open*/derived open open* 
				   define-dot define*-dot define-dot/derived define*-dot/derived
				   rename-potential-package rename*-potential-package)
    (define (do-open stx orig-name
		     path
		     ex-name bind-name
		     def)
      ;; Find the package:
      (let* ([env+rns+subs 
	      ;; Find the initial package. `open' reports an error if it can't find one
	      (let ([env+rns+subs (open (car path) orig-name stx)])
		(walk-path (cdr path) env+rns+subs stx))]
	     ;; If we're in a package's body, get it's rename environment
	     [cp-env+rns+subs (let ([cp (syntax-local-value #'current-package (lambda () #f))])
				(and cp (open cp cp stx)))]
	     [env (and env+rns+subs (let ([e (car env+rns+subs)])
				      (if ex-name
					  (let ([a (stx-assoc 
						    (let ([id (syntax-local-introduce ex-name)])
						      ;; Reverse-map renaming due to being in a package body:
						      (let ([in-pack-bind
							     (and cp-env+rns+subs
								  (ormap (lambda (p)
									   (and (bound-identifier=? (cdr p) id)
										p))
									 (cadr cp-env+rns+subs)))])
							(if in-pack-bind
							    (car in-pack-bind)
							    id)))
						    (car env+rns+subs))])
					    (unless a
					      (raise-syntax-error
					       #f
					       "no such export from package"
					       stx
					       ex-name))
					    (list a))
					  e)))]
	     ;; Find the names that `open' is supposed to bind
	     [shadowers (if bind-name
			    (list bind-name)
			    (map (lambda (x)
				   (let ([in-pack-bind (and cp-env+rns+subs
							    (stx-assoc (car x) (cadr cp-env+rns+subs)))])
				     (if in-pack-bind
					 (syntax-local-get-shadower
					  (cdr in-pack-bind))
					 (syntax-local-get-shadower
					  (car x)))))
				 env))])
	;; Set up the defined-name -> opened-name mapping
	(with-syntax ([((pub . hid) ...)
		       (map (lambda (x shadower)
			      (cons (if bind-name
					shadower ; which is bind-name
					(syntax-local-introduce shadower))
				    (syntax-local-introduce (cdr x))))
			    env shadowers)]
		      [def-stxes def])
	  ;; In case another `open' follows this one in an
	  ;; internal-defn position, register renames for
	  ;; packages that we just made available:
	  (let* ([ctx (syntax-local-context)]
		 [subs (caddr env+rns+subs)])
	    (when (pair? ctx)
	      (for-each (lambda (x shadower)
			  (re-pre-register-package subs ctx 
						   (if bind-name
						       (syntax-local-introduce shadower)
						       shadower)
						   (if subs
						       (car x)
						       (cdr x))))
			env shadowers)))
	  ;; Open produces a syntax binding to map to the opened names:
	  (syntax/loc stx
	    (def-stxes (pub ...)
	      (values (make-rename-transformer (quote-syntax hid)) ...))))))

    (define (generic-open stx def)
      (check-defn-context stx)
      (syntax-case stx ()
	[(_ elem1 elem ...)
	 (do-open stx #f (syntax->list #'(elem1 elem ...))
		  #f #f
		  def)]))

    (define (generic-open/derived stx def)
      (syntax-case stx ()
	[(_ orig-stx name elem ...)
	 (do-open #'orig-stx #'name (syntax->list #'(elem ...))
		  #f #f
		  def)]))

    (define (open/proc stx)
      (generic-open stx #'define-syntaxes))
    (define (open*/proc stx)
      (generic-open stx #'define*-syntaxes))

    (define (open/derived/proc stx)
      (generic-open/derived stx #'define-syntaxes))
    (define (open*/derived/proc stx)
      (generic-open/derived stx #'define*-syntaxes))

    (define (do-define-dot stx def-stxes path bind-name)
      (unless (identifier? bind-name)
	(raise-syntax-error #f "not an identifier" stx bind-name))
      (let-values ([(path last) (split path)])
	(do-open stx #f
		 path
		 last bind-name
		 def-stxes)))

    (define (generic-define-dot stx def-stxes)
      (check-defn-context stx)
      (syntax-case stx ()
	((_ bind-name path1 path2 path3 ...)
	 (do-define-dot stx def-stxes (syntax->list #'(path1 path2 path3 ...)) #'bind-name))))

    (define (generic-define-dot/derived stx def-stxes)
      (check-defn-context stx)
      (syntax-case stx ()
	((_ orig-stx bind-name path1 path2 path3 ...)
	 (do-define-dot #'orig-stx def-stxes (syntax->list #'(path1 path2 path3 ...)) #'bind-name))))

    (define (define-dot/proc stx)
      (generic-define-dot stx #'define-syntaxes))

    (define (define*-dot/proc stx)
      (generic-define-dot stx #'define*-syntaxes))

    (define (define-dot/derived/proc stx)
      (generic-define-dot/derived stx #'define-syntaxes))

    (define (define*-dot/derived/proc stx)
      (generic-define-dot/derived stx #'define*-syntaxes))

    (define (do-rename stx def-stxes)
      (syntax-case stx ()
	[(_ new-name old-name)
	 (begin
	   (unless (identifier? #'new-name)
	     (raise-syntax-error #f "new name must be an identifier" stx #'new-name))
	   (unless (identifier? #'old-name)
	     (raise-syntax-error #f "old name must be an identifier" stx #'old-name))
	   ;; Re-register if in nested int-def context, and if old-name has
	   ;; a package mapping:
	   (let ([ctx (syntax-local-context)])
	     (when (list? ctx)
	       (re-pre-register-package #f (syntax-local-context)
					(syntax-local-introduce #'new-name)
					(syntax-local-introduce #'old-name))))
	   ;; Produce syntax-level renaming:
	   #`(#,def-stxes (new-name) (make-rename-transformer (quote-syntax old-name))))]))

    (define (rename-potential-package/proc stx)
      (do-rename stx #'define-syntaxes))
    (define (rename*-potential-package/proc stx)
      (do-rename stx #'define*-syntaxes)))
  
  (define-syntax (dot stx)
    (syntax-case stx ()
      ((_ path1 path2 path-rest ...)
       (let ([path (syntax->list #'(path1 path2 path-rest ...))])
	 (for-each (lambda (elem)
		     (unless (identifier? elem)
		       (raise-syntax-error
			#f
			"path element must be an identfier"
			stx
			elem)))
		   path)
	 (let*-values ([(path field) (split path)])
	   (quasisyntax/loc
	    stx
	    (let ()
	      (package this-pkg all-defined
		(open/derived #,stx #f #,@path))
	      (let-syntax ([#,field (lambda (stx)
				      (raise-syntax-error
				       #f
				       "no such exported identifier"
				       (quote-syntax #,stx)
				       stx))])
		(open/derived #f #f this-pkg)
		(let ()
		  #,field)))))))))

  )